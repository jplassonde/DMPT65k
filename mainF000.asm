;--------------------------------------------------------------------------
; Name: mainF000.acme
;
; Desc: Main program of the DM+T 65000
;	Fetch and convert values from analog and I2C sensors, display
;	them on a character LCD screen and send them over UART on request. 
;	- Also do bleeps and blops -
;
; Todo:	- Remove test code artifacts
;	- ZP needs to be cleaned
;	- Stacks have feelings too! It feels left out.
;	  Is "but Zero Page is faster" really a good excuse?
;	- Use a coherent and consistant calling convention
;	- Use loops instead of copy/paste in non time-critical sections
;---------------------------------------------------------------------------

*=$f000
		!to "mainF000.bin", plain
		!cpu 65c02

;------ Music settings ----------------------------------

;; Currently set for Crest Rules by Chris Ammermuller
sid_init 	= $1CB0
SID_LOAD_M   	= $10
SID_LOAD_L	= $00
SID_LENGTH   	= $0D		; length in pages

;------ Zero page statics -----------------------------------
; Muhahah KERNAL, they are all mine now.
; TODO: a bit of cleaning would not hurt though. 

;; Leave $0-$10 & $F0-$FF clean, as they are 
;; statistically the most used for SID.

isr_counter	= $1F   ; counter used in ISR, counting up at 50Hz to set the hz_flag every sec
hz_flag	 	= $20   ; 10hz flag

string_lsb 	= $22 	; used to store string adress for indirect indexed
string_msb	= $23

spi_rx 		= $26  	; storage for the ADC data in. set to 8 bits for now

uart_int_f 	= $27
uart_data	= $28

i2c_buffer	= $29

;; Mult-div 
result1 	= $40
result2		= $41
result3		= $42
result4		= $43
num1_msb	= $44
num1_lsb	= $45
num2_msb	= $46
num2_lsb	= $47
remainder_lsb	= $48
remainder_msb   = $49

;; ITOA 
itoa_lsb 	= $50
itoa_msb 	= $51
temp_msb 	= $52
temp_lsb 	= $53
itoa_str 	= $54 ; take space up to $5A

;; Sensor data
humidity_msb	= $60
humidity_lsb	= $61
air_temp_msb	= $62
air_temp_lsb	= $63

humidity	= $64
air_temp	= $65

ph_quotient	= $66
ph_remainder	= $67

;; END ZP mess

;---------------------------   HARDWARE/MEM INIT ------------------------------

init		sei 		; disable interrupt
		ldx #$ff
		txs     	; set stack pointer to end of page 1 in ram (stack is growing down)
		
		lda #$00	;; init flags and counters to 0
		sta hz_flag
		sta isr_counter

;------------------------------  MUSIC INIT -----------------------------------
; Copy the routine from the $C000 sector in flash to the RAM load address  
; 			    then run the init routine		 			 
source_msb=	$22
source_lsb=	$23
destination_msb=$24
destination_lsb=$25

		; Use indirect indexed addressing mode to copy music routine from
		; ROM to RAM			
		lda #$C0       
 		sta source_msb 		; Set initials addresses for source
		lda #SID_LOAD_M   	; and destination on ZP
		sta destination_msb
		stz source_lsb
		lda #SID_LOAD_L
		sta destination_lsb
		ldy #$0			; Init index 
		ldx #$0			; Init pages counter
write_page	lda (source_lsb),Y	; Load byte from ROM
		sta (destination_lsb),Y	; Copy byte in RAM
		iny			; increase index register
		bne write_page 		; write 256 bytes (until overflow)
		inx			; increase counter 
		cpx #SID_LENGTH		; compare it to music data length (in pages)
		bcs sid_done		; end if done
		inc destination_msb 	; otherwise increase both pointer MSB
		inc source_msb
		jmp write_page		; write next page
					
sid_done	jsr sid_init	; run the music routine init, now in ram.	
	
; ----------------------------- CIA INIT ----------------------------------------
;   Initialize the Timer A and the IO port of the Complex Interface Adapter  

timer_init	
		lda #$20	; Set Timer A to 50Hz
		sta $DC04
		lda #$4e
		sta $DC05	
		lda #$01   	; Set Timer A control register - phi2 input, repeat 
		sta $DC0E		
		lda #$81	; Set interrupt control register - TA int enabled
		sta $DC0D

io_port_init 	
		lda #$1F 	;set port A pin 1-2-3-4-5 as output
		sta $DC02
		lda $DC00	; Adc CS# high, i2c sda/scl high, everything else low
		ora #$1C
		sta $DC00
		cli 		; Enable Interrupt

;----------------- Init LCD, UART and request a conversion from the ADC -------
		jsr lcd_init
		jsr uart_init
		ldx #adc_get_ch1
		jsr spi_send

;------------------------------------------------------------------------------
;------------------------ MAIN PROGRAM LOOP -----------------------------------
;------------------------------------------------------------------------------
	
main_loop	
		; Continuously poll for UART int flag

		lda uart_int_f		; Check for UART interrupt
		beq .check_hz_flag	; Skip the rest if unset
		stz uart_int_f		; Clear flag
		ldx uart_data 		; Otherwise take the byte transmitted (should be 0-2 depending on val requested)
		lda ($64, X) 		; Use indexed indirect to get the right value
		sta U_TRB		; Send it to transmit buffer
		cpx #$2			; Check if value requested is pH (the only 2 bytes value)
		bne .check_hz_flag	; If not, work is over.
		jsr wait_for_uart 	; Otherwise wait for UART - tx ready
		lda ph_remainder  	; And send second byte (remainder)
		sta U_TRB

		; Run the rest of the loop every second

.check_hz_flag	lda hz_flag
		beq main_loop
		stz hz_flag		; Clear flag
		
		jsr get_i2c_vals 	; Get and convert values from HIH9000 sensor
				
		; display values
		lda #$01 ; display clear
		sta LCD0
		jsr lcd_busy_wait		

		jsr cursor_l1		; Print humidity on 1st row
		lda #<humidity_str
		sta string_lsb
		lda #>humidity_str
		sta string_msb
		jsr write_string
		lda humidity
		sta itoa_lsb
		stz itoa_msb
		jsr itoa
		jsr print_itoa_str

		jsr cursor_l2		; Print temperature on 2nd row
		lda #<temperature_str
		sta string_lsb
		lda #>temperature_str
		sta string_msb
		jsr write_string
		lda air_temp
		sta itoa_lsb
		stz itoa_msb
		jsr itoa
		jsr print_itoa_str
		
		ldx #adc_get_ch1	; Get value from ADC (Analog pH sensor)
		jsr spi_send
		lda spi_rx 		; Value for ch1
		sta num1_lsb
		lda #$7
		sta num2_lsb
		stz num1_msb
		stz num2_msb
		jsr multiply		; Convert value
		lda result1
		sta num1_lsb
		lda result2
		sta num1_msb
		lda #$64
		sta num2_lsb
		stz num2_msb
		jsr divide

		lda num1_lsb		; Print value on 3rd line of LCD
		sta itoa_lsb		; Quotient, ".", remainder
		stz itoa_msb
		jsr itoa
		jsr cursor_l3
		lda #<ph_str
		sta string_lsb
		lda #>ph_str
		sta string_msb
		jsr write_string
		jsr print_itoa_str
		lda #$2E
		sta LCD1
		jsr lcd_busy_wait
		lda remainder_lsb
		sta itoa_lsb
		stz itoa_msb
		jsr itoa
		jsr print_itoa_str
			
		jmp main_loop
;------------------------------------------------------------------------------
;----------------------- MAIN PROGRAM LOOP END---------------------------------
;------------------------------------------------------------------------------



;---------------------- I/O, String formatting and misc subroutines -----------------
; Collectively called "The Crysis Kernel" (yes it can run Crysis please stop asking)


;------- LCD ------------------------------------------------------------------------

; The two LCD addresses
LCD0 	 = $D800
LCD1 	 = $D801

;----------------------------------------------------------------------------------
;  FUNCTION NAME: lcd_init
;
;  DESCRIPTION:   perform initialization of the lcd
;----------------------------------------------------------------------------------
;  ARGUMENTS: none
;  RETURNS:   none	  
;----------------------------------------------------------------------------------

lcd_init	lda #$38; 8 bit op, "2-lines" (it's actually 4), 5x8 font
		sta LCD0
		jsr lcd_busy_wait
		
		lda #$0C; display on, cursor and blink off
		sta LCD0
		jsr lcd_busy_wait

		lda #$06; increment address, shift cursor to right on write
		sta LCD0
		jsr lcd_busy_wait

		lda #$01 ; display clear
		sta LCD0
		jsr lcd_busy_wait

		lda #$02 ; set cursor to home
		sta LCD0 
		jsr lcd_busy_wait
		rts

;----------------------------------------------------------------------------------
;  FUNCTIONS NAME: cursor_lx
;
;  DESCRIPTION:    set cursor on a specific row of the LCD
;----------------------------------------------------------------------------------
;  ARGUMENTS: none
;  RETURNS:   none	  
;----------------------------------------------------------------------------------
; todo: are 4 function really necessary for this?
;	make labels for line pos, pass them as an arg in accumulator and 
;	simply use 1 function for this instead

cursor_l1	lda #$80	; first column of 1st row
		sta LCD0
		jsr lcd_busy_wait
		rts

cursor_l2	lda #$C0	; first column of 2nd row
		sta LCD0
		jsr lcd_busy_wait
		rts

cursor_l3	lda #$94	; first column of 3rd row
		sta LCD0
		jsr lcd_busy_wait
		rts

cursor_l4	lda #$D4	; first column of 4th row
		sta LCD0
		jsr lcd_busy_wait
		rts
;----------------------------------------------------------------------------------
;  FUNCTION NAME: clear_line
;
;  DESCRIPTION:   fill a specific line with spaces
;----------------------------------------------------------------------------------
;  ARGUMENTS: none (cursor must be positioned at the beginning of the line)
;  RETURNS:   none	  
;----------------------------------------------------------------------------------
clear_line	ldx #$14
.loop_clear	lda #$20
		sta LCD1
		jsr lcd_busy_wait
		dex
		bne .loop_clear
		rts
;----------------------------------------------------------------------------------
;  FUNCTION NAME: write_string
;
;  DESCRIPTION:   output a string at the cursor position on LCD
;----------------------------------------------------------------------------------
;  ARGUMENTS: LSB on zero page (string_lsb), MSB at string_lsb+1
;  RETURNS:   none	  
;----------------------------------------------------------------------------------
write_string	ldy #$0
.write_str_loop	lda (string_lsb),Y 	; indirect indexed. will take address from string lsb & string lsb+1, add Y... 
		beq .end_write_str	; end on null char
		sta LCD1		
		jsr lcd_busy_wait
		iny
		jmp .write_str_loop
.end_write_str	rts
		
lcd_busy_wait	lda LCD0
		and #$80
		bne lcd_busy_wait
		rts
;----------------------------------------------------------------------------------
;  FUNCTION NAME: print_drdnght
;
;  DESCRIPTION: Print: 	Dreadnought
;		       	Mega+
;			Turbo
;			65000
;	  	   on the 4 rows LCD screen. 
;----------------------------------------------------------------------------------
;  ARGUMENTS: none
;  RETURNS:   none 
;----------------------------------------------------------------------------------
print_drdnght	jsr cursor_l1
		lda #<dreadnought
		sta string_lsb
		lda #>dreadnought
		jsr write_string
	
		jsr cursor_l2
		lda #<mega
		sta string_lsb
		lda #>mega
		jsr write_string
		
		jsr cursor_l3
		lda #<turbo
		sta string_lsb
		lda #>turbo
		jsr write_string

		jsr cursor_l4
		lda #<sixtyfive
		sta string_lsb
		lda #>sixtyfive
		sta string_msb
		jsr write_string
		rts

;; END LCD ROUTINES


;---------------  UART -------------------------------------------------------
U_TRB = $D300	; Transmit and receive buffer
U_DLL = $D300	; Divisor data latch - LSB
U_DLM = $D301	; Divisor data latch - MSB
U_IER = $D301	; Interrupt enable register
U_LCR = $D303	; Line Control Register
U_LSR = $D305	; Line Status Register

;-----------------------------------------------------------------------------
;  FUNCTION NAME: uart_init
;
;  DESCRIPTION: Perform initialization of the ST16C450 UART chip
;-----------------------------------------------------------------------------
;  ARGUMENTS:	none
;  RETURNS:    	nothing		  
;-----------------------------------------------------------------------------

uart_init	lda #$80   ; Divisor latch enable
		sta U_LCR  ; Store in line control register
		lda #$01   ; Set UART to 115.2kbps
		sta U_DLL  ; Store val in divisor latch LSB
		lda #$0	    
		sta U_DLM  ; Store val in divisor latch MSB
		lda #$03   ; Disable divisor latch, set word length to 8
		sta U_LCR  ; Store val in line control register
		lda #$01   ; enable received-data-ready interrupt
		sta U_IER  ; Store val in interrupt enable register
		rts

;-----------------------------------------------------------------------------
;  FUNCTION NAME: wait_for_uart
;
;  DESCRIPTION: Poll the status of UART tx ready
;-----------------------------------------------------------------------------
;  ARGUMENTS:	none
;  RETURNS:    	nothing		  
;-----------------------------------------------------------------------------

wait_for_uart	lda U_LSR ;; poll the tx ready status
		and #$20
		beq wait_for_uart
		rts

;-----------------------------------------------------------------------------
;  FUNCTION NAME: uart_received
;
;  DESCRIPTION: Used for testing. Takes a byte from UART, print on LCD
;		and send it back
;-----------------------------------------------------------------------------
;  ARGUMENTS:	none
;  RETURNS:    	nothing		  
;-----------------------------------------------------------------------------

uart_received	lda U_TRB		; Load byte from transmit buffer
		sta LCD1		; Store it on LCD
		tax			; Preserve value in X
		jsr wait_for_uart	; Check if UART is ready to transmit
		txa			; Restore byte in accumulator
		sta U_TRB		; Send it back
		rts

;; End UART Routines


;--------- SPI ---------------------------------------------------------------

; CIA ports addresses
port_a	 = $DC00 ; 8 bit I/O port A
port_b	 = $DC01 ; 8 bit I/O port B

; Masks for port A / Outputs
S_CLK	 	= $01     
S_MOSI 		= $02
S_CS 	  	= $04

S_NOT_CLK 	= $FE
S_NOT_MOSI 	= $FD
S_NOT_CS 	= $FB

;Mask for port B / Input
S_MISO 		= $01

; LTC1290 commands
adc_get_ch1	= $8C
adc_get_ch2 	= $CC
;-----------------------------------------------------------------------------
;  FUNCTION NAME: spi_send
;
;  DESCRIPTION: Send a byte to SPI,  requesting a reading from the LTC1290 ADC
;		and read the result of last request
;		
;-----------------------------------------------------------------------------
;  ARGUMENTS:	Value to send in X
;  RETURNS:    	Store value received in spi_rx on Zero Page		  
;-----------------------------------------------------------------------------
;	todo: 	LTC1290 doesn't care much about clock speed / shape.
;		Most of the nop'in can be removed and the whole function
;		could be performed in a neat loop instead.
;-----------------------------------------------------------------------------
spi_send	
		lda #$0 	; Clear receive holding byte before starting
		sta spi_rx
		
;set first bit (msb)
		txa			; Transfer value to send in accumulator
		and #$80		; Check if msb is set
		beq .b10		; If not, set MOSI to 0
		lda port_a		; Otherwise load port A in accumulator
		ora #S_MOSI		; Set MOSI pin High
		eor #S_CS		; Set Chip Select low
		sta port_a		; Store value back to CIA port A
		jmp .set_second_bit	; Jump to 2nd bit
; If msb unset
.b10		lda port_a		; Load port A in accumulator
		and #S_NOT_MOSI		; Set MOSI low
		eor #S_CS		; Set Chip Select low
		sta port_a		; Store value back to CIA port A

.set_second_bit nop			; Semi-useless nops
		nop			; They make the clock look neat
		nop			; on a logic analyzer but
		nop			; the ADC probably don't care.
		lda port_a		; Load port A value in accumulator
		ora #S_CLK		; Set SCLK high
		sta port_a		; Store back to CIA port A
		
		lda port_b		; Get data from adc 
		and #S_MISO		; Bitwise-and with MISO (pin 0)
		ora spi_rx		; Bitwise-or with rx "buffer"
		asl 			; Shift left
		sta spi_rx		; Store back on ZP

		nop			; Same as above, for each bits.
		nop
		nop
		nop
		txa
		and #$40
		beq .b20
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .set_third_bit
.b20		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a

.set_third_bit	nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a

		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		asl 
		sta spi_rx

		nop
		nop
		nop
		nop
		txa
		and #$20
		beq .b30
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .set_fourth_bit
.b30		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a

.set_fourth_bit nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a
		
		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		asl 
		sta spi_rx

		nop
		nop
		nop
		nop
		txa
		and #$10
		beq .b40
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .set_fifth_bit
.b40		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a

.set_fifth_bit	nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a

		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		asl 
		sta spi_rx
	
		nop
		nop
		nop
		nop
		txa
		and #$08
		beq .b50
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .set_sixth_bit
.b50		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a

.set_sixth_bit  nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a
		
		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		asl 
		sta spi_rx
		
		nop
		nop
		nop
		nop
		txa
		and #$04
		beq .b60
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .set_seventh_bit
.b60		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a

.set_seventh_bit nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a

		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		asl 
		sta spi_rx

		nop
		nop
		nop
		nop
		txa
		and #$02
		beq .b70
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .set_eighth_bit
.b70		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a
		
.set_eighth_bit nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a

 		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		asl
		sta spi_rx

		nop
		nop
		nop
		nop
		txa
		and #$01
		beq .b80
		lda port_a
		ora #S_MOSI
		eor #S_CLK
		sta port_a
		jmp .spi_out_done
.b80		lda port_a
		and #S_NOT_MOSI
		eor #S_CLK
		sta port_a

.spi_out_done	nop
		nop
		nop
		nop
		lda port_a
		ora #S_CLK
		sta port_a
		lda port_b	; get data from adc
		and #S_MISO
		ora spi_rx
		sta spi_rx

		nop
		nop
		nop
		nop
		lda port_a
		ora #S_CS
		eor #S_MOSI
		eor #S_CLK
		sta port_a
		rts
		

;; End SPI routines


;----------------------------------------------------------------------------------
;  FUNCTION NAME: itoa
;
;  DESCRIPTION: Use divisions by 10 on a 16-bit integer and add ascii offset
; 		on the remainder to convert a binary number to a string
;		
;----------------------------------------------------------------------------------
;  ARGUMENTS: (none)Value to convert must be in itoa_msb and itoa_lsb (ZP) before the call
;  RETURNS:   (none)String is stored on ZP, starting at itoa_str address	  
;----------------------------------------------------------------------------------
; Sad attempt at implementing the following algorithm: 
; 1. q = (n >> 1) + (n >> 2) 				
; 2. q = q + (q >> 4) 					
; 3. q = q + (q >> 8)					
; 4. q = q >> 3						
; 5. r = n - q*10  -> r = n - (((q << 2) + q) << 1) 	
; 6. sub 10 from r if r >= 10, add 1 to q, 		
; r is now the remainder, q the quotient		

itoa 		ldx #$05	; loop counter, 16-bit -> 5 digits
.itoa_loop
; 1. q = (n >> 1) + (n >> 2)
		lda itoa_msb
		sta temp_msb
		lda itoa_lsb
		pha		; stack -> initial lsb, needed for last step
		sta temp_lsb
		lsr itoa_msb
		ror itoa_lsb  	; n >> 1 in itoa vars
		lsr temp_msb
		ror temp_lsb
		lsr temp_msb
		ror temp_lsb 	; n >> 2 in temp vars
		clc
		lda temp_lsb
		adc itoa_lsb
		sta itoa_lsb
		sta temp_lsb
		lda temp_msb
		adc itoa_msb
		sta itoa_msb
		sta temp_msb
; 2. q = q + (q >> 4)
		lsr temp_msb
		ror temp_lsb
		lsr temp_msb
		ror temp_lsb
		lsr temp_msb
		ror temp_lsb
		lsr temp_msb
		ror temp_lsb
		clc
		lda temp_lsb
		adc itoa_lsb
		sta temp_lsb
		sta itoa_lsb
		lda temp_msb
		adc itoa_msb
		sta temp_msb
		sta itoa_msb		
; 3. q = q + (q >> 8)
		lda temp_msb
		sta temp_lsb
		stz temp_msb
		clc
		adc itoa_lsb
		sta itoa_lsb
		lda 0
		adc itoa_msb
		sta itoa_msb
; 4. q = q >> 3
		lsr itoa_msb
		ror itoa_lsb
		lsr itoa_msb
		ror itoa_lsb
		lsr itoa_msb
		ror itoa_lsb	; at this point the quotient is ready for next loop iter
; 5. r = n - q*10  -> r = n - (((q << 2) + q) << 1)
		lda itoa_lsb	
		asl
		asl 
		clc
		adc itoa_lsb
		asl
		sta temp_lsb
		pla
		sec
		sbc temp_lsb		; acc now has remainder
		sta temp_lsb
		cmp #$0A 	
		bcc .remainder_ok
		sec
		sbc #$0A		; if remainder is over 10, sub 10, add 1 to quotient		
		sta temp_lsb
		inc itoa_lsb
		bne .remainder_ok
		inc itoa_msb
.remainder_ok
		lda temp_lsb
		clc
		adc #$30		; add 30 to get ascii
		dex
		sta itoa_str, x
		beq .itoa_end 		;; too far to branch directly to top of loop
		jmp .itoa_loop		;; have to take a long jump
.itoa_end
		rts			


;----------------------------------------------------------------------------------
;  FUNCTION NAME: print_itoa_str
;
;  DESCRIPTION: Print up to 5 characters generated by itoa, starting from the 
;		most significant non-zero digit.
;----------------------------------------------------------------------------------
;  ARGUMENTS: (none) Takes value in itoa_str in ZP
;  RETURNS:   (none) Print on LCD	  
;----------------------------------------------------------------------------------
print_itoa_str	ldx #$0
		ldy #$0
.itoa_print_lp	lda itoa_str,x
		inx
		cmp #$30	; Don't print zeros
		bne .print_it
		cpy #$0		; Which are before a non-zero number.
		bne .print_it
		cpx #$05	; Unless it's the last character
		bcc  .itoa_print_lp

.print_it	iny		; increase y to signal we got a leading non-zero number. 
		sta LCD1
		jsr lcd_busy_wait
		cpx #$05
		bcc .itoa_print_lp
		rts

;----- I2C ----------------------------
; Masks for Ports A and B of the CIA

; Port A / Outputs
I2C_SDAO 	= $10 ; SDA out mask
I2C_SCL 	= $08 ; SCL mask
N_I2C_SDAO	= $EF ; SDA out mask - not
N_I2C_SCL 	= $F7 ; SCL mask - not

; Port B / Input
I2C_SDAI	= $02 ; SDA in mask (buffered input on separate pin)

; Bit masks
bit_0 = $01
bit_1 = $02
bit_2 = $04
bit_3 = $08
bit_4 = $10
bit_5 = $20
bit_6 = $40
bit_7 = $80

;----------------------------------------------------------------------------------
;  FUNCTION NAME: i2c_tx
;
;  DESCRIPTION: Send a byte over I2C
;----------------------------------------------------------------------------------
;  ARGUMENTS: Byte to send in X.
;	      Y must be set to 1 if sendstop in needed, otherwise 0
;  RETURNS:   (none)	  
;----------------------------------------------------------------------------------
; Todo: comments

i2c_tx	
		lda port_a  	;sendstart
		and #N_I2C_SDAO
		sta port_a
		and #N_I2C_SCL
		sta port_a
.set_1st	txa
		and #bit_7
		beq .setFirstTo0
		lda port_a
		ora #I2C_SDAO
		jmp .set_2nd
.setFirstTo0	lda port_a
		and #N_I2C_SDAO

.set_2nd	sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_6
		beq .set2ndTo0
		lda port_a
		ora #I2C_SDAO
		jmp .set3rd
.set2ndTo0	lda port_a
		and #N_I2C_SDAO
		
.set3rd		sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_5
		beq .set3rdTo0
		lda port_a
		ora #I2C_SDAO
		jmp .set4th
.set3rdTo0	lda port_a
		and #N_I2C_SDAO

.set4th		sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_4
		beq .set4thTo0
		lda port_a
		ora #I2C_SDAO
		jmp .set5th
.set4thTo0	lda port_a
		and #N_I2C_SDAO

.set5th		sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_3
		beq .set5thTo0
		lda port_a
		ora #I2C_SDAO
		jmp .set6th
.set5thTo0	lda port_a
		and #N_I2C_SDAO

.set6th		sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_2
		beq .set6thTo0
		lda port_a
		ora #I2C_SDAO
		jmp .set7th
.set6thTo0	lda port_a
		and #N_I2C_SDAO

.set7th		sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_1
		beq .set7thTo0
		lda port_a
		ora #I2C_SDAO
		jmp .setrw
.set7thTo0	lda port_a
		and #N_I2C_SDAO

.setrw		sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		txa
		and #bit_0
		beq .setrwTo0
		lda port_a
		ora #I2C_SDAO
		sta port_a
		jmp .i2cReadEnd 
.setrwTo0	lda port_a
		and #N_I2C_SDAO
.i2cWriteAck	sta port_a
		ora #I2C_SCL
		sta port_a
		and #N_I2C_SDAO
		and #N_I2C_SCL
		sta port_a
		ora #I2C_SDAO
		sta port_a
		; not checking ack ....
.i2cReadEnd	ora #I2C_SCL
		sta port_a
		and #N_I2C_SCL
		sta port_a
		and #N_I2C_SDAO 
		sta port_a
		ora #I2C_SCL 
		sta port_a
		cpy #$0
		beq .i2c_write_end
		ora #I2C_SDAO
		sta port_a
.i2c_write_end	rts

;----------------------------------------------------------------------------------
;  FUNCTION NAME: i2c_read
;
;  DESCRIPTION: Read a byte over I2C
;		Nowhere as fast at the minimum clock speed specified in datasheet
;		(~43kHz vs 100kHz) but it works somehow. 
;               (to be kept unrolled just in case)
;----------------------------------------------------------------------------------
;  ARGUMENTS: None
;  RETURNS:   (none) Value on Zero Page (i2c_buffer)
;----------------------------------------------------------------------------------
; Todo: -fix the sendstop on last byte... Currently has to be done after return.
;	-Comments!!

i2c_read	stz i2c_buffer
		lda port_a
		ora #I2C_SDAO
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		tax
.read_1st_i2c	lda #I2C_SDAI
		bit port_b
		beq .i2c_first_0
		inc i2c_buffer
.i2c_first_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_2nd_0
		inc i2c_buffer
.i2c_2nd_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_3rd_0
		inc i2c_buffer
.i2c_3rd_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_4th_0
		inc i2c_buffer
.i2c_4th_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_5th_0
		inc i2c_buffer
.i2c_5th_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_6th_0
		inc i2c_buffer
.i2c_6th_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_7th_0
		inc i2c_buffer
.i2c_7th_0	asl i2c_buffer
		txa
		and #N_I2C_SCL
		sta port_a
		nop
		ora #I2C_SCL
		sta port_a
		lda #I2C_SDAI
		bit port_b
		beq .i2c_8th_0
		inc i2c_buffer		
.i2c_8th_0	txa
		and #N_I2C_SCL
		cpy #$0
		beq .i2c_nak
		and #N_I2C_SDAO
.i2c_nak	sta port_a
		nop
		ora #I2C_SCL 
		sta port_a
		nop
		and #N_I2C_SCL
		sta port_a
		ora #I2C_SDAO
		sta port_a
		rts ; end. send stop manually when done, afterward.

;----------------------------------------------------------------------------------
;  FUNCTION NAME: get_i2c_vals
;
;  DESCRIPTION: Get the humidity and temperature sensor from I2C & convert them
;----------------------------------------------------------------------------------
;  ARGUMENTS: None
;  RETURNS:   (none) 8 bit unsigned values on Zero Page (humidity, air_temp)
;----------------------------------------------------------------------------------
; todo: comments!!1

get_i2c_vals	lda #$27
		asl 
		tax 
		ldy #$01
		jsr i2c_tx
.i2c_wait_loop  lda hz_flag
		cmp #$03
		bcc .i2c_wait_loop
		stz hz_flag
		sei
		lda #$27
		asl
		ora #$1 ; set write
		tax
		ldy #$00
		jsr i2c_tx
		ldy #$01
		jsr i2c_read
		lda i2c_buffer
		sta humidity_msb
		jsr i2c_read
		lda i2c_buffer
		sta humidity_lsb
		jsr i2c_read
		lda i2c_buffer
		sta air_temp_msb
		ldy #$0
		jsr i2c_read
		lda port_a
		and #N_I2C_SDAO	
		sta port_a 
		ora #I2C_SCL
		sta port_a
		ora #I2C_SDAO
		cli
		sta port_a
		lda i2c_buffer
		sta air_temp_lsb
		
		lda humidity_lsb
		sta num1_lsb
		lda humidity_msb
		sta num1_msb
		stz num2_msb
		lda #$64
		sta num2_lsb
		jsr multiply
		lda result2
		sta result1
		lda result3
		sta result2  ;; >> 8
		lsr result2
		ror result1  ;; >> 9
		lsr result2
		ror result1  ;; >> 10
		lsr result2
		ror result1  ;; >> 11
		lsr result2
		ror result1  ;; >> 12
		lsr result2
		ror result1  ;; >> 13
		lsr result2
		ror result1  ;; >> 14
		lda result1
		sta humidity
		
		lsr air_temp_msb
		ror air_temp_lsb
		lsr air_temp_msb
		ror air_temp_lsb
		
		lda air_temp_lsb
		sta num1_lsb
		lda air_temp_msb
		sta num1_msb
		lda #$A5
		sta num2_lsb
		stz num2_msb
		jsr multiply
		lda result2
		sta result1
		lda result3
		sta result2  ;; >> 8
		lsr result2
		ror result1  ;; >> 9
		lsr result2
		ror result1  ;; >> 10
		lsr result2
		ror result1  ;; >> 11
		lsr result2
		ror result1  ;; >> 12
		lsr result2
		ror result1  ;; >> 13
		lsr result2
		ror result1  ;; >> 14
		lda result1
		sec
		sbc #$28
		sta air_temp		
		rts
;----------------------------------------------------------------------------------
;  FUNCTION NAME: multiply
;
;  DESCRIPTION: Multiply 2 16 bit unsigned integers -> 32 bit unsigned
;  Note: I did not write this algo
;----------------------------------------------------------------------------------
;  ARGUMENTS: (none) Values on ZP, num1_msb, num1_lsb, num2_msb, num2_lsb
;  RETURNS:   (none) Value on ZP, result1 to result4
;----------------------------------------------------------------------------------
; Todo: Comments!!

multiply	lda #$00
		sta result3
		ldx #$10
ml1		lsr num2_msb
		ror num2_lsb
		bcc ml2
		tay
		clc
		lda num1_lsb
		adc result3
		sta result3
		tya
		adc num1_msb
ml2		ror 
		ror result3
		ror result2
		ror result1
		dex
		bne ml1
		sta result4
		rts

;----------------------------------------------------------------------------------
;  FUNCTION NAME: divide
;
;  DESCRIPTION: Divide a 16 bit int by a 16 bit int
;  Note: I did not write this algo
;----------------------------------------------------------------------------------
;  ARGUMENTS: (none) Values on ZP (num1_lsb, num1_msb, num2_lsb, num2_msb)
;  RETURNS:   (none) Value on Zero Page (Quotient in num1_lsb, num1_msb
;					remainder in remainder_lsb, remainder_msb)
;----------------------------------------------------------------------------------
; Todo: Comment!!!
		
divide 		lda #$0
		sta remainder_lsb
		sta remainder_msb
		ldx #16
divl1		asl num1_lsb
		rol num1_msb
		rol remainder_lsb
		rol remainder_msb
		lda remainder_lsb
		sec
		sbc num2_lsb
		tay
		lda remainder_msb
		sbc num2_msb
		bcc divl2
		sta remainder_msb
		sty remainder_lsb
		inc num1_lsb
divl2		dex
		bne divl1
		rts

;--- I2C End ----------

; String constants
dreadnought	!raw "Dreadnought", 0
mega		!raw "Mega+", 0
turbo	 	!raw "Turbo", 0
sixtyfive	!raw "65000", 0
adc_str		!raw "ADC val: ", 0
humidity_str	!raw "Humidity: ", 0
temperature_str !raw "Temperature: ", 0
ph_str		!raw "pH: ", 0
