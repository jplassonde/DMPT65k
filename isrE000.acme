;----------------------------------------------------
; Name: isrE000.acme
; Desc: Interrupt service routine for the DM+T65K
; 	Finds interrupt souce, ack it and react
;	accordingly
;
; Todo: test spare CIA (87) to see if it also has
;	timer B glitch... This would be better than
;	incrementing a counter on TA	
;----------------------------------------------------

*=$E000
		!to "isrE000.bin", plain
		!cpu 65c02

;; todo: this should go in a .inc...
play_address	= $1003	; Setting for Crest Rules
cia_icr 	= $DC0D
counter		= $1F
hz_flag 	= $20
uart_int_f 	= $27
uart_data	= $28

int_routine	pha
		phx
		phy		
		
check_timer_a	lda cia_icr 	; ack int/get int data
		and #$01
		beq check_uart
		jsr play_address ; execute SID play routine inside int for now 
	
inc_count	inc counter	; Increment a counter on TA  
		lda counter
		cmp #$32  
		bmi check_uart
		inc hz_flag	; set flag every sec
		stz counter
		
check_uart	lda $D302	; test/ack uart interrupt 
		cmp #$4		; Received data available
		bne end_int
		lda $D300	; read receiver buffer to ack
		sta uart_data
		inc uart_int_f	; signal main loop that data is ready

end_int		ply
		plx
		pla
		rti
