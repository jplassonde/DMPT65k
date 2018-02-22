/*-----------------------------------------------------------------------
* Flash programmer                                                      |
* Description: Write binary code (converted to C arrays)                |        
* on an SST39SF040 DIP chip using Cypress CY8CKIT-059 (PSoC5 LP)        |
*                                                                       |
* Code for each sector need to be included in their respective header   |
*                                                                       |
* Caveat: The timing delay are a lot longer than specified on datasheet |
*         as faster transactions were unreliable.                       |
*         Tweaking these values could improve programming time.         |
*----------------------------------------------------------------------*/

//#define WRITE_C0  // Program sector C0 (music data)
//#define WRITE_E0  // Program sector E0 (ISR)
#define WRITE_F0    // Program sector F0 (main program)

#include "project.h"

#ifdef WRITE_C0
#include "sectorC0.h"
extern uint8 sectorC0[];
#endif

#ifdef WRITE_E0
#include "sectorE0.h"
extern uint8 sectorE0[];
#endif

#ifdef WRITE_F0
#include "sectorF0.h"
extern uint8 sectorF0[];
#endif

// Output 16-bit address on bus
void set_address(uint16 address) {
    A0_Write((address & 1));
    A1_Write((address >> 1) & 1 );
    A2_Write((address >> 2) & 1 );
    A3_Write((address >> 3) & 1 );
    A4_Write((address >> 4) & 1 );
    A5_Write((address >> 5) & 1 );
    A6_Write((address >> 6) & 1 );
    A7_Write((address >> 7) & 1 );
    A8_Write((address >> 8) & 1 );
    A9_Write((address >> 9) & 1 );
    A10_Write((address >> 10) & 1 );
    A11_Write((address >> 11) & 1 );
    A12_Write((address >> 12) & 1 );
    A13_Write((address >> 13) & 1 );
    A14_Write((address >> 14) & 1 );
    A15_Write((address >> 15) & 1 );
}

// Output 8-bit data on bus
void set_data(uint8 data) {
    D0_Write((data & 1));
    D1_Write((data >> 1) & 1);
    D2_Write((data >> 2) & 1);
    D3_Write((data >> 3) & 1);
    D4_Write((data >> 4) & 1);
    D5_Write((data >> 5) & 1);
    D6_Write((data >> 6) & 1);
    D7_Write((data >> 7) & 1);
}

// Generate control bus signal to select address and send data
void write_cycle(uint16 address, uint8 data) {
    set_address(address); 
    OE_Write(1);
    WE_Write(0);
    CyDelay(1);
    CE_Write(0);

    CyDelay(1);  
    set_data(data);
    CyDelay(1);
    
    CE_Write(1);
    WE_Write(1);
    OE_Write(0);
    CyDelay(1);
}

// Send command sequence for Sector-Erase
void erase_sector(uint16 base_address) {
    write_cycle(0x5555, 0xAA);
    write_cycle(0x2AAA, 0x55);
    write_cycle(0x5555, 0x80);
    write_cycle(0x5555, 0xAA);
    write_cycle(0x2AAA, 0x55);
    write_cycle(base_address, 0x30);
    CyDelay(50);
}

// Send command sequence for Byte-Program
void write_byte(uint16 address, uint8 data) {
    write_cycle(0x5555, 0xAA);
    write_cycle(0x2AAA, 0x55);
    write_cycle(0x5555, 0xA0);
    write_cycle(address, data);
    CyDelay(1);
}

int main(void)
{  
    uint16 i;
    CyGlobalIntEnable;

    // Give some delay at power-on before starting the programming
    CyDelay(100); 
    
    // Init control bus
    OE_Write(0);
    WE_Write(1);
    CE_Write(1);
    CyDelay(1);
   
#ifdef WRITE_C0
    erase_sector(0xC000);
    for (i=0; i < sizeof(sectorC0)/sizeof(sectorC0[0]);i++) {
        write_byte(0xc000+i, sectorC0[i]);
    }
#endif

#ifdef WRITE_E0
    erase_sector(0xE000);
    for (i=0; i < sizeof(sectorE0)/sizeof(sectorE0[0]);i++) {
        write_byte(0xe000+i, sectorE0[i]);
    }
#endif

#ifdef WRITE_F0
    erase_sector(0xF000);
    
    // PC Init to $F000
    write_byte(0xFFFC, 0x00);
    write_byte(0xFFFD, 0xF0);
    
    // ISR Init to $E000
    write_byte(0xFFFE, 0x00);
    write_byte(0xFFFF, 0xE0);
    
    for (i=0;i < sizeof(sectorF0)/sizeof(sectorF0[0]);i++) {
        write_byte(0xf000+i, sectorF0[i]);
    }
 #endif   
    
    for(;;) {} // Programming done. Hang program. An onboard LED indicate end of transfer.
}

/* [] END OF FILE */
