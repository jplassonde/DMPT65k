# DMPT 65k

This is a breadboard computer based on the 65c02 and the Commodore 64 chipset, following the same memory mapping . 

It features partial software implementation of the I2C and SPI protocols (bitbanging on the Complex Interface Adapter), a UART chip, a 20x4 character LCD, hardware timers, I/O ports and can play Commodore 64 music.

To load the program into the device, I made a small script to convert the assembled binaries to C arrays and a PSoC5 firmware to store that data inside the ROM (flash) chip.

I used it to read data from sensors, perform an approximation of the inverse transfer functions, then transmit the resulting data via UART.

Although made with obsolete tech it was an interesting exercise to investigate timing issues and signal integrity using logic analyser, observing clock skew and capacitive loading in their natural habitat.

![dreadnought](https://user-images.githubusercontent.com/36741050/36557215-567cc37a-1810-11e8-969b-55706240e588.png)
