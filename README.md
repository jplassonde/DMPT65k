# Dreadnought Mega+ Turbo 65000

The Dreadnought Mega+ Turbo 65000 is a 65c02 Single-Breadboard Computer I designed, built, programmed and (mainly) debugged as a part of a school project in fall 2017. The primary purpose was to read, convert and transmit sensors data over UART to another device (ConnectCore 6UL SBC Express), so the latter could relay the data on the cloud. 

It features partial software implementation of the I2C and SPI protocols, a 20x4 character LCD, hardware timers, I/O ports and can play Commodore 64 music.

To load the program into the device, I made a small script to convert the assembled binaries to C arrays and a PSoC5 firmware to store that data inside the ROM (flash) chip.

Unfortunately, due to time constraints (being part of a project which had to be completed in around 5 weeks), the main software was rushed. Therefore, although functional and stable, only the essentials features to carry its tasks were implemented.

![dreadnought](https://user-images.githubusercontent.com/36741050/36557215-567cc37a-1810-11e8-969b-55706240e588.png)
