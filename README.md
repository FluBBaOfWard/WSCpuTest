# WonderSwan CPU Test V0.0.1 (20220501)

This is a CPU Test program for Bandai WonderSwan (Color/Crystal) & PocketChallenge V2.

## How to use:

Load the ROM in an emulator or flash it to a flashcart and put it in your WonderSwan.
The program will go through all the tests and then write "Ok".
If run in an emulator and it doesn't emulate the WonderSwan CPU correctly,
the program will stop at the first failure and print out expected value/flags
and the 
Now you can use the X1-X4 to navigate the menus, A to select an option,
B to go back.

## Building:
	I use nasm https://nasm.us/ by running "nasm -f bin -o WSCpuTest.wsc WSCpuTest.asm".

## How do the undefine flags work?
### Mul
Mulu/Muls/IMul change all the undefined flags.
AuxCarry, Parity & Sign are always cleared, Zero is always set.


## Controls:
Use WS X1-X4 to navigate the menus. A to select, B to go back.


## Credits:

Fredrik Ahlström

Twitter @TheRealFluBBa

http://www.github.com/FluBBaOfWard
