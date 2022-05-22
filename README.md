# WonderSwan CPU Test V0.0.1 (20220518)

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

## How do the undefined flags / undocumented op-codes work?
If there is a division by zero the input (AL, AX/AW) is not modified.
The flags marked as Undefined in the manual are always modified by the instructions,
the flags are never kept as they were before the instruction.

### AND, OR, XOR & TEST
AuxCarry, Carry & Overflow are always cleared.
Parity, Sign & Zero are set according to result.

### MUL
AuxCarry, Parity & Sign are always cleared.
Zero is always set.
Carry & Overflow are set if the result doesn't fit in 8 bits for 8bit multiplies.

### DIV / DIVU (unsigned division)
Normaly:
	AuxCarry, Carry, Overflow, Parity & Sign are always cleared.
	Zero is set when rest is zero and bit 0 of result is set.
If division by zero:
	AuxCarry, Carry, Overflow, Parity & Sign are cleared.
	Zero is set in some weird way (not tested).

### IDIV / DIV (signed division)
If dividing 0x8000 by 0x00 you will not get a division by zero and a result of 0x0081.
Normaly:
	AuxCarry, Carry & Overflow are cleared.
	Parity, Sign & Zero are set according to result.
If division by zero:
	AuxCarry, Carry, Overflow, Parity & Sign are cleared.
	Zero is set in some weird way (not tested).

### AAM / CVTBD
The AAM op-code is a 2 byte op-code, and the second byte can be any value not just 10.
So it's basically a byte by byte divide.
Normaly:
	AuxCarry, Carry & Overflow are cleared.
	Parity, Sign & Zero are set according to result.
If division by zero:
	AuxCarry, Carry, Overflow, Parity & Sign are cleared.
	Zero is set if bit 6 or 7 of AL is set (AL > 0x3F).

### AAD / CVTDB
The AAD op-code just as the AAM op-code is a 2 byte op-code, and the second byte can be any value not just 10. So this is a byte by byte multiplication plus byte addition. The answear is only in AL
and AH is always zero. Flags are calculated only from the add after the multiplication, the flags are exactly like a normal add.

### DAA / ADJ4A
All flags are the same as a normal addition except that AuxCarry & Carry are never cleared.

### DAS / ADJ4S
Compare is done for AL > 0x99 first and then lower nybble ((AL & 0xF) > 0x9).
Same calculation as DAA except it does a subtraction instead of an addition.

### AAA / ADJBA
Overflow is always cleared.
Parity is always set.
AuxCarry, Carry & Zero are set if AuxCarry is set before or (AL & 0xF) > 0x9.
Sign is set when AuxCarry (, Carry & Zero) is not set.
AL is always masked to lower nybble.

### AAS / ADJBS
Overflow is always cleared.
Parity is always set.
AuxCarry, Carry & Zero are set if AuxCarry is set before or (AL & 0xF) > 0x9.
Sign is set when AuxCarry (, Carry & Zero) is not set.
AL is always masked to lower nybble.

### Push SP to stack
8086/80186
{
	SP      = SP - 2
	[SS:SP] = SP
}

## Controls:
Use WS X1-X4 to navigate the menus. A to select/continue, B to go back/cancel.


## Credits:

Fredrik Ahlström

Twitter @TheRealFluBBa

https://github.com/FluBBaOfWard/WSCpuTest
