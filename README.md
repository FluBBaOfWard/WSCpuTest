# WonderSwan CPU Test V0.6.0 (20220713)

This is a CPU Test program for Bandai WonderSwan (Color/Crystal) & PocketChallenge V2.

## How to use:

Load the ROM in an emulator or flash it to a flashcart and put it in your WonderSwan.
The program will go through all the tests and then write "Ok".
If run in an emulator and it doesn't emulate the WonderSwan CPU correctly,
the program will stop at the first failure and print out intput value/flags and  expected value/flags (and exception for division). Press A to try the next value or B to try the next test.
Now you can use the X1-X4 to navigate the menus, A to select an option,
B to go back.

## Building:
	I use nasm https://nasm.us/ by running "nasm -f bin -o WSCpuTest.wsc WSCpuTest.asm".

## How do the undefined flags / opcodes work?
If there is a division exception the input (AL, AX/AW) is not modified.
The flags marked as Undefined in the manual are always modified by the instructions, the flags are never kept as they were before the instruction.
Most undefined opcodes are just 1 byte NOPs, the FPO1 (0xD8 - 0xDF) opcodes are 2 bytes NOPs.

### AND, OR, XOR & TEST
AuxCarry, Carry & Overflow are always cleared.
Parity, Sign & Zero are set according to result.

### NOT
No flags are changed, all bits of result are inverted.

### INC/DEC
Carry is not changed.
AuxCarry, Overflow, Parity, Sign & Zero are all set according to result (same as ADD/SUB 1).

### ADD
AuxCarry, Carry, Overflow, Parity, Sign & Zero are all set according to result.

### SUB
AuxCarry, Carry, Overflow, Parity, Sign & Zero are all set according to result.

### CMP
AuxCarry, Carry, Overflow, Parity, Sign & Zero are all set according to result.
Same as SUB except there is no result.

### ADC/ADDC

### SBB/SUBC

### NEG
AuxCarry, Carry, Overflow, Parity, Sign & Zero are all set according to result.
It's the same as doing a SUB with the destination set to 0.

### ROL
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of Carry value and bit 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### ROR
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of bit 6/14 & 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### RCL/ROLC
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of Carry value and bit 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### RCR/RORC
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of bit 6/14 & 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### SHL
AuxCarry is always cleared.
Parity, Sign & Zero are set according to result.
Overflow is set to xor of Carry value and bit 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### SHR
AuxCarry is always cleared.
Parity, Sign & Zero are set according to result.
Overflow is set to xor of bit 6/14 & 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### SAR / SHRA
AuxCarry is always cleared.
Parity, Sign & Zero are set according to result.
Overflow is set to xor of bit 6/14 & 7/15 of destination.
Normaly:
	Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
	Carry is not changed.

### MUL
AuxCarry, Parity & Sign are always cleared.
Zero is always set.
Carry & Overflow are set if the result doesn't fit in 8 bits for 8bit multiplies.

### DIV / DIVU (unsigned division)
Normaly:
	AuxCarry, Parity & Sign are always cleared.
	Carry & Overflow are from the last multiplication.
	Zero is set when remainder is zero and bit 0 of result is set.
If division exception:
	AuxCarry, Parity & Sign are always cleared.
	Carry & Overflow are from the last multiplication.
	Zero is set in some weird way (not tested).

### IDIV / DIV (signed division)
If dividing 0x8000 by 0x00 you will not get a division exception and a result of 0x0081.
Normaly:
	AuxCarry, Carry & Overflow are cleared.
	Parity, Sign & Zero are set according to result.
If division exception:
	AuxCarry, Parity & Sign are always cleared.
	Carry & Overflow are from the last multiplication.
	Zero is set in some weird way (not tested).

### AAM / CVTBD
The AAM opcode is a 2 byte opcode, and the second byte can be any value not just 10. So it's basically a byte by byte divide though the result is in AH and remainder in AL.
Normaly:
	AuxCarry, Carry & Overflow are cleared.
	Parity, Sign & Zero are set according to result (of AL, remainder).
If division exception:
	AuxCarry, Parity & Sign are always cleared.
	Carry & Overflow are from the last multiplication.
	Zero is set if bit 6 or 7 of AL is set (AL > 0x3F).

### AAD / CVTDB
The AAD opcode just as the AAM opcode is a 2 byte opcode, and the second byte can be any value not just 10. So this is a byte by byte multiplication plus byte addition. The answear is only in AL and AH is always zero. Flags are calculated only from the add after the multiplication, the flags are exactly like a normal add.

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

### PUSH/POP SP to/from stack
8086/80186
PUSH SP
{
	SP      = SP - 2
	[SS:SP] = SP
}

POP SP
{
	SP = [SS:SP]
}

## Undefined opcodes

### 0x0F
This opcode is not "POP CS" or Group3 it's just a 1 byte NOP.

### 0x63, 0x64, 0x65, 0x66, 0x67
These opcodes doesn't do anything, they are just 1 byte NOPs.

### 0x9B
This is known as POLL on other NEC Vx0 CPUs, on the V30MZ it doesn't wait or cause exception, just a 1 byte NOP.

### 0xD6
This is a one byte opcode called SALC, it sets AL to either 0x00 or 0xFF depending on if Carry is set or not. Though I couldn't get STC to set the carry before the SALC...

### 0xD8 - 0xDF
These opcodes are called FPO on other NEC Vx0 CPUs, used to communicate with an FPU.
On the V30MZ they are 2 byte NOPs.

### 0xF1
It doesn't look like it's a simple INT1 as I couldn't get a test to work, it looks like it might be BRKS from NEC V25/V35 or at least that it switches the MD flag. This app doesn't test it, if someone can write a test that works please contact me.

### 0xC0,0xF0,0x## (SAL al, ##)
This doesn't work as Shift Arithmetic Left but instead zeros al.

### 0xC1,0xF0,0x## (SAL ax, ##)
This doesn't work as Shift Arithmetic Left but instead zeros ax.

### 0xF6,0xC8
### 0xF7,0xC8

### 0xFE,0xD0 - 0xFE,0xF0
Does the same as 0xFF variants

### 0xFF,0xF8 (PUSH AX)
This doesn't seem to do anything.

## Controls:
Use WS X1-X4 to navigate the menus. A to select/continue, B to go back/cancel.


## Credits:

Fredrik Ahlström

Twitter @TheRealFluBBa

https://github.com/FluBBaOfWard/WSCpuTest
