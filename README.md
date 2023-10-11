# WonderSwan CPU Test V0.7.1 (20231011)

This is a CPU Test program for Bandai WonderSwan (Color/Crystal) & Benesse PocketChallenge V2.

## How to use

Load the ROM in an emulator or flash it to a flashcart and put it in your WonderSwan.
The program will go through all the tests and then write "Ok".
If run in an emulator and it doesn't emulate the WonderSwan CPU correctly,
the program will stop at the first failure and print out intput value/flags and  expected value/flags (and exception for division). Press A to try the next value or B to try the next test.
Now you can use the X1-X4 to navigate the menus, A to select an option,
B to go back.

## Building

I use nasm <https://nasm.us/> by running "nasm -f bin -o WSCpuTest.wsc WSCpuTest.asm".

## How do the undefined flags / opcodes work?

The flags marked as Undefined in the manual are always modified by the instructions, the flags are never kept as they were before the instruction.
Most undefined opcodes are just 1 byte NOPs, the FPO1 (0xD8 - 0xDF) opcodes are 2 bytes NOPs.

## Differences between ASWAN & SPHINX(2)

There is one difference between the SOCs and that is the Zero flag during unsigned Multiplication, it's allways set on ASWAN and allways clear on SPHINX(2).

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

```text
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of Carry value and bit 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### ROR

```text
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of bit 6/14 & 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### RCL/ROLC

```text
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of Carry value and bit 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### RCR/RORC

```text
AuxCarry, Parity, Sign & Zero are not changed.
Overflow is set to xor of bit 6/14 & 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### SHL

```text
AuxCarry is always cleared.
Parity, Sign & Zero are set according to result.
Overflow is set to xor of Carry value and bit 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### SHR

```text
AuxCarry is always cleared.
Parity, Sign & Zero are set according to result.
Overflow is set to xor of bit 6/14 & 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### SAR / SHRA

```text
AuxCarry is always cleared.
Parity, Sign & Zero are set according to result.
Overflow is set to xor of bit 6/14 & 7/15 of result.
Normaly:
    Carry is set if the last shifted bit was 1, otherwise cleared.
If the argument is & 0x1F = zero, ie. no shift is taking place:
    Carry is not changed.
```

### MUL / MULU (unsigned multiplication, 8x8)

```text
AuxCarry, Parity & Sign are always cleared.
On Color/Crystal: Zero is always set.
On Mono: Zero is always cleared.
Carry & Overflow are set if the result doesn't fit in 8 bits for 8bit multiplies.
```

### IMUL / MUL (signed multiplication, 8x8)

```text
AuxCarry, Parity & Sign are always cleared.
Zero is always set.
Carry & Overflow are set if the result doesn't fit in 8 bits for 8bit multiplies.
```

### DIV / DIVU (unsigned division, 16/8)

```text
Normaly:
    AuxCarry, Parity & Sign are always cleared.
    Carry & Overflow are from the last multiplication.
    Zero is set when remainder is zero and bit 0 of result is set.
If division exception:
    AuxCarry, Parity & Sign are always cleared.
    Carry & Overflow are from the last multiplication.
    Zero is set in some weird way (not tested).
    AX/AW is not modified.
```

### DIV / DIVU (unsigned division, 32/16)

```text
Normaly:
    AuxCarry, Carry, Overflow, Parity, & Sign are always cleared.
    Zero is set when remainder is zero and bit 0 of result is set.
If division exception:
    AuxCarry, Carry, Overflow, Parity, & Sign are always cleared.
    Zero is set in some weird way (not tested).
    AX/AW, DX/DW is not modified.
```

### IDIV / DIV (signed division, 16/8)

```text
If dividing 0x8000 by 0x00 you will not get a division exception but a result of 0x0081.
Normaly:
    AuxCarry, Carry & Overflow are cleared.
    Parity, Sign & Zero are set according to result (AL).
If division exception:
    AuxCarry, Parity & Sign are always cleared.
    Carry & Overflow are from the last multiplication.
    Zero is set in some weird way (not tested).
    AX/AW is not modified.
```

### AAM / CVTBD (8/8)

```text
The AAM opcode is a 2 byte opcode, and the second byte can be any value not just 10. So it's basically a byte by byte divide though the result is in AH and remainder in AL.
Normaly:
    AuxCarry, Carry & Overflow are cleared.
    Parity, Sign & Zero are set according to result (of AL, remainder).
If division exception:
    AuxCarry, Parity & Sign are always cleared.
    Carry & Overflow are from the last multiplication.
    Zero is set if bit 6 or 7 of AL is set (AL > 0x3F).
    AL, AX/AW is not modified.
```

### AAD / CVTDB (8x8+8)

The AAD opcode just as the AAM opcode is a 2 byte opcode, and the second byte can be any value not just 10. So this is a byte by byte multiplication plus byte addition. The answear is only in AL, AH is always zero. Flags are calculated only from the add after the multiplication, the flags are exactly like a normal add.

### DAA / ADJ4A

All flags are the same as a normal addition except that AuxCarry & Carry are never cleared.

### DAS / ADJ4S

Compare is done for AL > 0x99 first and then lower nybble ((AL & 0xF) > 0x9).
Same calculation as DAA except it does a subtraction instead of an addition.

### AAA / ADJBA

Overflow is always cleared. Parity is always set.
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

```code
PUSH SP
{
    SP      = SP - 2
    [SS:SP] = SP
}

POP SP
{
    SP = [SS:SP]
}
```

## BOUND / CHKIND

Comparison of values are done with signed values.

## Undefined opcodes

### 0x0F

This opcode is not "POP CS" or Group3 it's just a 1 byte NOP (1 cycle).

### 0x63, 0x64, 0x65, 0x66, 0x67

These opcodes doesn't do anything, they are just 1 byte NOPs (1 cycle).

### 0x8C,0xF8 / 0x8E,0xF8

This is to test that bit 5 (0x20) does not affect which segment register is accessed.

### 0x8D,0xC8 - 0x8D,0xCF (LEA cx)

```text
This is the LEA instruction but with address mode set to register.
It doesn't use the registers directly but instead gives you a couple of new addressing modes.
The low 3 bits are mapped like this:
0x0 = [ds:bx + ax]
0x1 = [ds:bx + cx]
0x2 = [ss:bp + dx]
0x3 = [ss:bp + bx]
0x4 = [ds:si + sp]
0x5 = [ds:di + bp]
0x6 = [ss:bp + si]
0x7 = [ds:bx + di]
```

### 0x9B (WAIT / POLL)

On the WonderSwan it doesn't wait or cause exception, the POLL pin is probably held low at all times, works as a 1 byte NOP (9 cycles).

### 0xC0,0xF0,0x## (SAL al, ##)

This doesn't work as Shift Arithmetic Left but instead zeros al.

### 0xC1,0xF0,0x## (SAL ax, ##)

This doesn't work as Shift Arithmetic Left but instead zeros ax.

### 0xC4,0xD8 - 0xC4,0xDF (LES bx)

This is the LES instruction but with address mode set to register. It doesn't use the registers directly but instead uses the same new addressing modes as LEA.

### 0xC5,0xD8 - 0xC5,0xDF (LDS bx)

This is the LDS instruction but with address mode set to register. It doesn't use the registers directly but instead uses the same new addressing modes as LEA.

### 0xD6 (SALC)

This is a one byte opcode called SALC, it sets AL to either 0x00 or 0xFF depending on if Carry is set or not (8 cycles). Though I couldn't get STC to set carry before the SALC...

### 0xD8 - 0xDF

These opcodes are called FPO on other NEC Vx0 CPUs, used to communicate with an FPU. On the V30MZ they are 2 byte NOPs (1 cycle).

### 0xF1

It doesn't look like it's a simple INT1 as I couldn't get a test to work, it looks like it might be BRKS from NEC V25/V35 or at least that it switches the MD flag. This app doesn't test it, if someone can write a test that works please contact me.

### 0xF6,0xC8 / 0xF7,0xC8 (TEST)

This doesn't seem to change flags or registers (1 cycle).

### 0xFE,0xD0 - 0xFE,0xF0

Does the same as 0xFF variants (CALL, BRA & PUSH).

### 0xFF,0xF8 (PUSH AX?)

This doesn't seem to do anything.

## Controls

Use WS X1-X4 to navigate the menus. A to select/continue failed test, B to go back/skip failed test.

## Credits

Fredrik Ahlström

Twitter @TheRealFluBBa

<https://github.com/FluBBaOfWard/WSCpuTest>
