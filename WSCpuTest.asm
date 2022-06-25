;-----------------------------------------------------------------------------
;
;  WonderSwan CPU Test
;         by Fredrik Ahlström, 2022
;         https://github.com/FluBBaOfWard/WSCpuTest
;
;  For more information on the hardware specs, port descriptions, sprite
;  format, etc., see the hardware.txt file in the wonderdev root directory.
;
;  UP/DOWN    - Choose option
;  A          - Start
;
;  Assemble with: 
;                   nasm -f bin -o WSCpuTest.wsc WSCpuTest.asm
;
;-----------------------------------------------------------------------------

	ORG 0x0000
	CPU 186
	BITS 16

SECTION .data
	%include "WonderSwan.inc"

	MYSEGMENT equ 0xf000
	foregroundMap equ WS_TILE_BANK - MAP_SIZE
	backgroundMap equ foregroundMap - MAP_SIZE
	spriteTable equ backgroundMap - SPR_TABLE_SIZE

	COLLISION_RADIUS equ 6
	PSR_S equ 0x80
	PSR_Z equ 0x40
	PSR_P equ 0x04

SECTION .text
	;PADDING 15

initialize:
	cli
	cld

;-----------------------------------------------------------------------------
; If it's not the Color version of the console, lock the CPU
;-----------------------------------------------------------------------------
;	in al, IO_HARDWARE_TYPE
;	test al, WS_COLOR
;lock_cpu:
;	jz lock_cpu

;-----------------------------------------------------------------------------
; Initialize registers and RAM
;-----------------------------------------------------------------------------
	mov ax, MYSEGMENT
	mov ds, ax
	xor ax, ax
	mov es, ax			; Set ES segment to 0x0000 (RAM).

	; Setup stack
	mov bp, ax
	mov ss, ax
	mov sp, WS_STACK

	; Clear Ram
	mov di, 0x0100
	mov cx, 0x1E80
	rep stosw

	out IO_SRAM_BANK,al

;-----------------------------------------------------------------------------
; Initialize variables
;-----------------------------------------------------------------------------
	mov word [es:globalFrameCounter], 0
	mov word [es:lfsr1], 0x0234
	mov word [es:lfsr2], 0x1234

;-----------------------------------------------------------------------------
; Initialize video
;-----------------------------------------------------------------------------
	in al, SYSTEM_CTRL2
;	or al, VMODE_4C | VMODE_CLEANINIT
	or al, VMODE_CLEANINIT
	out SYSTEM_CTRL2, al

	xor ax, ax
	mov al, BG_MAP( backgroundMap ) | FG_MAP( foregroundMap )
	out IO_SCR_AREA, al

	mov al, SPR_AREA( spriteTable )
	out IO_SPR_AREA, al

	in al, IO_LCD_IF_CTRL
	or al, LCD_ON
	out IO_LCD_IF_CTRL, al

	xor al, al
	out IO_LCD_SEG_DATA, al

;-----------------------------------------------------------------------------
; Register our interrupt handlers
;-----------------------------------------------------------------------------
	mov di, 0*4		; Division error vector
	mov word [es:di], divisionErrorHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 6*4		; Illegal instruction vector
	mov word [es:di], illegalInstructionHandler
	mov word [es:di + 2], MYSEGMENT

	mov di, 0x10*4	; output char vector
	mov word [es:di], outputCharHandler
	mov word [es:di + 2], MYSEGMENT

	mov ax, INT_BASE
	out IO_INT_VECTOR, al

	mov di, INTVEC_VBLANK_START
	add di, ax
	shl di, 2
	mov word [es:di], vblankInterruptHandler
	mov word [es:di + 2], MYSEGMENT

	; Clear HBL & Timer
	xor ax, ax
	out IOw_H_BLANK_TIMER, ax
	out IO_TIMER_CTRL, al

	; Acknowledge all interrupts
	dec al
	out INT_CAUSE_CLEAR, al

	; Enable VBL interrupt
	mov al, INT_VBLANK_START 
	out IO_INT_ENABLE, al

	; We have finished initializing, interrupts can now fire again
	sti

;-----------------------------------------------------------------------------
; Copy font tile data into WS's tile mem
;-----------------------------------------------------------------------------
	; Copy font tile data to tile bank 1
	xor ax,ax
	mov si, MonoFont
	mov di, WS_TILE_BANK + 16*16*2
	mov cx, 8*16*6
monoFontLoop:
	lodsb
	stosw
	dec cx
	jnz monoFontLoop

;-----------------------------------------------------------------------------
; Copy font palette into WSC's palette area
;-----------------------------------------------------------------------------

	; Copy 2-colour (2 bytes per colour) font palette to 
	; beginning of palettes area (becoming palette 0)
	mov si, FontTilePalette
	mov di, WSC_PALETTES
	mov cx, 2
	rep movsw

	mov al, 0xf0
	out IO_LCD_GRAY_01, al
	mov ax, 0x0010
	out IOw_SCR_LUT_0, ax

;-----------------------------------------------------------------------------
; Make background map point to our tiles, essentially "painting" the
; background layer with our tiles, coloured as per our palettes
;-----------------------------------------------------------------------------

	call clearScreen

	mov si, headLineStr
	call writeString

	; Turn on display
	mov al, BG_ON
	out IO_DISPLAY_CTRL, al

	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al


	call testEqu
	call testAnd8
	call testNot8
	call testOr8
	call testTest8
	call testXor8
	call testInc8
	call testDec8

	call testAdd8
	call testSub8
	call testCmp8
	call testNeg8
	call testAdc8
	call testSbb8

	call testRol8
	call testRor8
	call testRcl8
	call testRcr8
	call testShl8
	call testShr8
	call testSar8

	call testDaa
	call testDas
	call testAaa
	call testAas
	call testSPStack
	call testJmp

	call testMulu8
	call testMuls8
	call testAad

	call testAam
	call testDivu8
	call testDivs8

skipTests:
;-----------------------------------------------------------------------------
; Done initializing... We can now start the main loop.
;-----------------------------------------------------------------------------
	; Start main loop
	jmp main_loop

;-----------------------------------------------------------------------------
; Test equality by CMP, SUB & XOR of all byte/word values.
;-----------------------------------------------------------------------------
testEqu:
	mov si, testingEquStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cl, 0
testEqu8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], cl
	mov al, cl
	cmp al, cl
	jnz equ8Failed
	sub al, cl
	jnz equ8Failed
	mov al, cl
	xor al, cl
	jnz equ8Failed
continueEqu8:
	inc cl
	jnz testEqu8Loop

	mov cl, 0
testNeq8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], cl
	mov al, cl
	inc al
	cmp al, cl
	jz neq8Failed
	sub al, cl
	jz neq8Failed
	mov al, cl
	inc al
	xor al, cl
	jz neq8Failed
continueNeq8:
	inc cl
	jnz testNeq8Loop

	hlt
	mov al, 10
	int 0x10
	mov si, test16x16InputStr
	call writeString
	mov byte [es:isTesting], 3

	mov cx, 0
testEqu16Loop:
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
	mov ax, cx
	cmp ax, cx
	jnz equ16Failed
	sub ax, cx
	jnz equ16Failed
	mov ax, cx
	xor ax, cx
	jnz equ16Failed
continueEqu16:
	inc cx
	jnz testEqu16Loop

	mov cx, 0
testNeq16Loop:
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
	mov ax, cx
	inc ax
	cmp ax, cx
	jz neq16Failed
	sub ax, cx
	jz neq16Failed
	mov ax, cx
	inc ax
	xor ax, cx
	jz neq16Failed
continueNeq16:
	inc cx
	jnz testNeq16Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
equ8Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueEqu8
	ret
neq8Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueNeq8
	ret
equ16Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueEqu16
	ret
neq16Failed:
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jnz continueNeq16
	ret

;-----------------------------------------------------------------------------
; Test logical AND of all byte values.
;-----------------------------------------------------------------------------
testAnd8:
	mov si, testingAnd8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov word [es:expectedResult1], 0

	xor cx, cx
testAnd8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov ax, cx
	not ax
	or al, ah
	not al
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testAnd8Single
	xor al, 0
	jnz stopAnd8Test
continueAnd8:
	inc cx
	jnz testAnd8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAnd8Test:
	call checkKeyInput
	xor al, 0
	jnz continueAnd8
	ret

;-----------------------------------------------------------------------------
testAnd8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	and al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz and8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz and8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	and al, cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz and8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz and8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

and8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical NOT of all byte values.
;-----------------------------------------------------------------------------
testNot8:
	mov si, testingNot8Str
	call writeString
	mov si, test8InputStr
	call writeString

	mov byte [es:isTesting], 4
	mov word [es:expectedResult1], 0

	xor cx, cx
	dec ch
testNot8Loop:
	mov [es:inputVal1], cl
	mov [es:expectedResult1], ch
	call testNot8Single
	xor al, 0
	jnz stopNot8Test
continueNot8:
	dec ch
	inc cl
	jnz testNot8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopNot8Test:
	call checkKeyInput
	xor al, 0
	jnz continueNot8
	ret

;-----------------------------------------------------------------------------
testNot8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax
	mov ax, 0xF202
	mov [es:expectedFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	popf
	not al
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz not8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz not8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax
	mov ax, 0xFAD7
	mov [es:expectedFlags], ax

	xor bh, bh
	mov bl, [es:inputVal1]
	popf
	not bl
	pushf

	mov [es:testedResult1], bx
	pop ax
	mov [es:testedFlags], ax
	mov cx, [es:expectedResult1]
	xor bx, cx
	jnz not8Failed
	mov cx, [es:expectedFlags]
	xor ax, cx
	jnz not8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

not8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical OR of all byte values.
;-----------------------------------------------------------------------------
testOr8:
	mov si, testingOr8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov word [es:expectedResult1], 0

	mov cx, 0
testOr8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov ax, cx
	not ax
	and al, ah
	not al
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testOr8Single
	xor al, 0
	jnz stopOr8Test
continueOr8:
	inc cx
	jnz testOr8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopOr8Test:
	call checkKeyInput
	xor al, 0
	jnz continueOr8
	ret

;-----------------------------------------------------------------------------
testOr8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	or al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz or8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz or8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	or al, cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz or8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz or8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

or8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical TEST of all byte values.
;-----------------------------------------------------------------------------
testTest8:
	mov si, testingTest8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov word [es:expectedResult1], 0

	xor cx, cx
testTest8Loop:
	mov [es:inputVal1], cl
	mov [es:expectedResult1], cl
	mov [es:inputVal2], ch
	mov al, cl
	and al, ch
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testTest8Single
	xor al, 0
	jnz stopTest8Test
continueTest8:
	inc cx
	jnz testTest8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopTest8Test:
	call checkKeyInput
	xor al, 0
	jnz continueTest8
	ret

;-----------------------------------------------------------------------------
testTest8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	test al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz test8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz test8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	test al, cl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz test8Failed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz test8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

test8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test logical XOR of all byte values.
;-----------------------------------------------------------------------------
testXor8:
	mov si, testingXor8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov word [es:expectedResult1], 0

	mov cx, 0
testXor8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov ax, cx
	and al, ah
	mov bl, cl
	or bl, ch
	not al
	and al, bl
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testXor8Single
	cmp al, 0
	jnz stopXor8Test
continueXor8:
	inc cx
	jnz testXor8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	mov ax, 0
	ret
stopXor8Test:
	call checkKeyInput
	cmp al, 0
	jnz continueXor8
	ret

;-----------------------------------------------------------------------------
testXor8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov ah, 0
	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	xor al, bl
	pushf

	mov [es:testedResult1], al
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	cmp ax, cx
	jnz xor8Failed
	mov cx, [es:expectedFlags]
	cmp bx, cx
	jnz xor8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov ah, 0
	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	xor al, cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	cmp ax, cx
	jnz xor8Failed
	mov cx, [es:expectedFlags]
	cmp bx, cx
	jnz xor8Failed

	mov ax, 0
	pop cx
	pop bx
	ret

xor8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test INC for all byte values.
;-----------------------------------------------------------------------------
testInc8:
	mov si, testingInc8Str
	call writeString
	mov si, test8InputStr
	call writeString

	mov byte [es:isTesting], 4

	xor cx, cx
testInc8Loop:
	mov [es:inputVal1], cl
	call calcInc8Result
	call testInc8Single
	xor al, 0
	jnz stopInc8Test
continueInc8:
	inc cl
	jnz testInc8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopInc8Test:
	call checkKeyInput
	xor al, 0
	jnz continueInc8
	ret

;-----------------------------------------------------------------------------
testInc8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal1]
	popf
	inc bl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz inc8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz inc8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx
	xor byte [es:expectedFlags], 0x01

	mov cl, [es:inputVal1]
	popf
	inc cl
	pushf

	mov [es:testedResult1], cl
	pop ax
	mov [es:testedFlags], ax
	mov bl, [es:expectedResult1]
	xor cl, bl
	jnz inc8Failed
	mov bx, [es:expectedFlags]
	xor ax, bx
	jnz inc8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

inc8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcInc8Result:
	push bx
	push cx

	mov al, [es:inputVal1]
	lea bx, IncTable
	xlat
	mov [es:expectedResult1], al
	mov cx, 0xF202
	mov bl, 0x80
	xor bl, al
	jnz inc8NoOv
	xor ch, 0x08
inc8NoOv:
	mov bl, al
	and bl, 0x0F
	jnz inc8NoAC
	or cl, 0x10
inc8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx

	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test DEC for all byte values.
;-----------------------------------------------------------------------------
testDec8:
	mov si, testingDec8Str
	call writeString
	mov si, test8InputStr
	call writeString

	mov byte [es:isTesting], 4

	xor cx, cx
testDec8Loop:
	mov [es:inputVal1], cl
	call calcDec8Result
	call testDec8Single
	xor al, 0
	jnz stopDec8Test
continueDec8:
	inc cl
	jnz testDec8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopDec8Test:
	call checkKeyInput
	xor al, 0
	jnz continueDec8
	ret

;-----------------------------------------------------------------------------
testDec8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal1]
	popf
	dec bl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz dec8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz dec8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx
	xor byte [es:expectedFlags], 0x01

	mov cl, [es:inputVal1]
	popf
	dec cl
	pushf

	mov [es:testedResult1], cl
	pop ax
	mov [es:testedFlags], ax
	mov bl, [es:expectedResult1]
	xor cl, bl
	jnz dec8Failed
	mov bx, [es:expectedFlags]
	xor ax, bx
	jnz dec8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

dec8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcDec8Result:
	push bx
	push cx

	mov al, [es:inputVal1]
	lea bx, DecTable
	xlat
	mov [es:expectedResult1], al
	mov cx, 0xF202
	mov bl, 0x7F
	xor bl, al
	jnz dec8NoOv
	xor ch, 0x08
dec8NoOv:
	mov bl, al
	and bl, 0x0F
	xor bl, 0x0F
	jnz dec8NoAC
	or cl, 0x10
dec8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx

	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ADD for all bytes & bytes values.
;-----------------------------------------------------------------------------
testAdd8:
	mov si, testingAdd8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov cx, [es:expectedResult1]
testAdd8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcAdd8Result
	call testAdd8Single
	xor al, 0
	jnz stopAdd8Test
continueAdd8:
	inc word [es:expectedResult1]
	inc cl
	jnz testAdd8Loop
	mov word [es:expectedResult1], 0
	inc ch
	mov [es:expectedResult1], ch
	jnz testAdd8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAdd8Test:
	call checkKeyInput
	xor al, 0
	jnz continueAdd8
	ret

;-----------------------------------------------------------------------------
testAdd8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	add bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz add8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz add8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx

	mov cl, [es:inputVal1]
	mov al, [es:inputVal2]
	popf
	add al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz add8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz add8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

add8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAdd8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	xor bl, al

	mov ax, [es:expectedResult1]
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz add8NoC
	or cx, 0x801
add8NoC:
	test bl, 0x80
	jz add8NoOv
	xor ch, 0x08
add8NoOv:
	test bl, 0x10
	jz add8NoAC
	or cl, 0x10
add8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ADC/ADDC for all bytes & bytes values + carry.
;-----------------------------------------------------------------------------
testAdc8:
	mov si, testingAdc8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov byte [es:inputCarry], 0

	xor cx, cx
	mov [es:expectedResult1], cx
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
testAdc8CLoop:
	mov ax, [es:inputCarry]
	mov [es:expectedResult2], ax
testAdc8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcAdc8Result
	call testAdc8Single
	xor al, 0
	jnz stopAdc8Test
continueAdc8:
	inc word [es:expectedResult2]
	inc cl
	jnz testAdc8Loop
	xor ah, ah
	mov al, ch
	inc al
	mov bl, [es:inputCarry]
	xor bl, 0
	jz adc8NoInpCarry
	inc ax
adc8NoInpCarry:
	mov [es:expectedResult2], ax
	inc ch
	jnz testAdc8Loop
	cmp byte [es:inputCarry], 0
	jnz testAdcEnd
	mov byte [es:inputCarry], 1
	jmp testAdc8CLoop

testAdcEnd:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	mov byte [es:inputCarry], 0
	xor ax, ax
	ret
stopAdc8Test:
	call checkKeyInput
	xor al, 0
	jnz continueAdc8
	mov byte [es:inputCarry], 0
	ret

;-----------------------------------------------------------------------------
testAdc8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputCarry]
	and bl, 1
	or al, bl
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	adc bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz adc8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz adc8Failed

	pushf
	pop bx
	or bx, 0x78FE
	mov al, [es:inputCarry]
	and al, 1
	or bl, al
	push bx
	mov [es:inputFlags], bx

	mov cl, [es:inputVal1]
	mov al, [es:inputVal2]
	popf
	adc al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz adc8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz adc8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

adc8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAdc8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	xor bl, al

	mov ax, [es:expectedResult2]
	mov [es:expectedResult1], al
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz adc8NoC
	or cx, 0x801
adc8NoC:
	test bl, 0x80
	jz adc8NoOv
	xor ch, 0x08
adc8NoOv:
	test bl, 0x10
	jz adc8NoAC
	or cl, 0x10
adc8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test SUB for all bytes & bytes values.
;-----------------------------------------------------------------------------
testSub8:
	mov si, testingSub8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov cx, [es:expectedResult1]
testSub8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcSub8Result
	call testSub8Single
	xor al, 0
	jnz stopSub8Test
continueSub8:
	dec word [es:expectedResult1]
	inc cl
	jnz testSub8Loop
	mov word [es:expectedResult1], 0
	inc ch
	mov [es:expectedResult1], ch
	jnz testSub8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopSub8Test:
	call checkKeyInput
	xor al, 0
	jnz continueSub8
	ret

;-----------------------------------------------------------------------------
testSub8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	sub bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz sub8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz sub8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx

	mov cl, [es:inputVal1]
	mov al, [es:inputVal2]
	popf
	sub al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz sub8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz sub8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

sub8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcSub8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	xor bl, al

	mov ax, [es:expectedResult1]
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz sub8NoC
	or cx, 0x801
sub8NoC:
	test bl, 0x80
	jz sub8NoOv
	xor ch, 0x08
sub8NoOv:
	test bl, 0x10
	jz sub8NoAC
	or cl, 0x10
sub8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test SBB/SUBC for all bytes & bytes values + carry.
;-----------------------------------------------------------------------------
testSbb8:
	mov si, testingSbb8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov byte [es:inputCarry], 0

	xor cx, cx
	mov [es:expectedResult1], cx
testSbb8CLoop:
	mov ax, [es:inputCarry]
	neg ax
	mov [es:expectedResult2], ax
testSbb8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcSbb8Result
	call testSbb8Single
	xor al, 0
	jnz stopSbb8Test
continueSbb8:
	dec word [es:expectedResult2]
	inc cl
	jnz testSbb8Loop
	xor ah, ah
	mov al, ch
	inc al
	mov bl, [es:inputCarry]
	xor bl, 0
	jz sbb8NoInpCarry
	dec ax
sbb8NoInpCarry:
	mov [es:expectedResult2], ax
	inc ch
	jnz testSbb8Loop
	cmp byte [es:inputCarry], 0
	jnz testSbbEnd
	mov byte [es:inputCarry], 1
	jmp testSbb8CLoop

testSbbEnd:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	mov byte [es:inputCarry], 0
	xor ax, ax
	ret
stopSbb8Test:
	call checkKeyInput
	xor al, 0
	jnz continueSbb8
	mov byte [es:inputCarry], 0
	ret

;-----------------------------------------------------------------------------
testSbb8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputCarry]
	and bl, 1
	or al, bl
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	sbb bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz sbb8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz sbb8Failed

	pushf
	pop bx
	or bx, 0x78FE
	mov al, [es:inputCarry]
	and al, 1
	or bl, al
	push bx
	mov [es:inputFlags], bx

	mov cl, [es:inputVal1]
	mov al, [es:inputVal2]
	popf
	sbb al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz sbb8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz sbb8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

sbb8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcSbb8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	xor bl, al

	mov ax, [es:expectedResult2]
	mov [es:expectedResult1], al
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz sbb8NoC
	or cx, 0x801
sbb8NoC:
	test bl, 0x80
	jz sbb8NoOv
	xor ch, 0x08
sbb8NoOv:
	test bl, 0x10
	jz sbb8NoAC
	or cl, 0x10
sbb8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test CMP for all bytes & bytes values.
;-----------------------------------------------------------------------------
testCmp8:
	mov si, testingCmp8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov cx, [es:expectedResult2]
testCmp8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcCmp8Result
	call testCmp8Single
	xor al, 0
	jnz stopCmp8Test
continueCmp8:
	dec word [es:expectedResult2]
	inc cl
	jnz testCmp8Loop
	mov word [es:expectedResult2], 0
	inc ch
	mov [es:expectedResult2], ch
	jnz testCmp8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopCmp8Test:
	call checkKeyInput
	xor al, 0
	jnz continueCmp8
	ret

;-----------------------------------------------------------------------------
testCmp8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	cmp bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz cmp8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz cmp8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx

	mov cl, [es:inputVal1]
	mov al, [es:inputVal2]
	popf
	cmp al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz cmp8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz cmp8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

cmp8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcCmp8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	mov [es:expectedResult1], al
	xor bl, al

	mov ax, [es:expectedResult2]
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz cmp8NoC
	or cx, 0x801
cmp8NoC:
	test bl, 0x80
	jz cmp8NoOv
	xor ch, 0x08
cmp8NoOv:
	test bl, 0x10
	jz cmp8NoAC
	or cl, 0x10
cmp8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test NEG for all byte values.
;-----------------------------------------------------------------------------
testNeg8:
	mov si, testingNeg8Str
	call writeString
	mov si, test8InputStr
	call writeString

	mov byte [es:isTesting], 4

	xor bx, bx
	xor cx, cx
testNeg8Loop:
	mov [es:inputVal1], cl
	mov [es:expectedResult1], bx
	call calcNeg8Result
	call testNeg8Single
	xor al, 0
	jnz stopNeg8Test
continueNeg8:
	dec bx
	inc cl
	jnz testNeg8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopNeg8Test:
	call checkKeyInput
	xor al, 0
	jnz continueNeg8
	ret

;-----------------------------------------------------------------------------
testNeg8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal1]
	popf
	neg bl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz neg8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz neg8Failed

	pushf
	pop bx
	or bx, 0x78FF
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal1]
	popf
	neg al
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz neg8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz neg8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

neg8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcNeg8Result:
	push bx
	push cx

	mov bl, [es:inputVal1]
	mov ax, [es:expectedResult1]
	xor bl, al
	mov cx, 0xF202
	test ah, 1
	jz neg8NoC
	or cx, 0x801
neg8NoC:
	test bl, 0x80
	jz neg8NoOv
	xor ch, 0x08
neg8NoOv:
	test bl, 0x10
	jz neg8NoAC
	or cl, 0x10
neg8NoAC:
	lea bx, PZSTable
	xlat
	or cl, al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ROL for all byte & 5bit values.
;-----------------------------------------------------------------------------
testRol8:
	mov si, testingRol8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRol8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRol8Result
	call testRol8Single
	xor al, 0
	jnz stopRol8Test
continueRol8:
	inc cx
	jnz testRol8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopRol8Test:
	call checkKeyInput
	xor al, 0
	jnz continueRol8
	ret

;-----------------------------------------------------------------------------
testRol8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	mov al, cl
	and al, 0x1F
	jnz rol8Normal
	test bl, 0x80
	jz rol8Normal
	or word [es:expectedFlags], 0x0800
rol8Normal:

	popf
	rol bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz rol8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rol8Failed

	mov cl, [es:inputVal1]
	mov al, cl
	and al, 0xE0
	pushf
	pop bx
	or bx, 0x78FF
	cmp al, 0x20
	jnz rol8NormalC2
	and bx, 0xFFFE
rol8NormalC2:
	cmp al, 0x30
	jnz rol8NormalV2
	and bx, 0xF7FF
rol8NormalV2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	or byte [es:expectedFlags], 0xD4
	mov ah, cl
	and ah, 0x1F
	jnz rol8Normal2
	and word [es:expectedFlags], 0xF7FF
	and bx, 0x0001
	jz rol8NormalC3
	or bx, 0x0800
rol8NormalC3:
	or [es:expectedFlags], bx
	test al, 0x80
	jz rol8Normal2
	xor word [es:expectedFlags], 0x0800
rol8Normal2:
	popf
	rol al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz rol8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rol8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

rol8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcRol8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	and bl, 0x1F
	jz rol8NoOv
rol8Loop:
	add al, al
	jnc rol8NoC
	or al, 0x01
rol8NoC:
	dec bl
	jnz rol8Loop

rol8SetRes:
	mov ah, al
	test ah, 0x01
	jz rol8NoCy
	or cl, 0x01
	xor ah, 0x80
rol8NoCy:
	test ah, 0x80
	jz rol8NoOv
	or ch, 0x08
rol8NoOv:
	mov [es:expectedResult1], al
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ROR for all byte & 5bit values.
;-----------------------------------------------------------------------------
testRor8:
	mov si, testingRor8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRor8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRor8Result
	call testRor8Single
	xor al, 0
	jnz stopRor8Test
continueRor8:
	inc cx
	jnz testRor8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopRor8Test:
	call checkKeyInput
	xor al, 0
	jnz continueRor8
	ret

;-----------------------------------------------------------------------------
testRor8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]

	popf
	ror bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz ror8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz ror8Failed

	mov cl, [es:inputVal1]
	mov al, cl
	and al, 0xE0
	pushf
	pop bx
	or bx, 0x78FF
	cmp al, 0x20
	jnz ror8NormalC2
	and bx, 0xFFFE
ror8NormalC2:
	cmp al, 0x30
	jnz ror8NormalV2
	and bx, 0xF7FF
ror8NormalV2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	or byte [es:expectedFlags], 0xD4
	mov ah, cl
	and ah, 0x1F
	jnz ror8Normal2
	and word [es:expectedFlags], 0xF7FF
	and bx, 0x0001
	or [es:expectedFlags], bx
	test al, 0x40
	jz ror8NormalV3
	xor word [es:expectedFlags], 0x0800
ror8NormalV3:
	test al, 0x80
	jz ror8Normal2
	xor word [es:expectedFlags], 0x0800
ror8Normal2:
	popf
	ror al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz ror8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz ror8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

ror8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcRor8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	mov ah, al
	and bl, 0x1F
	jz ror8NoCy
	and bl, 0x07
	jz ror8SetRes
	neg bl
	and bl, 0x07
ror8Loop:
	add ax, ax
	jnc ror8NoC
	or al, 0x01
ror8NoC:
	dec bl
	jnz ror8Loop

ror8SetRes:
	test al, 0x80
	jz ror8NoCy
	or cl, 0x01
ror8NoCy:
	test ah, 0x40
	jz ror8NoOv
	or ch, 0x08
ror8NoOv:
	test ah, 0x80
	jz ror8NoOv2
	xor ch, 0x08
ror8NoOv2:
	mov [es:expectedResult1], ah
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test RCL/ROLC for all byte & 5bit values.
;-----------------------------------------------------------------------------
testRcl8:
	mov si, testingRcl8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRcl8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRcl8Result
	call testRcl8Single
	xor al, 0
	jnz stopRcl8Test
continueRcl8:
	inc cx
	jnz testRcl8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopRcl8Test:
	call checkKeyInput
	xor al, 0
	jnz continueRcl8
	ret

;-----------------------------------------------------------------------------
testRcl8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov cl, [es:inputVal1]
	test cl, 0x80
	jz rcl8NormalC1
	or al, 0x01
rcl8NormalC1:
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal2]

	popf
	rcl bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz rcl8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rcl8Failed

	pushf
	pop bx
	or bx, 0x78FF
	mov cl, [es:inputVal1]
	test cl, 0x80
	jnz rcl8NormalC2
	and bl, 0xFE
rcl8NormalC2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	or byte [es:expectedFlags], 0xD4
	popf
	rcl al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz rcl8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rcl8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

rcl8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcRcl8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov ah, [es:inputVal2]
	xor al, al
	test bl, 0x80
	jz rcl8NoC1
	or al, 0x80
rcl8NoC1:
	and bl, 0x1F
	jz rcl8SetRes
rcl8Loop:
	add ax, ax
	jnc rcl8NoC
	or al, 0x80
rcl8NoC:
	dec bl
	jnz rcl8Loop

rcl8SetRes:
	test al, 0x80
	jz rcl8NoCy
	or cl, 0x01
	or ch, 0x08
rcl8NoCy:
	test ah, 0x80
	jz rcl8NoOv
	xor ch, 0x08
rcl8NoOv:
	mov [es:expectedResult1], ah
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test RCR/RORC for all byte & 5bit values.
;-----------------------------------------------------------------------------
testRcr8:
	mov si, testingRcr8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRcr8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRcr8Result
	call testRcr8Single
	xor al, 0
	jnz stopRcr8Test
continueRcr8:
	inc cx
	jnz testRcr8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopRcr8Test:
	call checkKeyInput
	xor al, 0
	jnz continueRcr8
	ret

;-----------------------------------------------------------------------------
testRcr8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov cl, [es:inputVal1]
	test cl, 0x80
	jz rcr8NormalC1
	or al, 0x01
rcr8NormalC1:
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal2]

	popf
	rcr bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz rcr8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rcr8Failed

	pushf
	pop bx
	or bx, 0x78FF
	mov cl, [es:inputVal1]
	test cl, 0x80
	jnz rcr8NormalC2
	and bl, 0xFE
rcr8NormalC2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	or byte [es:expectedFlags], 0xD4
	popf
	rcr al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz rcr8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz rcr8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

rcr8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcRcr8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov ah, [es:inputVal2]
	xor al, al
	test bl, 0x80
	jz rcr8NoC1
	or al, 0x80
rcr8NoC1:
	and bl, 0x1F
	jz rcr8SetRes
	mov bh, 9*4
	sub bh, bl
rcr8Loop:
	add ax, ax
	jnc rcr8NoC
	or al, 0x80
rcr8NoC:
	dec bh
	jnz rcr8Loop

rcr8SetRes:
	test al, 0x80
	jz rcr8NoCy
	or cl, 0x01
rcr8NoCy:
	test ah, 0x40
	jz rcr8NoOv
	or ch, 0x08
rcr8NoOv:
	test ah, 0x80
	jz rcr8NoOv2
	xor ch, 0x08
rcr8NoOv2:
	mov [es:expectedResult1], ah
	mov [es:expectedFlags], cx
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test SHL for all byte & 5bit values.
;-----------------------------------------------------------------------------
testShl8:
	mov si, testingShl8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testShl8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcShl8Result
	call testShl8Single
	xor al, 0
	jnz stopShl8Test
continueShl8:
	inc cx
	jnz testShl8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopShl8Test:
	call checkKeyInput
	xor al, 0
	jnz continueShl8
	ret

;-----------------------------------------------------------------------------
testShl8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]
	mov al, cl
	and al, 0x1F
	jnz shl8Normal
	test bl, 0x80
	jz shl8Normal
	or word [es:expectedFlags], 0x0800
shl8Normal:

	popf
	shl bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz shl8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz shl8Failed

	mov cl, [es:inputVal1]
	mov al, cl
	and al, 0xE0
	pushf
	pop bx
	or bx, 0x78FF
	cmp al, 0x20
	jnz shl8NormalC2
	and bx, 0xFFFE
shl8NormalC2:
	cmp al, 0x30
	jnz shl8NormalV2
	and bx, 0xF7FF
shl8NormalV2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	mov ah, cl
	and ah, 0x1F
	jnz shl8Normal2
	and word [es:expectedFlags], 0xF7FF
	and bx, 0x0001
	jz shl8NormalC3
	or bx, 0x0800
shl8NormalC3:
	or [es:expectedFlags], bx
	test al, 0x80
	jz shl8Normal2
	xor word [es:expectedFlags], 0x0800
shl8Normal2:
	popf
	shl al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz shl8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz shl8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

shl8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcShl8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	and bl, 0x1F
	jz shl8NoOv
shl8Loop:
	xor ah, ah
	add al, al
	jnc shl8NoC
	mov ah, 0x01
shl8NoC:
	dec bl
	jnz shl8Loop

shl8SetRes:
	or ah, al
	test ah, 0x01
	jz shl8NoCy
	or cl, 0x01
	xor ah, 0x80
shl8NoCy:
	test ah, 0x80
	jz shl8NoOv
	or ch, 0x08
shl8NoOv:
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or ax, cx
	mov [es:expectedFlags], ax
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test SHR for all byte & 5bit values.
;-----------------------------------------------------------------------------
testShr8:
	mov si, testingShr8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testShr8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcShr8Result
	call testShr8Single
	xor al, 0
	jnz stopShr8Test
continueShr8:
	inc cx
	jnz testShr8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopShr8Test:
	call checkKeyInput
	xor al, 0
	jnz continueShr8
	ret

;-----------------------------------------------------------------------------
testShr8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]

	popf
	shr bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz shr8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz shr8Failed

	mov cl, [es:inputVal1]
	mov al, cl
	and al, 0xE0
	pushf
	pop bx
	or bx, 0x78FF
	cmp al, 0x20
	jnz shr8NormalC2
	and bx, 0xFFFE
shr8NormalC2:
	cmp al, 0x30
	jnz shr8NormalV2
	and bx, 0xF7FF
shr8NormalV2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	mov ah, cl
	and ah, 0x1F
	jnz shr8Normal2
	and bx, 0x0001
	or [es:expectedFlags], bx
shr8Normal2:
	popf
	shr al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz shr8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz shr8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

shr8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcShr8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	mov ah, al
	and bl, 0x1F
	jz shr8NoCy
	xor ah, ah
	cmp bl, 8
	jz shr8SetRes
	jnc shr8NoOv2
	neg bl
	and bl, 0x07
shr8Loop:
	add ax, ax
	dec bl
	jnz shr8Loop

shr8SetRes:
	test al, 0x80
	jz shr8NoCy
	or cl, 0x01
shr8NoCy:
	test ah, 0x40
	jz shr8NoOv
	or ch, 0x08
shr8NoOv:
	test ah, 0x80
	jz shr8NoOv2
	xor ch, 0x08
shr8NoOv2:
	mov al, ah
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or ax, cx
	mov [es:expectedFlags], ax
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test SAR/SHRA for all byte & 5bit values.
;-----------------------------------------------------------------------------
testSar8:
	mov si, testingSar8Str
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testSar8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcSar8Result
	call testSar8Single
	xor al, 0
	jnz stopSar8Test
continueSar8:
	inc cx
	jnz testSar8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopSar8Test:
	call checkKeyInput
	xor al, 0
	jnz continueSar8
	ret

;-----------------------------------------------------------------------------
testSar8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov bl, [es:inputVal2]

	popf
	sar bl, cl
	pushf

	mov [es:testedResult1], bl
	pop cx
	mov [es:testedFlags], cx
	mov al, [es:expectedResult1]
	xor al, bl
	jnz sar8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz sar8Failed

	mov cl, [es:inputVal1]
	mov al, cl
	and al, 0xE0
	pushf
	pop bx
	or bx, 0x78FF
	cmp al, 0x20
	jnz sar8NormalC2
	and bx, 0xFFFE
sar8NormalC2:
	cmp al, 0x30
	jnz sar8NormalV2
	and bx, 0xF7FF
sar8NormalV2:
	push bx
	mov [es:inputFlags], bx

	mov al, [es:inputVal2]
	mov ah, cl
	and ah, 0x1F
	jnz sar8Normal2
	and bx, 0x0001
	or [es:expectedFlags], bx
sar8Normal2:
	popf
	sar al, cl
	pushf

	mov [es:testedResult1], al
	pop cx
	mov [es:testedFlags], cx
	mov bl, [es:expectedResult1]
	xor al, bl
	jnz sar8Failed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz sar8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

sar8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcSar8Result:
	push bx
	push cx

	mov cx, 0xF202
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	mov ah, al
	and bl, 0x1F
	jz sar8NoCy
	cbw 
	cmp bl, 8
	jnc sar8SetRes
	neg bl
	and bl, 0x07
sar8Loop:
	add ax, ax
	dec bl
	jnz sar8Loop

sar8SetRes:
	test al, 0x80
	jz sar8NoCy
	or cl, 0x01
sar8NoCy:
	test ah, 0x40
	jz sar8NoOv
	or ch, 0x08
sar8NoOv:
	test ah, 0x80
	jz sar8NoOv2
	xor ch, 0x08
sar8NoOv2:
	mov al, ah
	mov [es:expectedResult1], al
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or ax, cx
	mov [es:expectedFlags], ax
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test unsigned multiplication of all byte values.
;-----------------------------------------------------------------------------
testMulu8:
	mov si, testingMuluStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov [es:inputVal1], cx
	mov [es:inputVal2], cx
testMuluLoop2:
	mov [es:inputVal2], ch
	xor bx, bx
	mov [es:expectedResult1], bx
testMuluLoop:
	mov [es:inputVal1], cl
	mov ax, 0xF242
	mov bx, [es:expectedResult1]
	xor bh, 0
	jz noMuluOverflow
	or ax, 0x0801
noMuluOverflow:
	mov [es:expectedFlags], ax
	call testMulu8Single
	xor al, 0
	jnz stopMuluTest
continueMulu:
	xor bx, bx
	mov bl, ch
	add [es:expectedResult1], bx
	inc cl
	jnz testMuluLoop
	inc ch
	jnz testMuluLoop2

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopMuluTest:
	call checkKeyInput
	xor al, 0
	jnz continueMulu
	ret

;-----------------------------------------------------------------------------
testMulu8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	mul bl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz muluFailed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz muluFailed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	mul cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	xor ax, cx
	jnz muluFailed
	mov cx, [es:expectedFlags]
	xor bx, cx
	jnz muluFailed

	xor ax, ax
	pop cx
	pop bx
	ret

muluFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test signed multiplication of all byte values.
;-----------------------------------------------------------------------------
testMuls8:
	mov si, testingMulsStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
	mov [es:inputVal2], cx
testMulsLoop:
	mov bx, [es:expectedResult1]
	mov al, ch
	cbw
	add bx, ax
	cmp cl, 0x80
	jnz noNeg
	neg bx
noNeg:
	mov [es:inputVal1], cl
	xor cl, 0
	jnz skipMulsVal2
	xor bx, bx
	mov [es:inputVal2], ch
skipMulsVal2:
	mov [es:expectedResult1], bx
	mov ax, 0xF242
	sar bx, 7
	jz noMulsOverflow
	not bx
	xor bx, 0
	jz noMulsOverflow
	or ax, 0x0801
noMulsOverflow:
	mov [es:expectedFlags], ax
	call testMuls8Single
	xor al, 0
	jnz stopMulsTest
continueMuls:
	inc cx
	jnz testMulsLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	ret
stopMulsTest:
	call checkKeyInput
	xor al, 0
	jnz continueMuls
	ret

;-----------------------------------------------------------------------------
testMuls8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	imul bl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz mulsFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz mulsFailed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	imul cl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz mulsFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz mulsFailed

	xor ax, ax
	pop cx
	pop bx
	ret

mulsFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test unsigned multiplication + addition of all word & byte values.
;-----------------------------------------------------------------------------
testAad:
	mov si, testingAadStr
	call writeString
	mov si, test16x8InputStr
	call writeString

	mov byte [es:isTesting], 2
	mov byte [es:selfModifyingCode], 0xD5	; AAD
	mov byte [es:selfModifyingCode+2], 0xCB	; RETF

	xor cx, cx
	xor dx, dx
testAadLoop2:
	mov byte [es:expectedResult2], 0
	mov [es:inputVal1], dl
	mov [es:selfModifyingCode+1], dl	; multiplicand
testAadLoop:
	mov [es:inputVal2], cx
	call calcAadResult
	call testAadSingle
	xor al, 0
	jnz stopAadTest
continueAad:
	inc cl
	jnz testAadLoop
	add [es:expectedResult2], dl
	inc ch
	jnz testAadLoop
	inc dl
	jnz testAadLoop2

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAadTest:
	call checkKeyInput
	xor al, 0
	jnz continueAad
	ret

;-----------------------------------------------------------------------------
testAadSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov ax, [es:inputVal2]
	popf
	call 0x0000:selfModifyingCode
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aadFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aadFailed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov ax, [es:inputVal2]
	popf
	call 0x0000:selfModifyingCode
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aadFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aadFailed

	xor ax, ax
	pop cx
	pop bx
	ret

aadFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAadResult:
	mov ah, [es:inputVal2]
	mov al, [es:expectedResult2]
	add al, ah
	pushf
	xor ah, ah
	mov [es:expectedResult1], ax
	pop ax							; All flags are from the last add.
	mov [es:expectedFlags], ax
	ret

;-----------------------------------------------------------------------------
; Test unsigned division of all word/byte values.
;-----------------------------------------------------------------------------
testDivu8:
	mov si, testingDivuStr
	call writeString
	mov si, test16x8InputStr
	call writeString

	mov byte [es:isTesting], 2

	xor cx, cx
	xor dx, dx
testDivu8Loop:
	mov [es:inputVal1], dl
	mov [es:inputVal2], cx
	call calcDivu8Result
	call testDivu8Single
	xor al, 0
	jnz stopDivu8Test
continueDivu8:
	inc cx
	jnz testDivu8Loop
	inc dl
	jnz testDivu8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopDivu8Test:
	call checkKeyInput
	xor al, 0
	jnz continueDivu8
	ret

;-----------------------------------------------------------------------------
testDivu8Single:
	push bx
	push cx
	push dx

	mov byte [es:testedException], 0
	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	div bl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz divu8Failed
	mov al, [es:testedException]
	mov bx, [es:expectedFlags]
	xor cx, bx
	cmp al, 0
	jz divu8DoZTst
	and cx, 0xFFBF				; Mask out Zero flag
divu8DoZTst:
	cmp cx, 0
	jnz divu8Failed
	mov bl, [es:expectedException]
	xor al, bl
	jnz divu8Failed

	mov byte [es:testedException], 0
	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	div cl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz divu8Failed
	mov al, [es:testedException]
	mov bx, [es:expectedFlags]
	xor cx, bx
	cmp al, 0
	jz divu8DoZTst2
	and cx, 0xFFBF				; Mask out Zero flag
divu8DoZTst2:
	cmp cx, 0
	jnz divu8Failed
	mov bl, [es:expectedException]
	xor al, bl
	jnz divu8Failed

	pop dx
	pop cx
	pop bx
	xor ax, ax
	ret

divu8Failed:
	call printFailedResult
	pop dx
	pop cx
	pop bx
	mov ax, 1
	ret

;-----------------------------------------------------------------------------
calcDivu8Result:
	push bx
	push cx
	push dx

	mov byte [es:expectedException], 0
	xor bx, bx
	xor cx, cx
	mov dx, 0xF202				; Expected flags
	call getLFSR1Value
	and al, 0x10
	imul al
	jnc divu8NoCV
	or dx, 0x0801				; Carry & Overflow
divu8NoCV:

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	mov [es:expectedResult1], ax
	cmp ah, bl
	jnc divu8Error
	cmp ax, 0
	jz divu8Done
divu8Loop:
	sub ax, bx
	jc divu8SetRes
	inc cl
	jmp divu8Loop

divu8SetRes:
	add ax, bx
	mov ah, al
	mov al, cl
	mov [es:expectedResult1], ax
divu8SetZ:
	cmp ah, 0
	jnz divu8Done
	test al, 1
	jz divu8Done
	or dl, 0x40
divu8Done:
	mov [es:expectedFlags], dx
	pop dx
	pop cx
	pop bx
	ret
divu8Error:
	mov byte [es:expectedException], 1
	jmp divu8Done
;-----------------------------------------------------------------------------
; Test signed division of all word/byte values.
;-----------------------------------------------------------------------------
testDivs8:
	mov si, testingDivsStr
	call writeString
	mov si, test16x8InputStr
	call writeString

	mov byte [es:isTesting], 2

	xor cx, cx
	xor dx, dx
testDivs8Loop:
	mov [es:inputVal1], dl
	mov [es:inputVal2], cx
	call calcDivs8Result
	call testDivs8Single
	xor al, 0
	jnz stopDivs8Test
continue8Divs:
	mov byte [es:isTesting], 2
	inc cx
	jnz testDivs8Loop
	inc dl
	jnz testDivs8Loop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopDivs8Test:
	call checkKeyInput
	xor al, 0
	jnz continue8Divs
	ret

;-----------------------------------------------------------------------------
testDivs8Single:
	push bx
	push cx

	mov byte [es:testedException], 0
	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	idiv bl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz divs8Failed
	mov al, [es:testedException]
	mov bx, [es:expectedFlags]
	xor cx, bx
	cmp al, 0
	jz divs8DoZTst
	and cx, 0xFFBF				; Mask out Zero flag
divs8DoZTst:
	cmp cx, 0
	jnz divs8Failed
	mov bl, [es:expectedException]
	xor al, bl
	jnz divs8Failed

	mov byte [es:testedException], 0
	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov cl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	idiv cl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz divs8Failed
	mov al, [es:testedException]
	mov bx, [es:expectedFlags]
	xor cx, bx
	cmp al, 0
	jz divs8DoZTst2
	and cx, 0xFFBF				; Mask out Zero flag
divs8DoZTst2:
	cmp cx, 0
	jnz divs8Failed
	mov bl, [es:expectedException]
	xor al, bl
	jnz divs8Failed

	xor ax, ax
	pop cx
	pop bx
	ret

divs8Failed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
calcDivs8Result:
	push bx
	push cx
	push dx

	mov byte [es:expectedException], 0
	mov al, [es:inputVal1]
	cbw
	mov bx, ax
	mov ax, [es:inputVal2]
	mov [es:expectedResult1], ax
	mov dl, ah
	mov dh, ah
	xor dh, bh
	cmp bx, 0
	jz divs8Error
	jns den8Pos
	neg bx
den8Pos:
	cmp ax, 0
	jz divs8Done
	jns enum8Pos
	neg ax
enum8Pos:
	mov cx, ax
	shr cx, 7
	cmp cx, bx
	jnc divs8ErrCnt
	xor cx, cx
divs8Loop:
	sub ax, bx
	jc divs8SetRes
	inc cl
	jmp divs8Loop

divs8SetRes:
	add ax, bx
	cmp dh, 0
	jns result8Pos
	neg cl
result8Pos:
	cmp dl, 0
	jns rest8Pos
	neg al
rest8Pos:
	mov ah, al
	mov al, cl
	mov [es:expectedResult1], ax
divs8Done:
	mov dx, 0xF202				; Expected flags
	lea bx, PZSTable
	xlat						; Fetch Sign, Zero & Parity
	or dl, al
divs8End:
	mov [es:expectedFlags], dx
	pop dx
	pop cx
	pop bx
	ret
divs8Error:
	cmp ax, 0x8000
	jnz divs8ErrCnt
	mov ax, 0x0081
	mov [es:expectedResult1], ax
	jmp divs8Done
divs8ErrCnt:
	mov byte [es:expectedException], 1
	mov dx, 0xF202				; Expected flags
	call getLFSR1Value
	and al, 0x10
	mov al, 0x10
	imul al
	jnc divs8NoCV
	or dx, 0x0801				; Carry & Overflow
divs8NoCV:
	jmp divs8End
;-----------------------------------------------------------------------------
; Test unsigned division of all byte/byte values.
;-----------------------------------------------------------------------------
testAam:
	mov si, testingAamStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1
	mov byte [es:selfModifyingCode], 0xD4	; AAM
	mov byte [es:selfModifyingCode+2], 0xCB	; RETF

	xor cx, cx
testAamLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcAamResult
	call testAamSingle
	xor al, 0
	jnz stopAamTest
continueAam:
	inc cx
	jnz testAamLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAamTest:
	call checkKeyInput
	xor al, 0
	jnz continueAam
	ret

;-----------------------------------------------------------------------------
testAamSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	push ax
	mov [es:inputFlags], ax

	mov byte [es:testedException], 0
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	mov ah, al
	xor ah, 0xa5
	mov [es:selfModifyingCode+1], bl	; dividend

	popf
	call 0x0000:selfModifyingCode
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aamFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aamFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	xor al, bl
	jnz aamFailed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	mov byte [es:testedException], 0
	mov al, [es:inputVal2]
	mov ah, al
	xor ah, 0xa5
	popf
	call 0x0000:selfModifyingCode
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aamFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aamFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	xor al, bl
	jnz aamFailed

	xor ax, ax
	pop cx
	pop bx
	ret

aamFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAamResult:
	push bx
	push dx

	mov byte [es:expectedException], 0
	mov bl, [es:inputVal1]
	mov al, [es:inputVal2]
	mov ah, al
	xor ah, 0xa5
	mov [es:expectedResult1], ax
	xor bl, 0
	jz aamError
	xor ah, ah
aamLoop:
	sub al, bl
	jc aamSetRes
	inc ah
	jmp aamLoop

aamSetRes:
	add al, bl
	mov [es:expectedResult1], ax
	mov dx, 0xF202				; Expected flags
	lea bx, PZSTable
	xlat				; Fetch Sign, Zero & Parity
	or dl, al
aamDone:
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

aamError:
	mov dx, 0xF202				; Expected flags
	test al, 0xc0
	jnz aamErrNoZ
	or dl, 0x40			; Zero flag
aamErrNoZ:
	call getLFSR1Value
	and al, 0x10
	mov bl, al
	mul bl
	jnc aamNoCV
	or dx, 0x0801				; Carry & Overflow
aamNoCV:
	mov byte [es:expectedException], 1
	jmp aamDone

;-----------------------------------------------------------------------------
; Test Decimal Adjust after Addition of all byte values & AC + CY.
;-----------------------------------------------------------------------------
testDaa:
	mov si, testingDaaStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testDaaLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcDaaResult
	call testDaaSingle
	xor al, 0
	jnz stopDaaTest
continueDaa:
	inc cx
	cmp cx, 0x400
	jnz testDaaLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopDaaTest:
	call checkKeyInput
	xor al, 0
	jnz continueDaa
	ret

;-----------------------------------------------------------------------------
testDaaSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputVal2]
	test bl, 1
	jz daaTestNoCY
	or al, 0x01
daaTestNoCY:
	test bl, 2
	jz daaTestNoAC
	or al, 0x10
daaTestNoAC:
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov ah, al
	xor ah, 0xa5

	popf
	daa
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz daaFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz daaFailed

	pushf
	pop ax
	xor al, al
	or ax, 0x78EE
	mov bl, [es:inputVal2]
	test bl, 1
	jz daaTest2NoCY
	or al, 0x01
daaTest2NoCY:
	test bl, 2
	jz daaTest2NoAC
	or al, 0x10
daaTest2NoAC:
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov ah, al
	xor ah, 0xa5
	popf
	daa
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz daaFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz daaFailed

	xor ax, ax
	pop cx
	pop bx
	ret

daaFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcDaaResult:
	push bx
	push dx

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	mov ah, al
	mov bh, al
	shl bh, 4

	cmp al, 0x9A
	jc daaNoCY
	or bl, 1
daaNoCY:
	test bl, 1
	jz daaNoHighAdd
	add al, 0x60
daaNoHighAdd:
	cmp bh, 0xA0
	jc daaNoAC
	or bl, 2
daaNoAC:
	test bl, 2
	jz daaNoLowAdd
	add al, 0x06
daaNoLowAdd:
daaSetRes:
	mov dx, 0xF202				; Expected flags
	test bl, 1
	jz daaSkipCY
	or dl, 0x01
daaSkipCY:
	test bl, 2
	jz daaSkipAC
	or dl, 0x10
daaSkipAC:
	cmp al, ah
	jno daaSkipOV
	or dx, 0x800
daaSkipOV:
	xor ah, 0xA5
	mov [es:expectedResult1], ax
	lea bx, PZSTable
	xlat				; Fetch Sign, Zero & Parity
	or dl, al
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test Decimal Adjust after Subtraction of all byte values & AC + CY.
;-----------------------------------------------------------------------------
testDas:
	mov si, testingDasStr
	call writeString
	mov si, test8x8InputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testDasLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcDasResult
	call testDasSingle
	xor al, 0
	jnz stopDasTest
continueDas:
	inc cx
	cmp cx, 0x400
	jnz testDasLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopDasTest:
	call checkKeyInput
	xor al, 0
	jnz continueDas
	ret

;-----------------------------------------------------------------------------
testDasSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputVal2]
	test bl, 1
	jz dasTestNoCY
	or al, 0x01
dasTestNoCY:
	test bl, 2
	jz dasTestNoAC
	or al, 0x10
dasTestNoAC:
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov ah, al
	xor ah, 0xA5

	popf
	das
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz dasFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz dasFailed

	pushf
	pop ax
	xor al, al
	or ax, 0x78EE
	mov bl, [es:inputVal2]
	test bl, 1
	jz dasTest2NoCY
	or al, 0x01
dasTest2NoCY:
	test bl, 2
	jz dasTest2NoAC
	or al, 0x10
dasTest2NoAC:
	push ax
	mov [es:inputFlags], ax

	mov al, [es:inputVal1]
	mov ah, al
	xor ah, 0xA5
	popf
	das
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz dasFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz dasFailed

	xor ax, ax
	pop cx
	pop bx
	ret

dasFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcDasResult:
	push bx
	push dx

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	mov ah, al
	mov bh, al
	shl bh, 4

	cmp al, 0x9A
	jc dasNoCY
	or bl, 1
dasNoCY:
	test bl, 1
	jz dasNoHighSub
	sub al, 0x60
dasNoHighSub:
	cmp bh, 0xA0
	jc dasNoAC
	or bl, 2
dasNoAC:
	test bl, 2
	jz dasNoLowSub
	sub al, 0x06
dasNoLowSub:
dasSetRes:
	mov dx, 0xF202				; Expected flags
	test bl, 1
	jz dasSkipCY
	or dl, 0x01
dasSkipCY:
	test bl, 2
	jz dasSkipAC
	or dl, 0x10
dasSkipAC:
	cmp al, ah
	jno dasSkipOV
	or dx, 0x800
dasSkipOV:
	xor ah, 0xA5
	mov [es:expectedResult1], ax
	lea bx, PZSTable
	xlat				; Fetch Sign, Zero & Parity
	or dl, al
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ASCII Adjust After Addition of all word values & AC.
;-----------------------------------------------------------------------------
testAaa:
	mov si, testingAaaStr
	call writeString
	mov si, test16x8InputStr
	call writeString

	mov byte [es:isTesting], 2
	xor bl, bl

testAaaLoop:
	mov [es:inputVal1], bl
	xor cx, cx
testAaaLoop2:
	mov [es:inputVal2], cx
	call calcAaaResult
	call testAaaSingle
	xor al, 0
	jnz stopAaaTest
continueAaa:
	inc cx
	jnz testAaaLoop2
	mov bl, [es:inputVal1]
	inc bl
	cmp bl, 2
	jnz testAaaLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAaaTest:
	call checkKeyInput
	xor al, 0
	jnz continueAaa
	ret

;-----------------------------------------------------------------------------
testAaaSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputVal1]
	test bl, 1
	jz aaaTestNoAC
	or al, 0x10
aaaTestNoAC:
	push ax
	mov [es:inputFlags], ax

	mov ax, [es:inputVal2]
	popf
	aaa
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aaaFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aaaFailed

	pushf
	pop ax
	xor al, al
	or ax, 0x78EF
	mov bl, [es:inputVal1]
	test bl, 1
	jz aaaTest2NoAC
	or al, 0x10
aaaTest2NoAC:
	push ax
	mov [es:inputFlags], ax

	mov ax, [es:inputVal2]
	popf
	aaa
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aaaFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aaaFailed

	xor ax, ax
	pop cx
	pop bx
	ret

aaaFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAaaResult:
	push bx
	push dx

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	mov dx, 0xF206				; Expected flags

	and al, 0x0F
	cmp al, 0x0A
	jc aaaNoCY
	or bl, 1
aaaNoCY:
	test bl, 1
	jz aaaSkipCY
	add al, 0x06
	inc ah
	and al, 0x0F
	or dl, 0x51
	jmp aaaSetRes
aaaSkipCY:
	or dl, 0x80
aaaSetRes:
	mov [es:expectedResult1], ax
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test ASCII Adjust After Subtraction of all word values & AC.
;-----------------------------------------------------------------------------
testAas:
	mov si, testingAasStr
	call writeString
	mov si, test16x8InputStr
	call writeString

	mov byte [es:isTesting], 2
	xor bl, bl

testAasLoop:
	mov [es:inputVal1], bl
	xor cx, cx
testAasLoop2:
	mov [es:inputVal2], cx
	call calcAasResult
	call testAasSingle
	xor al, 0
	jnz stopAasTest
continueAas:
	inc cx
	jnz testAasLoop2
	mov bl, [es:inputVal1]
	inc bl
	cmp bl, 2
	jnz testAasLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopAasTest:
	call checkKeyInput
	xor al, 0
	jnz continueAas
	ret

;-----------------------------------------------------------------------------
testAasSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bl, [es:inputVal1]
	test bl, 1
	jz aasTestNoAC
	or al, 0x10
aasTestNoAC:
	push ax
	mov [es:inputFlags], ax

	mov ax, [es:inputVal2]
	popf
	aas
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aasFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aasFailed

	pushf
	pop ax
	xor al, al
	or ax, 0x78EF
	mov bl, [es:inputVal1]
	test bl, 1
	jz aasTest2NoAC
	or al, 0x10
aasTest2NoAC:
	push ax
	mov [es:inputFlags], ax

	mov ax, [es:inputVal2]
	popf
	aas
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	xor ax, bx
	jnz aasFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aasFailed

	xor ax, ax
	pop cx
	pop bx
	ret

aasFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret
;-----------------------------------------------------------------------------
calcAasResult:
	push bx
	push dx

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	mov dx, 0xf206				; Expected flags

	and al, 0x0F
	cmp al, 0x0A
	jc aasNoCY
	or bl, 1
aasNoCY:
	test bl, 1
	jz aasSkipCY
	sub al, 0x06
	dec ah
	and al, 0x0F
	or dl, 0x51
	jmp aasSetRes
aasSkipCY:
	or dl, 0x80
aasSetRes:
	mov [es:expectedResult1], ax
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test all conditional JMP/BRA with all flags.
;-----------------------------------------------------------------------------
testJmp:
	mov si, testingJmpStr
	call writeString
	mov si, test16InputStr
	call writeString

	mov byte [es:isTesting], 2

	xor cx, cx
testJmpLoop:
	mov [es:inputVal1], cx
	call calcJmpFlags
	call testJmpSingle
	xor al, 0
	jnz stopJmpTest
continueJmp:
	inc cx
	cmp ch, 0x04
	jnz testJmpLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
	ret
stopJmpTest:
	call checkKeyInput
	xor al, 0
	jnz continueJmp
	ret

;-----------------------------------------------------------------------------
testJmpSingle:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0x8700
	mov bx, [es:inputFlags]
	or ax, bx
	push ax					; Push flags for test.
	mov bx, [es:inputVal1]
	shr bx, 6
	cmp bl, 0
	jz jmpTestJo
	cmp bl, 1
	jz jmpTestJno
	cmp bl, 2
	jz jmpTestJb
	cmp bl, 3
	jz jmpTestJnb
	cmp bl, 4
	jz jmpTestJz
	cmp bl, 5
	jz jmpTestJnz
	cmp bl, 6
	jz jmpTestJbe
	cmp bl, 7
	jz jmpTestJnbe
	cmp bl, 8
	jz jmpTestJs
	cmp bl, 9
	jz jmpTestJns
	cmp bl, 10
	jz jmpTestJp
	cmp bl, 11
	jz jmpTestJnp
	cmp bl, 12
	jz jmpTestJl
	cmp bl, 13
	jz jmpTestJnl
	cmp bl, 14
	jz jmpTestJle
	jmp jmpTestJnle

;-------------------------------------
jmpTestJo:
	test ah, 0x08
	jz jmpTestJoNoO
	popf
	jo jmpJoOk
jmpJoFailed:
	mov si, joFailedStr
	jmp jmpFailed
jmpTestJoNoO:
	popf
	jo jmpJoFailed
jmpJoOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJno:
	test ah, 0x08
	jnz jmpTestJnoO
	popf
	jno jmpJnoOk
jmpJnoFailed:
	mov si, jnoFailedStr
	jmp jmpFailed
jmpTestJnoO:
	popf
	jno jmpJnoFailed
jmpJnoOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJb:
	test al, 0x01
	jz jmpTestJbNoC
	popf
	jb jmpJbOk
jmpJbFailed:
	mov si, jbFailedStr
	jmp jmpFailed
jmpTestJbNoC:
	popf
	jb jmpJbFailed
jmpJbOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJnb:
	test al, 0x01
	jnz jmpTestJnbC
	popf
	jnb jmpJnbOk
jmpJnbFailed:
	mov si, jnbFailedStr
	jmp jmpFailed
jmpTestJnbC:
	popf
	jnb jmpJnbFailed
jmpJnbOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJz:
	test al, 0x40
	jz jmpTestJzNoZ
	popf
	jz jmpJzOk
jmpJzFailed:
	mov si, jzFailedStr
	jmp jmpFailed
jmpTestJzNoZ:
	popf
	jz jmpJzFailed
jmpJzOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJnz:
	test al, 0x40
	jnz jmpTestJnzZ
	popf
	jnz jmpJnzOk
jmpJnzFailed:
	mov si, jnzFailedStr
	jmp jmpFailed
jmpTestJnzZ:
	popf
	jnz jmpJnzFailed
jmpJnzOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJbe:
	test al, 0x41
	jz jmpTestJbeNoZ
	popf
	jbe jmpJbeOk
jmpJbeFailed:
	mov si, jbeFailedStr
	jmp jmpFailed
jmpTestJbeNoZ:
	popf
	jbe jmpJbeFailed
jmpJbeOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJnbe:
	test al, 0x41
	jnz jmpTestJnbeZ
	popf
	jnbe jmpJnbeOk
jmpJnbeFailed:
	mov si, jnbeFailedStr
	jmp jmpFailed
jmpTestJnbeZ:
	popf
	jnbe jmpJnbeFailed
jmpJnbeOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJs:
	test al, 0x80
	jz jmpTestJsNoS
	popf
	js jmpJnbeOk
jmpJsFailed:
	mov si, jsFailedStr
	jmp jmpFailed
jmpTestJsNoS:
	popf
	js jmpJsFailed
jmpJsOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJns:
	test al, 0x80
	jnz jmpTestJnsS
	popf
	jns jmpJnsOk
jmpJnsFailed:
	mov si, jnsFailedStr
	jmp jmpFailed
jmpTestJnsS:
	popf
	jns jmpJnsFailed
jmpJnsOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJp:
	test al, 0x04
	jz jmpTestJpNoP
	popf
	jp jmpJpOk
jmpJpFailed:
	mov si, jpFailedStr
	jmp jmpFailed
jmpTestJpNoP:
	popf
	jp jmpJpFailed
jmpJpOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJnp:
	test al, 0x04
	jnz jmpTestJnpP
	popf
	jnp jmpJnpOk
jmpJnpFailed:
	mov si, jnpFailedStr
	jmp jmpFailed
jmpTestJnpP:
	popf
	jnp jmpJnpFailed
jmpJnpOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJl:
	and ax, 0x880
	jz jmpTestJlZ
	xor ax, 0x880
	jz jmpTestJlZ
	popf
	jl jmpJlOk
jmpJlFailed:
	mov si, jlFailedStr
	jmp jmpFailed
jmpTestJlZ:
	popf
	jl jmpJlFailed
jmpJlOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJnl:
	and ax, 0x880
	jz jmpTestJnlNoL
	xor ax, 0x880
	jz jmpTestJnlNoL
	popf
	jnl jmpJnlFailed
jmpJnlOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
jmpTestJnlNoL:
	popf
	jnl jmpJnlOk
jmpJnlFailed:
	mov si, jnlFailedStr
	jmp jmpFailed
;-------------------------------------
jmpTestJle:
	and ax, 0x8C0
	jz jmpTestJleNoLe
	xor ax, 0x880
	jz jmpTestJleNoLe
	popf
	jle jmpJleOk
jmpJleFailed:
	mov si, jleFailedStr
	jmp jmpFailed
jmpTestJleNoLe:
	popf
	jle jmpJleFailed
jmpJleOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
;-------------------------------------
jmpTestJnle:
	and ax, 0x8C0
	jz jmpTestJnleNoLe
	xor ax, 0x880
	jz jmpTestJnleNoLe
	popf
	jnle jmpJnleFailed
jmpJnleOk:
	pushf
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedFlags]
	xor cx, bx
	jz jmpExit
jmpTestJnleNoLe:
	popf
	jnle jmpJnleOk
jmpJnleFailed:
	mov si, jnleFailedStr
	jmp jmpFailed

;-------------------------------------
jmpFailed:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	call writeString
	mov ax, 1
	pop cx
	pop bx
	ret
;-------------------------------------
jmpExit:
	xor ax, ax
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
calcJmpFlags:
	push bx

	xor ax, ax
	mov bl, [es:inputVal1]

	mov al, bl
	and al, 0x01		; Carry

	test bl, 0x02		; Parity
	jz jmpNoP
	or al, 0x04
jmpNoP:
	test bl, 0x04		; Aux Carry
	jz jmpNoAC
	or al, 0x10
jmpNoAC:
	test bl, 0x08		; Zero
	jz jmpNoZ
	or al, 0x40
jmpNoZ:
	test bl, 0x10		; Sign
	jz jmpNoS
	or al, 0x80
jmpNoS:
	test bl, 0x20		; Overflow
	jz jmpNoV
	or ah, 0x08
jmpNoV:
	or ax, 0xf202		; Expected flags
	mov [es:inputFlags], ax
	mov [es:expectedFlags], ax
	pop bx
	ret

;-----------------------------------------------------------------------------
; Test pushing/popping SP to/from stack.
;-----------------------------------------------------------------------------
testSPStack:
	mov si, testingSPStackStr
	call writeString

	mov ax, sp
	mov [es:inputVal1], ax
	sub ax, 2
	mov [es:expectedResult1], ax
	push sp				; Save SP on stack to look at
	pop bx				; Get SP saved on stack
	mov [es:testedResult1], bx
	xor bx, ax
	jz testPopSpStack

	mov si, testPushSPStr
	call writeString
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz spStackFailed

testPopSpStack:
	mov ax, sp
	mov bx, ax
	mov [es:expectedResult1], bx
	mov [es:inputVal1], bx
	push bx				; Save BX on stack to look at
	pop sp				; Get SP saved on stack
	mov cx, sp
	mov sp, ax
	mov [es:testedResult1], cx
	xor bx, cx
	jz testPusha

	mov si, testPopSPStr
	call writeString
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz spStackFailed

testPusha:
	mov ax, sp
	mov [es:inputVal1], ax
	mov [es:expectedResult1], ax
	pusha
	pop cx				; IY
	pop cx				; IX
	pop cx				; BP
	pop bx				; SP
	pop cx				; BW
	pop cx				; DW
	pop cx				; CW
	pop cx				; AW
	mov [es:testedResult1], bx
	xor bx, ax
	jz testPopa

	mov si, testPushaStr
	call writeString
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz spStackFailed

testPopa:
	mov cx, sp
	mov ax, cx
	sub ax, 20
	mov [es:inputVal1], ax
	mov [es:expectedResult1], cx
	push ax				; AW
	push ax				; CW
	push ax				; DW
	push ax				; BW
	push ax				; SP
	push ax				; BP
	push ax				; IX
	push ax				; IY
	popa
	mov bx, sp
	mov cx, [es:expectedResult1]
	mov sp, cx
	mov [es:testedResult1], bx
	xor bx, cx
	jz spStackOk

	mov si, testPopaStr
	call writeString
	call printFailedResult
	call checkKeyInput
	xor al, 0
	jz spStackFailed

spStackOk:
	mov si, okStr
	call writeString
	xor ax, ax
	ret

spStackFailed:
	mov si, failedStr
	call writeString
	mov ax, 1
	ret

;-----------------------------------------------------------------------------
; Wait for input, A continue, B cancel.
;-----------------------------------------------------------------------------
checkKeyInput:
	hlt
	in al, IO_KEYPAD
	test al, PAD_A | PAD_B
	jnz checkKeyInput		; Make sure no input is held before.
keyLoop:
	hlt
	in al, IO_KEYPAD
	test al, PAD_A
	jnz keyContinue
	test al, PAD_B
	jnz keyCancel
	jmp keyLoop
keyContinue:
	mov al, 1
	ret
keyCancel:
	xor al, al
	ret
;-----------------------------------------------------------------------------
; Gets a new number from LFSR1
;-----------------------------------------------------------------------------
getLFSR1Value:
	mov ax, [es:lfsr1]
	shr ax, 1
	jnc noTaps1
	xor ax, 0xD008
noTaps1:
	mov [es:lfsr1], ax
	ret
;-----------------------------------------------------------------------------
; Gets a new number from LFSR2
;-----------------------------------------------------------------------------
getLFSR2Value:
	mov ax, [es:lfsr2]
	shr ax, 1
	jnc noTaps2
	xor ax, 0xD008
noTaps2:
	mov [es:lfsr2], ax
	ret
;-----------------------------------------------------------------------------
; Print expected result and flags plus tested result and flags.
;-----------------------------------------------------------------------------
printFailedResult:
	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, inputStr
	call writeString

	mov ax, [es:inputVal2]
	call printHexW
	mov si, hexPrefixStr
	call writeString
	mov ax, [es:inputVal1]
	call printHexW
	mov si, fHexPrefixStr
	call writeString
	mov ax, [es:inputFlags]
	call printHexW
	mov al, 10
	int 0x10

	mov si, expectedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:expectedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:expectedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:expectedException]
	add al, '0'
	int 0x10

	mov si, testedStr
	call writeString
	mov si, valueStr
	call writeString
	mov ax, [es:testedResult1]
	call printHexW
	mov si, flagsStr
	call writeString
	mov ax, [es:testedFlags]
	call printHexW
	mov al, ' '
	int 0x10
	mov al, 'X'
	int 0x10
	mov al, [es:testedException]
	add al, '0'
	int 0x10
	mov al, 10
	int 0x10

	ret

;-----------------------------------------------------------------------------
; Clear tilemap line.
;-----------------------------------------------------------------------------
clearLine:
	xor bh, bh
	mov bl, [es:cursorYPos]
	and bl, 0x1f
	shl bx, 6		; ax * MAP_TWIDTH
	mov di, backgroundMap
	add di, bx
	mov cx, MAP_TWIDTH
	mov ax, BG_CHR( ' ', 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	rep stosw
	ret
;-----------------------------------------------------------------------------
; Clear foreground tilemap.
;-----------------------------------------------------------------------------
clearForegroundMap:
	mov di, foregroundMap
	jmp clearTileMap
;-----------------------------------------------------------------------------
; Clear background tilemap.
;-----------------------------------------------------------------------------
clearScreen:
	mov di, backgroundMap
clearTileMap:
	; Clear a tilemap by writing space (0x20) to all locations.
	mov ax, BG_CHR( ' ', 0, 0, 0, 0 ) ; BG_CHR(tile,pal,bank,hflip,vflip)
	mov cx, MAP_TWIDTH * MAP_THEIGHT
	rep stosw
	xor ax, ax
	mov [es:cursorPos], ax
	ret
;-----------------------------------------------------------------------------
; Write text to background. si = source
;-----------------------------------------------------------------------------
writeString:
	mov cx, SCREEN_TWIDTH * SCREEN_THEIGHT
textLoop:
	lodsb
	int 0x10
	xor al, 0
	jz endString
	dec cx
	jnz textLoop
endString:
	ret

;-----------------------------------------------------------------------------
printHexW:
	push ax
	mov al, ah
	call printHexB
	pop ax
;-----------------------------------------------------------------------------
printHexB:
	push ax
	shr al, 0x04
	call printNibble
	pop ax
	and al, 0x0f
printNibble:
	cmp al, 0x09
	jg .letter
	add al, '0'
	int 0x10
	ret
.letter:
	add al, 'a' - 0xa
	int 0x10
	ret
;-----------------------------------------------------------------------------
; Our vblank interrupt handler
; It is called automatically whenever the vblank interrupt occurs, 
; that is, every time the screen is fully drawn.
;-----------------------------------------------------------------------------
vblankInterruptHandler:
	push ax
	push bx
	push di

	; globalFrameCounter++
	mov ax, [es:globalFrameCounter]
	inc ax
	mov [es:globalFrameCounter], ax

	mov ax, [es:bgPos]
	out IO_SCR1_SCRL_X, ax
	mov ax, [es:fgPos]
	out IO_SCR2_SCRL_X, ax

	mov al, [es:isTesting]
	xor al, 0
	jz skipValuePrint
	cmp al, 1
	jnz skipValue8x8Print
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal2]
	call printHexB
	mov byte [es:cursorXPos], 23
	mov al, [es:inputVal1]
	call printHexB
	cmp byte [es:inputCarry], 0
	jz skipValuePrint
	mov byte [es:cursorXPos], 26
	mov al, 'C'
	int 0x10
	jmp skipValuePrint
skipValue8x8Print:
	cmp al, 2
	jnz skipValue16x8Print
	mov byte [es:cursorXPos], 17
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 25
	mov al, [es:inputVal1]
	call printHexB
	jmp skipValuePrint
skipValue16x8Print:
	cmp al, 3
	jnz skipValue8Print
	mov byte [es:cursorXPos], 15
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 23
	mov ax, [es:inputVal1]
	call printHexW
	jmp skipValuePrint
skipValue8Print:
	cmp al, 4
	jnz skipValuePrint
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal1]
	call printHexB
	jmp skipValuePrint
skipValuePrint:
acknowledgeVBlankInterrupt:
	mov al, INT_VBLANK_START
	out INT_CAUSE_CLEAR, al

	pop di
	pop bx
	pop ax
	iret

;-----------------------------------------------------------------------------
; Our division error handler
; It is called if a division error occurs.
;-----------------------------------------------------------------------------
divisionErrorHandler:
;	mov word [es:WSC_PALETTES], 0xF0F
	mov byte [es:testedException], 1
	iret

;-----------------------------------------------------------------------------
; Our illegal instruction handler
; It is called if trying to execute an illegal instruction.
;-----------------------------------------------------------------------------
illegalInstructionHandler:
	push ax
	push bx
	push di

	mov word [es:WSC_PALETTES], 0x0F0

	pop di
	pop bx
	pop ax
	iret

;-----------------------------------------------------------------------------
; Write a char to background. al = char
;-----------------------------------------------------------------------------
outputCharHandler:
	push bx
	push cx
	push di

	xor bh, bh
	mov bl, [es:cursorYPos]
	and bl, 0x1F
	shl bx, 5		; ax * MAP_TWIDTH
	mov cl, [es:cursorXPos]
	add bl, cl
	shl bx, 1
	mov di, backgroundMap
	add di, bx
	xor al, 0
	jz endOutput
	cmp al, 10
	jz newLine
	stosb
	inc di
	inc cl
	cmp cl, 28
	jnz endOutput
newLine:
	mov bl, [es:cursorYPos]
	inc bl
	mov al, bl
	sub al, SCREEN_THEIGHT-1
	jle notAtEnd
	and bl, 0x1F
	or bl, 0x40
	shl al, 3
	mov [es:bgYPos], al
notAtEnd:
	mov [es:cursorYPos], bl
	call clearLine
	xor cl, cl
endOutput:
	mov [es:cursorXPos], cl
	pop di
	pop cx
	pop bx
	iret

;-----------------------------------------------------------------------------
;
; BEGIN main area
;
;-----------------------------------------------------------------------------
main_loop:
	hlt					; Wait until next interrupt

;	mov bl, [es:enemySpawnPosition]
;	cmp bl, 0
;	jnz dontPrint
;	mov si, alphabet
;	call writeString
dontPrint:

	mov al, KEYPAD_READ_ARROWS_H
	out IO_KEYPAD, al
	nop
	nop
	nop
	nop
	in al, IO_KEYPAD

	; Check player input
;	test al, PAD_RIGHT
;	jnz speed_up

;	test al, PAD_LEFT
;	jnz speed_down

;	test al, PAD_UP
;	jnz move_up

;	test al, PAD_DOWN
;	jnz move_down

	; No input, restart main loop
	jmp main_loop

;-----------------------------------------------------------------------------
;
; END main area
;
;-----------------------------------------------------------------------------

;-----------------------------------------------------------------------------
; Constants area
;-----------------------------------------------------------------------------

	align 2

PZSTable:
	db PSR_Z|PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0 ,PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0, 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P
	db 0, PSR_P, PSR_P, 0, PSR_P, 0, 0, PSR_P, PSR_P, 0, 0, PSR_P, 0, PSR_P, PSR_P, 0
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S
	db PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S, PSR_S+PSR_P, PSR_S, PSR_S+PSR_P, PSR_S, PSR_S, PSR_P+PSR_S

DecTable:
	db 0xFF, 0x00
IncTable:
	db 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10
	db 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20
	db 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30
	db 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F, 0x40
	db 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50
	db 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F, 0x60
	db 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F, 0x70
	db 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F, 0x80
	db 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F, 0x90
	db 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F, 0xA0
	db 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7, 0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF, 0xB0
	db 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7, 0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF, 0xC0
	db 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7, 0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF, 0xD0
	db 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7, 0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF, 0xE0
	db 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7, 0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF, 0xF0
	db 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7, 0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF, 0x00

FontTilePalette:
	dw 0xFFF, 0x000

MonoFont:
	db 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x18,0x08,0x10,0x00,0x18,0x00
	db 0x6C,0x6C,0x24,0x48,0x00,0x00,0x00,0x00,0x14,0x14,0xFE,0x28,0xFE,0x50,0x50,0x00
	db 0x10,0x7C,0x90,0x7C,0x12,0xFC,0x10,0x00,0x42,0xA4,0xA8,0x54,0x2A,0x4A,0x84,0x00
	db 0x30,0x48,0x38,0x62,0x94,0x88,0x76,0x00,0x18,0x18,0x08,0x10,0x00,0x00,0x00,0x00
	db 0x08,0x10,0x20,0x20,0x20,0x10,0x08,0x00,0x20,0x10,0x08,0x08,0x08,0x10,0x20,0x00
	db 0x10,0x92,0x54,0x38,0x38,0x54,0x92,0x00,0x10,0x10,0x10,0xFE,0x10,0x10,0x10,0x00
	db 0x00,0x00,0x00,0x30,0x30,0x10,0x20,0x00,0x00,0x00,0x00,0xFE,0x00,0x00,0x00,0x00
	db 0x00,0x00,0x00,0x00,0x00,0x60,0x60,0x00,0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x00

	db 0x3C,0x42,0x46,0x5A,0x62,0x42,0x3C,0x00,0x08,0x38,0x08,0x08,0x08,0x08,0x08,0x00
	db 0x3C,0x42,0x42,0x0C,0x30,0x40,0x7E,0x00,0x3C,0x42,0x02,0x1C,0x02,0x42,0x3C,0x00
	db 0x0C,0x14,0x24,0x44,0x7E,0x04,0x04,0x00,0x7E,0x40,0x7C,0x02,0x02,0x42,0x3C,0x00
	db 0x3C,0x40,0x7C,0x42,0x42,0x42,0x3C,0x00,0x7E,0x02,0x04,0x08,0x08,0x10,0x10,0x00
	db 0x3C,0x42,0x42,0x3C,0x42,0x42,0x3C,0x00,0x3C,0x42,0x42,0x42,0x3E,0x02,0x3C,0x00
	db 0x00,0x18,0x18,0x00,0x18,0x18,0x00,0x00,0x00,0x18,0x18,0x00,0x18,0x08,0x10,0x00
	db 0x00,0x08,0x10,0x20,0x10,0x08,0x00,0x00,0x00,0x00,0x3C,0x00,0x3C,0x00,0x00,0x00
	db 0x00,0x10,0x08,0x04,0x08,0x10,0x00,0x00,0x3C,0x62,0x62,0x0C,0x18,0x00,0x18,0x00

	db 0x7C,0x82,0xBA,0xA2,0xBA,0x82,0x7C,0x00,0x10,0x28,0x28,0x44,0x7C,0x82,0x82,0x00
	db 0x7C,0x42,0x42,0x7C,0x42,0x42,0x7C,0x00,0x1C,0x22,0x40,0x40,0x40,0x22,0x1C,0x00
	db 0x78,0x44,0x42,0x42,0x42,0x44,0x78,0x00,0x7E,0x40,0x40,0x7E,0x40,0x40,0x7E,0x00
	db 0x7E,0x40,0x40,0x7C,0x40,0x40,0x40,0x00,0x3C,0x42,0x80,0x9E,0x82,0x46,0x3A,0x00
	db 0x42,0x42,0x42,0x7E,0x42,0x42,0x42,0x00,0x10,0x10,0x10,0x10,0x10,0x10,0x10,0x00
	db 0x02,0x02,0x02,0x02,0x42,0x42,0x3C,0x00,0x42,0x44,0x48,0x50,0x68,0x44,0x42,0x00
	db 0x40,0x40,0x40,0x40,0x40,0x40,0x7E,0x00,0x82,0xC6,0xAA,0x92,0x82,0x82,0x82,0x00
	db 0x42,0x62,0x52,0x4A,0x46,0x42,0x42,0x00,0x38,0x44,0x82,0x82,0x82,0x44,0x38,0x00

	db 0x7C,0x42,0x42,0x7C,0x40,0x40,0x40,0x00,0x38,0x44,0x82,0x82,0x8A,0x44,0x3A,0x00
	db 0x7C,0x42,0x42,0x7C,0x48,0x44,0x42,0x00,0x3C,0x42,0x40,0x3C,0x02,0x42,0x3C,0x00
	db 0xFE,0x10,0x10,0x10,0x10,0x10,0x10,0x00,0x42,0x42,0x42,0x42,0x42,0x42,0x3C,0x00
	db 0x82,0x82,0x44,0x44,0x28,0x28,0x10,0x00,0x82,0x92,0x92,0xAA,0xAA,0x44,0x44,0x00
	db 0x82,0x44,0x28,0x10,0x28,0x44,0x82,0x00,0x82,0x44,0x28,0x10,0x10,0x10,0x10,0x00
	db 0x7E,0x04,0x08,0x10,0x20,0x40,0x7E,0x00,0x18,0x10,0x10,0x10,0x10,0x10,0x18,0x00
	db 0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x00,0x18,0x08,0x08,0x08,0x08,0x08,0x18,0x00
	db 0x10,0x28,0x44,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0xFE,0x00

	db 0x08,0x10,0x18,0x18,0x00,0x00,0x00,0x00,0x00,0x78,0x04,0x7C,0x84,0x84,0x7E,0x00
	db 0x40,0x40,0x7C,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x3C,0x42,0x40,0x42,0x3C,0x00
	db 0x02,0x02,0x3E,0x42,0x42,0x42,0x3C,0x00,0x00,0x00,0x3C,0x42,0x7E,0x40,0x3E,0x00
	db 0x0C,0x10,0x3E,0x10,0x10,0x10,0x10,0x00,0x00,0x3C,0x42,0x42,0x3E,0x02,0x7C,0x00
	db 0x40,0x40,0x7C,0x42,0x42,0x42,0x42,0x00,0x18,0x18,0x00,0x08,0x08,0x08,0x08,0x00
	db 0x06,0x06,0x00,0x02,0x42,0x42,0x3C,0x00,0x20,0x20,0x26,0x28,0x30,0x28,0x26,0x00
	db 0x30,0x10,0x10,0x10,0x10,0x10,0x10,0x00,0x00,0x80,0xEC,0x92,0x92,0x92,0x92,0x00
	db 0x00,0x40,0x78,0x44,0x44,0x44,0x44,0x00,0x00,0x00,0x3C,0x42,0x42,0x42,0x3C,0x00

	db 0x00,0x3C,0x42,0x42,0x7C,0x40,0x40,0x00,0x00,0x78,0x84,0x84,0x7C,0x04,0x06,0x00
	db 0x00,0x00,0x5C,0x62,0x40,0x40,0x40,0x00,0x00,0x00,0x3E,0x40,0x3C,0x02,0x7C,0x00
	db 0x00,0x10,0x7C,0x10,0x10,0x10,0x0E,0x00,0x00,0x00,0x42,0x42,0x42,0x42,0x3F,0x00
	db 0x00,0x00,0x42,0x42,0x24,0x24,0x18,0x00,0x00,0x00,0x92,0x92,0x92,0x92,0x6C,0x00
	db 0x00,0x00,0x42,0x24,0x18,0x24,0x42,0x00,0x00,0x00,0x42,0x42,0x3E,0x02,0x7C,0x00
	db 0x00,0x00,0x7E,0x02,0x3C,0x40,0x7E,0x00,0x08,0x10,0x10,0x20,0x10,0x10,0x08,0x00
	db 0x10,0x10,0x10,0x00,0x10,0x10,0x10,0x00,0x20,0x10,0x10,0x08,0x10,0x10,0x20,0x00
	db 0x00,0x00,0x60,0x92,0x0C,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00


alphabet: db "ABCDEFGHIJKLMNOPQRSTUVWXYZ!", 10, 0
alphabet2: db "abcdefghijklmnopqrstuvwxyz.,", 10, 0

headLineStr: db "WonderSwan CPU Test 20220625",10 , 0

testingEquStr: db "Equal by CMP, SUB & XOR", 10, 0
testingAnd8Str: db "Logical AND bytes", 10, 0
testingOr8Str: db "Logical OR bytes", 10, 0
testingTest8Str: db "Logical TEST bytes", 10, 0
testingXor8Str: db "Logical XOR bytes", 10, 0
testingNot8Str: db "Logical NOT bytes", 10, 0
testingInc8Str: db "INC bytes", 10, 0
testingDec8Str: db "DEC bytes", 10, 0

testingAdd8Str: db "ADD bytes", 10, 0
testingSub8Str: db "SUB bytes", 10, 0
testingCmp8Str: db "CMP bytes", 10, 0
testingNeg8Str: db "NEG bytes", 10, 0
testingAdc8Str: db "ADC/ADDC bytes", 10, 0
testingSbb8Str: db "SBB/SUBC bytes", 10, 0

testingRol8Str: db "ROL byte by CL", 10, 0
testingRor8Str: db "ROR byte by CL", 10, 0
testingRcl8Str: db "RCL/ROLC byte by CL", 10, 0
testingRcr8Str: db "RCR/RORC byte by CL", 10, 0
testingShl8Str: db "SHL byte by CL", 10, 0
testingShr8Str: db "SHR byte by CL", 10, 0
testingSar8Str: db "SAR/SHRA byte by CL", 10, 0

testingAaaStr: db "AAA/ADJBA", 10, 0
testingAasStr: db "AAS/ADJBS", 10, 0
testingDaaStr: db "DAA/ADJ4A", 10, 0
testingDasStr: db "DAS/ADJ4S", 10, 0

testingMuluStr: db "Unsigned Multiplication 8*8", 10, 0
testingMulsStr: db "Signed Multiplication 8*8", 10, 0
testingMulu16Str: db "Unsigned Multiplication 16*16", 0
testingMuls16Str: db "Signed Multiplication 16*16", 10, 0

testingDivuStr: db "Unsigned Division 16/8", 10, 0
testingDivsStr: db "Signed Division 16/8", 10, 0
testingDivu32Str: db "Unsigned Division 32/16", 10, 0
testingDivs32Str: db "Signed Division 32/16", 10, 0
testingAamStr: db "AAM/CVTBD (division 8/8)", 10, 0
testingAadStr: db "AAD/CVTDB (mulu 8*8 + add 8)", 10, 0

testingJmpStr: db "Conditional JMP/BRA", 10, 0

testingSPStackStr: db "PUSH/POP SP to/from stack", 10, 0
testPushSPStr: db "Push SP", 10, 0
testPopSPStr: db "Pop SP", 10, 0
testPushaStr: db "Pusha SP", 10, 0
testPopaStr: db "Popa SP", 10, 0

test8InputStr: db "Testing Input: 0x00", 0
test16InputStr: db "Testing Input: 0x0000", 0
test8x8InputStr: db "Testing Input: 0x00, 0x00", 0
test16x8InputStr: db "Testing Input: 0x0000, 0x00", 0
test16x16InputStr: db "Testing Inp: 0x0000, 0x0000", 0
inputStr: db "Input:0x", 0
expectedStr: db "Expected Result:", 10, 0
testedStr: db "Tested Result:", 10, 0
valueStr: db "Value:0x",0
flagsStr: db " Flags:0x",0
okStr: db "Ok!", 10, 0
failedStr: db "Failed!", 10, 0
preFlagStr: db "PreF: ", 0
postFlagStr: db "PostF: ", 0
hexPrefixStr: db " 0x",0
fHexPrefixStr: db " F:0x",0

joFailedStr: db "JO/BV Failed", 10, 0
jnoFailedStr: db "JNO/BNV Failed", 10, 0
jbFailedStr: db "JB/JNAE/JC/BC/BL Failed", 10, 0
jnbFailedStr: db "JNB/JAE/JNC/BNC/BNL Failed", 10, 0
jzFailedStr: db "JZ/JE/BE/BZ Failed", 10, 0
jnzFailedStr: db "JNZ/JNE/BNE/BNZ Failed", 10, 0
jbeFailedStr: db "JBE/JNA/BNH Failed", 10, 0
jnbeFailedStr: db "JNBE/JA/BH Failed", 10, 0
jsFailedStr: db "JS/BN Failed", 10, 0
jnsFailedStr: db "JNS/BP Failed", 10, 0
jpFailedStr: db "JP/JPE/BPE Failed", 10, 0
jnpFailedStr: db "JNP/JPO/BPO Failed", 10, 0
jlFailedStr: db "JL/JNGE/BLT Failed", 10, 0
jnlFailedStr: db "JNL/JGE/BGE Failed", 10, 0
jleFailedStr: db "JLE/JNG/BLE Failed", 10, 0
jnleFailedStr: db "JNLE/JG/BGT Failed", 10, 0

author: db "Written by Fredrik Ahlström, 2022"

	ROM_HEADER initialize, MYSEGMENT, RH_WS_COLOR, RH_ROM_4MBITS, RH_NO_SRAM, RH_HORIZONTAL

SECTION .bss start=0x0100 ; Keep space for Int Vectors

globalFrameCounter: resw 1
bgPos:
bgXPos: resb 1
bgYPos: resb 1
fgPos:
fgXPos: resb 1
fgYPos: resb 1
cursorPos:
cursorXPos: resb 1
cursorYPos: resb 1

lfsr1: resw 1
lfsr2: resw 1

inputVal1: resw 1
inputVal2: resw 1
inputFlags: resw 1
inputCarry: resw 1

testedResult1: resw 1
testedResult2: resw 1
testedFlags: resw 1
testedException: resw 1		; If a (division) exception occurred.

expectedResult1: resw 1
expectedResult2: resw 1
expectedFlags: resw 1
expectedException: resw 1

isTesting: resb 1			; If currently running test.
dummy: resb 1

selfModifyingCode: resb 8
