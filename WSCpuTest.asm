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

	call testAdd8

	call testEqu
	call testAnd8
	call testOr8
	call testTest8
	call testXor8

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
	mov si, testingInputStr
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
	cmp al, 0
	jnz continueEqu8
	ret
neq8Failed:
	call printFailedResult
	call checkKeyInput
	cmp al, 0
	jnz continueNeq8
	ret
equ16Failed:
	call printFailedResult
	call checkKeyInput
	cmp al, 0
	jnz continueEqu16
	ret
neq16Failed:
	call printFailedResult
	call checkKeyInput
	cmp al, 0
	jnz continueNeq16
	ret

;-----------------------------------------------------------------------------
; Test logical AND of all byte values.
;-----------------------------------------------------------------------------
testAnd8:
	mov si, testingAnd8Str
	call writeString
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1
	mov word [es:expectedResult1], 0

	mov cx, 0
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
	cmp al, 0
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
	cmp al, 0
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
; Test logical OR of all byte values.
;-----------------------------------------------------------------------------
testOr8:
	mov si, testingOr8Str
	call writeString
	mov si, testingInputStr
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
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cx, 0
testTest8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	mov al, cl
	and al, ch
	lea bx, PZSTable
	xlat
	mov ah, 0xf2
	or al, 0x02
	mov [es:expectedFlags], ax
	call testTest8Single
	cmp al, 0
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
	cmp al, 0
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

	pop bx
	mov [es:testedFlags], bx
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

	pop bx
	mov [es:testedFlags], bx
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
	mov si, testingInputStr
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
	xor ax, ax
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

	xor ah, ah
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
	xor bx, cx
	jnz xor8Failed

	pushf
	pop ax
	or ax, 0x78FF
	push ax
	mov [es:inputFlags], ax

	xor ah, ah
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
	xor bx, cx
	jnz xor8Failed

	xor ax, ax
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
; Test ADD for all bytes & bytes values.
;-----------------------------------------------------------------------------
testAdd8:
	mov si, testingAdd8Str
	call writeString
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testAdd8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcAdd8Result
	call testAdd8Single
	cmp al, 0
	jnz stopAdd8Test
continueAdd8:
	inc cx
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
	cmp al, 0
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
	mov cl, bl
	xor cl, al
	xor ah, ah
	xor bh, bh

	add ax, bx

	mov [es:expectedResult1], al
	xor cl, al
	mov bl, cl
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
; Test ROL for all byte & 5bit values.
;-----------------------------------------------------------------------------
testRol8:
	mov si, testingRol8Str
	call writeString
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRol8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRol8Result
	call testRol8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRor8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRor8Result
	call testRor8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRcl8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRcl8Result
	call testRcl8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testRcr8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcRcr8Result
	call testRcr8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testShl8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcShl8Result
	call testShl8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testShr8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcShr8Result
	call testShr8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testSar8Loop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcSar8Result
	call testSar8Single
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov word [es:inputVal2], 0
	mov byte [es:isTesting], 1

	mov cx, 0
testMuluLoop:
	mov bx, [es:expectedResult1]
	mov ax, [es:inputVal2]
	add bx, ax
	mov [es:inputVal1], cl
	cmp cl, 0
	jnz skipMuluVal2
	xor bx, bx
	mov [es:inputVal2], ch
skipMuluVal2:
	mov [es:expectedResult1], bx
	mov ax, 0xF242
	cmp bh, 0
	jz noMuluOverflow
	or ax, 0x0801
noMuluOverflow:
	mov [es:expectedFlags], ax
	call testMulu8Single
	cmp al, 0
	jnz stopMuluTest
continueMulu:
	inc cx
	jnz testMuluLoop

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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	mov cx, 0
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
	cmp cl, 0
	jnz skipMulsVal2
	xor bx, bx
	mov [es:inputVal2], ch
skipMulsVal2:
	mov [es:expectedResult1], bx
	mov ax, 0xF242
	sar bx, 7
	jz noMulsOverflow
	not bx
	cmp bx, 0
	jz noMulsOverflow
	or ax, 0x0801
noMulsOverflow:
	mov [es:expectedFlags], ax
	call testMuls8Single
	cmp al, 0
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
	cmp al, 0
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
testAadLoop:
	mov [es:inputVal1], dl
	mov [es:inputVal2], cx
	call calcAadResult
	call testAadSingle
	cmp al, 0
	jnz stopAadTest
continueAad:
	inc cx
	jnz testAadLoop
	inc dl
	jnz testAadLoop

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
	cmp al, 0
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

	mov bl, [es:inputVal1]
	mov [es:selfModifyingCode+1], bl	; dividend

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
	push bx

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	mov bh, al
	xor al, al
	cmp bl, 0
	jz aadSetRes
aadLoop:
	add al, ah
	dec bl
	jnz aadLoop

aadSetRes:
	add al, bh
	pushf
	xor ah, ah
	mov [es:expectedResult1], ax
	pop ax							; All flags are from the last add.
	mov [es:expectedFlags], ax
	pop bx
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
	cmp al, 0
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
	cmp al, 0
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
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
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
	cmp al, 0
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
	cmp al, 0
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
	cmp bl, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testDaaLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcDaaResult
	call testDaaSingle
	cmp al, 0
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
	cmp al, 0
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
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	xor cx, cx
testDasLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcDasResult
	call testDasSingle
	cmp al, 0
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
	cmp al, 0
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
	mov bl, 0

testAaaLoop:
	mov [es:inputVal1], bl
	xor cx, cx
testAaaLoop2:
	mov [es:inputVal2], cx
	call calcAaaResult
	call testAaaSingle
	cmp al, 0
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
	cmp al, 0
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
	mov bl, 0

testAasLoop:
	mov [es:inputVal1], bl
	xor cx, cx
testAasLoop2:
	mov [es:inputVal2], cx
	call calcAasResult
	call testAasSingle
	cmp al, 0
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
	cmp al, 0
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
; Test pushing SP to stack.
;-----------------------------------------------------------------------------
testSPStack:
	mov si, testingSPStackStr
	call writeString

	push sp				; Save SP on stack to look at
	pop bx				; Get SP saved on stack
	xor bx,sp
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
	cmp al, 0
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
	cmp al, 0
	jz skipValuePrint
	cmp al, 1
	jnz skipValue8x8Print
	mov byte [es:cursorXPos], 17
	mov al, [es:inputVal2]
	call printHexB
	mov byte [es:cursorXPos], 23
	mov al, [es:inputVal1]
	call printHexB
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
	jnz skipValuePrint
	mov byte [es:cursorXPos], 15
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 23
	mov ax, [es:inputVal1]
	call printHexW
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
	cmp al, 0
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

headLineStr: db "WonderSwan CPU Test 20220610",10 , 0

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

testingSPStackStr: db "Pushing SP to stack", 10, 0

testingInputStr: db "Testing Input: 0x00, 0x00", 0
test16x8InputStr: db "Testing Input: 0x0000, 0x00", 0
test16x16InputStr: db "Testing Inp: 0x0000, 0x0000", 0
inputStr: db "Input:0x", 0
expectedStr: db "Expected Result:", 10, 0
testedStr: db "Tested Result:", 10, 0
valueStr: db "Value:0x",0
flagsStr: db " Flags:0x",0
okStr: db "Ok! ", 10, 0
failedStr: db "Failed! ", 10, 0
preFlagStr: db "PreF: ", 0
postFlagStr: db "PostF: ", 0
hexPrefixStr: db " 0x",0
fHexPrefixStr: db " F:0x",0
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
