;-----------------------------------------------------------------------------
;
;  Swan Test
;         by Fredrik Ahlström, 2022
;         https://github.com/FluBBaOfWard
;
;  For more information on the hardware specs, port descriptions, sprite
;  format, etc., see the hardware.txt file in the wonderdev root directory.
;
;  UP/DOWN    - Choose option
;  A          - Start
;
;  Assemble with: 
;                   nasm -f bin -o %romname%.wsc %romname%.asm
;
;-----------------------------------------------------------------------------

	ORG 0x0000
	CPU 186
	BITS 16

SECTION .data
	%include "WonderSwan.inc"

	MYSEGMENT equ 0xF000
	foregroundMap equ WS_TILE_BANK - MAP_SIZE
	backgroundMap equ foregroundMap - MAP_SIZE
	spriteTable equ backgroundMap - SPR_TABLE_SIZE

	COLLISION_RADIUS equ 6

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

	mov al, 0xF0
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

;	call testMulu8
;	cmp al, 0
;	jnz skipTests

;	call testMuls8
;	cmp al, 0
;	jnz skipTests

;	call testDivu8
;	cmp al, 0
;	jnz skipTests

;	call testDivs8
;	cmp al, 0
;	jnz skipTests

	call testAam
	cmp al, 0
	jnz skipTests

;	call testAamSingle

skipTests:
;-----------------------------------------------------------------------------
; Done initializing... We can now start the main loop.
;-----------------------------------------------------------------------------
	; Start main loop
	jmp main_loop

;-----------------------------------------------------------------------------
; Test unsigned multiplication of all byte values.
;-----------------------------------------------------------------------------
testMulu8:
	mov si, testingMuluStr
	call writeString
	mov si, testingInputStr
	call writeString

	mov word [es:inputVal2], 0
	mov word [es:expectedFlags], 0xF242
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
	mov ax, [es:expectedFlags]
	cmp bh, 0
	jz clrMuluOverflow
	or ax, 0x0801
	jmp muluOverflowDone
clrMuluOverflow:
	and ax, 0xF7FE
muluOverflowDone:
	mov [es:expectedFlags], ax
	call testMulu8Single
	cmp al, 0
	jnz stopMuluTest
	inc cx
	jnz testMuluLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
stopMuluTest:
	ret

;-----------------------------------------------------------------------------
testMulu8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0xF700
	push ax

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	mul bl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	cmp ax, cx
	jnz muluFailed
	mov cx, [es:expectedFlags]
	cmp bx, cx
	jnz muluFailed

	pushf
	pop ax
	or ax, 0x08FF
	push ax

	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	mul cl
	pushf

	mov [es:testedResult1], ax
	pop bx
	mov [es:testedFlags], bx
	mov cx, [es:expectedResult1]
	cmp ax, cx
	jnz muluFailed
	mov cx, [es:expectedFlags]
	cmp bx, cx
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

	mov word [es:expectedFlags], 0xF242
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
	mov ax, [es:expectedFlags]
	sar bx, 7
	jz clrMulsOverflow
	not bx
	cmp bx, 0
	jz clrMulsOverflow
	or ax, 0x0801
	jmp mulsOverflowDone
clrMulsOverflow:
	and ax, 0xF7FE
mulsOverflowDone:
	mov [es:expectedFlags], ax
	call testMuls8Single
	cmp al, 0
	jnz stopMulsTest
	inc cx
	jnz testMulsLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
stopMulsTest:
	ret

;-----------------------------------------------------------------------------
testMuls8Single:
	push bx
	push cx

	pushf
	pop ax
	and ax, 0xF700
	push ax

	mov al, [es:inputVal1]
	mov bl, [es:inputVal2]
	popf
	imul bl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	cmp ax, bx
	jnz mulsFailed
	mov bx, [es:expectedFlags]
	cmp cx, bx
	jnz mulsFailed

	pushf
	pop ax
	or ax, 0x08FF
	push ax

	mov al, [es:inputVal1]
	mov cl, [es:inputVal2]
	popf
	imul cl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	cmp ax, bx
	jnz mulsFailed
	mov bx, [es:expectedFlags]
	cmp cx, bx
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
; Test unsigned division of all word/byte values.
;-----------------------------------------------------------------------------
testDivu8:
	mov si, testingDivuStr
	call writeString
	mov si, testDivInputStr
	call writeString

	mov byte [es:isTesting], 2

	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	xor cx, cx
	xor dx, dx
testDivuLoop:
	mov [es:inputVal1], dl
	mov [es:inputVal2], cx
	call calcDivuResult
	call testDivu8Single
;	cmp al, 0
;	jnz stopDivuTest
hold:
	in al, IO_KEYPAD
	test al, PAD_A
	jnz hold

	inc cx
	jnz testDivuLoop
	inc dl
	jnz testDivuLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
stopDivuTest:
	ret

;-----------------------------------------------------------------------------
testDivu8Single:
	push bx
	push cx

	mov byte [es:testedException], 0
	pushf
	pop ax
	and ax, 0xF700
	push ax

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	div bl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	cmp ax, bx
	jnz divuFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	and cx, 0xffbF				; Clear Zero flag
	jnz divuFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	cmp al, bl
	jnz divuFailed

	mov byte [es:testedException], 0
	pushf
	pop ax
	or ax, 0x08FF
	push ax

	mov cl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	div cl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	cmp ax, bx
	jnz divuFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	and cx, 0xffbF				; Clear Zero flag
	jnz divuFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	cmp al, bl
	jnz divuFailed

	xor ax, ax
	pop cx
	pop bx
	ret

divuFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
calcDivuResult:
	push ax
	push bx
	push cx
	push dx

	mov byte [es:expectedException], 0
	xor bx, bx
	xor cx, cx
	mov dx, 0xfa03				; Expected flags
	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	mov [es:expectedResult1], ax
	cmp bl, 0
	jz divuError
	cmp ah, bl
	jnc divuError
	cmp ax, 0
	jz divuDone
divuLoop:
	sub ax, bx
	jc divuSetRes
	inc cl
	jmp divuLoop

divuSetRes:
	add ax, bx
	mov ah, al
	mov al, cl
	mov [es:expectedResult1], ax
divuSetZ:			; This is wrong!
;	cmp ah, 0
;	jnz divuDone
;	or dl, 0x40
divuDone:
	mov [es:expectedFlags], dx
	pop dx
	pop cx
	pop bx
	pop ax
	ret
divuError:
	mov byte [es:expectedException], 1
	jmp divuSetZ
;-----------------------------------------------------------------------------
; Test signed division of all word/byte values.
;-----------------------------------------------------------------------------
testDivs8:
	mov si, testingDivsStr
	call writeString
	mov si, testDivInputStr
	call writeString

	mov byte [es:isTesting], 2

	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	xor cx, cx
	xor dx, dx
testDivsLoop:
	mov [es:inputVal1], dl
	mov [es:inputVal2], cx
	call calcDivsResult
	call testDivs8Single
;	cmp al, 0
;	jnz stopDivsTest
	mov byte [es:isTesting], 2
holds:
	in al, IO_KEYPAD
	test al, PAD_A
	jnz holds

	inc cx
	jnz testDivsLoop
	inc dl
	jnz testDivsLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
stopDivsTest:
	ret

;-----------------------------------------------------------------------------
testDivs8Single:
	push bx
	push cx

	mov byte [es:testedException], 0
	pushf
	pop ax
	and ax, 0xF700
	push ax

	mov bl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	idiv bl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	cmp ax, bx
	jnz divsFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	and cx, 0xf73a				; Clear some flags
	jnz divsFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	cmp al, bl
	jnz divsFailed

	mov byte [es:testedException], 0
	pushf
	pop ax
	or ax, 0x08FF
	push ax

	mov cl, [es:inputVal1]
	mov ax, [es:inputVal2]
	popf
	idiv cl
	pushf

	mov [es:testedResult1], ax
	pop cx
	mov [es:testedFlags], cx
	mov bx, [es:expectedResult1]
	cmp ax, bx
	jnz divsFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	and cx, 0xf73a				; Clear some flags
	jnz divsFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	cmp al, bl
	jnz divsFailed

	xor ax, ax
	pop cx
	pop bx
	ret

divsFailed:
	call printFailedResult
	mov ax, 1
	pop cx
	pop bx
	ret

;-----------------------------------------------------------------------------
calcDivsResult:
	push ax
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
	jz divsError
	jns denPos
	neg bx
denPos:
	cmp ax, 0
	jz divsDone
	jns enumPos
	neg ax
enumPos:
	mov cx, ax
	shr cx, 7
	cmp cx, bx
	jnc divsError
	xor cx, cx
divsLoop:
	sub ax, bx
	jc divsSetRes
	inc cl
	jmp divsLoop

divsSetRes:
	add ax, bx
	cmp dh, 0
	jns resultPos
	neg cl
resultPos:
	cmp dl, 0
	jns restPos
	neg al
restPos:
	mov ah, al
	mov al, cl
	mov [es:expectedResult1], ax
divsSetP:			; This is wrong!
;	cmp ah, 0
;	jnz divsDone
;	or dl, 0x04
divsDone:
	mov dx, 0xf282				; Expected flags
	mov [es:expectedFlags], dx
	pop dx
	pop cx
	pop bx
	pop ax
	ret
divsError:
	cmp ax, 0x8000
	jnz divsErrCnt
	cmp bl, 0x00
	jnz divsErrCnt
	mov ax, 0x0081
	mov [es:expectedResult1], ax
	jmp divsSetP
divsErrCnt:
	mov byte [es:expectedException], 1
	jmp divsSetP
;-----------------------------------------------------------------------------
; Test unsigned division of all byte/byte values.
;-----------------------------------------------------------------------------
testAam:
	mov si, testingAamStr
	call writeString
	mov si, testingInputStr
	call writeString

	mov byte [es:isTesting], 1

	mov al, KEYPAD_READ_BUTTONS
	out IO_KEYPAD, al
	xor cx, cx
testAamLoop:
	mov [es:inputVal1], cl
	mov [es:inputVal2], ch
	call calcAamResult
	call testAamSingle
;	cmp al, 0
;	jnz stopAamTest
hold2:
	in al, IO_KEYPAD
	test al, PAD_A
	jnz hold2

	inc cx
	jnz testAamLoop

	hlt						; Wait for VBlank
	mov byte [es:isTesting], 0
	mov al, 10
	int 0x10
	mov si, okStr
	call writeString
	xor ax, ax
stopAamTest:
	ret

;-----------------------------------------------------------------------------
testAamSingle:
	push bx
	push cx

	mov byte [es:selfModifyingCode], 0xd4	; AAM
	mov byte [es:selfModifyingCode+2], 0xcb	; RETF

	pushf
	pop ax
	and ax, 0xF700
	push ax

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
	cmp ax, bx
	jnz aamFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aamFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	cmp al, bl
	jnz aamFailed

	pushf
	pop ax
	or ax, 0x08FF
	push ax

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
	cmp ax, bx
	jnz aamFailed
	mov bx, [es:expectedFlags]
	xor cx, bx
	jnz aamFailed
	mov al, [es:testedException]
	mov bl, [es:expectedException]
	cmp al, bl
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
	mov dx, 0xf202				; Expected flags
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
	pushf
	pop ax
	and al,0xc4			; Mask Zero, Sign & Parity
	or dl, al
aamDone:
	mov [es:expectedFlags], dx
	pop dx
	pop bx
	ret

aamError:
	or dx, 0x0801		; Overflow & Carry flag
	test al, 0xc0
	jnz aamErrNoZ
	or dl, 0x40			; Zero flag
aamErrNoZ:
	mov byte [es:expectedException], 1
	jmp aamDone

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
	mov al, 10
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
	and bl, 0x1F
	shl bx, 6		; ax * MAP_TWIDTH
	mov di, backgroundMap
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
	jnz skipValuePrint
	mov byte [es:cursorXPos], 17
	mov ax, [es:inputVal2]
	call printHexW
	mov byte [es:cursorXPos], 25
	mov al, [es:inputVal1]
	call printHexB
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
;	or bl, 0x80
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

headLineStr: db "WonderSwan CPU Test 20220504", 0
testingMuluStr: db "Unsigned Multiplication 8*8", 10, 0
testingMulsStr: db "Signed Multiplication 8*8", 10, 0
testingMulu16Str: db "Unsigned Multiplication 16*16", 0
testingMuls16Str: db "Signed Multiplication 16*16", 10, 0
testingDivuStr: db "Unsigned Division 16/8", 10, 0
testingDivsStr: db "Signed Division 16/8", 10, 0
testingDivu32Str: db "Unsigned Division 32/16", 10, 0
testingDivs32Str: db "Signed Division 32/16", 10, 0
testingAamStr: db "AAM/CVTBD (division 8/8)", 10, 0
testingInputStr: db "Testing Input: 0x00, 0x00", 0
testDivInputStr: db "Testing Input: 0x0000, 0x00", 0
inputStr: db "Input: 0x", 0
expectedStr: db "Expected Result:", 10, 0
testedStr: db "Tested Result:", 10, 0
valueStr: db "Value:0x",0
flagsStr: db " Flags:0x",0
okStr: db "Ok! ", 10, 0
preFlagStr: db "PreF: ", 0
postFlagStr: db "PostF: ", 0
hexPrefixStr: db " 0x",0
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

inputVal1: resw 1
inputVal2: resw 1

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
