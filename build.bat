set romname=WSCpuTest

del %romname%.wsc

..\bin\nasm -f bin -o %romname%.wsc %romname%.asm

pause
