set romname=WSCpuTest

del ..\emulator\rom\%romname%.wsc
copy /y %romname%.wsc ..\emulator\rom\
..\emulator\Oswan.exe ..\emulator\rom\%romname%.wsc
