#!/bin/sh
romname=WSCpuTest

rm $romname.wsc

nasm -f bin -o $romname.wsc $romname.asm -l $romname.lst
