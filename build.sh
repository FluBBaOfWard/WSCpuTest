#!/bin/sh
romname=WSCpuTest

nasm -f bin -o $romname.wsc $romname.asm -l $romname.lst
