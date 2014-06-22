#!/bin/bash
c2hs -C "-I/usr/include/SDL" -C "-I../../../../include" -t ../ $1
