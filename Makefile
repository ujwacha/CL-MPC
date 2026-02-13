# Use GCC, if you have it installed.
CC=gcc

mylib: C/mylib.c C/mylib.h
	${CC} -shared -fPIC -o mylib.so  C/mylib.c
