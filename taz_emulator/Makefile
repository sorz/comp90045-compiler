# Makefile for the taz emulator

HDR =		taz.h std.h missing.h
SRC = 		taz.c ptaz.c ltaz.c
OBJ = 		taz.o ptaz.o ltaz.o
CC =	 	gcc
# We need -ffloat-store to help ensure consistent results for 
# floating point operations on different architectures and with 
# different optimization levels.
CFLAGS =	-Wall -ffloat-store -g -O0
LINKFLAGS =	-g

taz:		$(OBJ)
		$(CC) $(CFLAGS) $(LINKFLAGS) -o taz $(OBJ) -lm

ptaz.c ptaz.h:	ptaz.y
		bison -p tazyy -d ptaz.y -o ptaz.c

ltaz.c:		ltaz.l ptaz.h
		flex -s -t -Ptazyy ltaz.l > ltaz.c

clean:
		-/bin/rm -f $(OBJ) ptaz.c ptaz.h ltaz.c
clobber: clean
		-/bin/rm -f taz

$(OBJ):		$(HDR)
