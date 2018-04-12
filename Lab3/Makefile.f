#=======================================================================
#-------------------  object files  ------------------------------------
OBJ = burger.py

#-------------------  macro definitions  -------------------------------
.SUFFIXES: .c .o
EXEDIR = ./exe-dir/
USER   = #user
FC     = gfortran
OPTS   = -c 
CC     = gcc
CFLAGS = -O3 -w
LDR    = gfortran
#---------------------  targets  ---------------------------------------
all: 
	make compile

help:
	@echo Type 'make compile' to generate lab3 executable
	@echo Type 'make clean'   to remove *.o files
#-----------------------------------------------------------------------
clean:
	rm *.o 
#-----------------------------------------------------------------------
%.o: %.f90 
	gfortran -c $(CFLAGS) $<

compile: ${OBJ} #${USER}.o
	${LDR} -o ${EXEDIR}Lab3-581.exe ${OBJ}