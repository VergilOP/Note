# Examine me if you wish, but we will learn about Makefiles in a later
# lecture.  This file allows us to automate tedious repetitive build
# commands
CFLAGS = -Wall -Werror 
GCC = gcc $(CFLAGS)

all: exec fork 

exec: exec.c
	$(GCC) exec.c -o exec

fork: fork.c
	$(GCC) $< -o $@


clean:
	rm -f exec fork 
	rm -f *.o
