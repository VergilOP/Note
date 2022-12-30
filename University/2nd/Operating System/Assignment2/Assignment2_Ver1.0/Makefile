GCC=gcc
OPTIONS=-Werror -Wall -lpthread 
OPTIONS_C99=-std=gnu99 $(OPTIONS)
all: test_bst.c serve_client.c bst.o
	$(GCC) bst.o test_bst.c -o test_bst.o $(OPTIONS_C99)

lib: bst.c bst.h
	$(GCC) $(OPTIONS) -c bst.c -o bst.o

test: all
	./test.sh

cycle: test_bst.c bst.o 
	$(GCC) bst.o test_bst.c -o test_bst_cycle.o $(OPTIONS) -DCYCLE_TEST
	./test_bst_cycle.o 

clean:
	rm bst.o test_bst.o log_1 b_tree.log u_tree.log test_bst_cycle.o

