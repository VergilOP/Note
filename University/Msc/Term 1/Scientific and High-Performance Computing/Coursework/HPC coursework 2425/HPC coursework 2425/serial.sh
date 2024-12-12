rm ./*.txt
gcc -O3 serial.c -Wall -o serial -lm
time ./serial