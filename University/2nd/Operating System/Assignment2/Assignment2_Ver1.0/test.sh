#!/bin/bash

diff="diff -iadwy" 

./test_bst.o -v > log_1

sed -n '/label1/,/label2/{/label1/b;/label2/b;p}' log_1 > u_tree.log
sed -n '/label3/,/label4/{/label3/b;/label4/b;p}' log_1 > b_tree.log

$diff u_tree.log b_tree.log

e_code=$?
if [ $e_code != 0 ]; then
    printf "Part 1 failed : %d\n" "$e_code"
else
    printf "Part 1 OK!\n"
fi

sed -n '$p' log_1
