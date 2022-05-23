# Exercise Sheet 1 Math

## 1.1 Prove by induction that 2a < a! for all natural numbers a greater than 3.

2<sup>a</sup> < a!  
**P<sub>1</sub>:**  
when a = 4  
2<sup>4</sup> = 16 and 4! = 24  
16 < 24  
It is true for a = 4
**P<sub>k</sub>:**  
assume that a = k, 2<sup>k</sup> < k!  
**P<sub>k+1</sub>:**  
when a = k + 1  
2<sup>k+1</sup> = 2 x 2<sup>k</sup>  
(k+1)! = k! x (k+1)  
cus 2<sup>k</sup> < k! and 2 < (k + 1) as start from 4  
so 2<sup>k+1</sup> < (k+1)!  
**P<sub>k</sub>** => **P<sub>k+1</sub>**  
It is true for all a >= 4  
It is true for all natural numbers a greater than 3

## 1.2 Use the ring laws and the cancellation law for multiplication to derive the following statement:  If a×b = 0 then a = 0 or b = 0

for a x b = 0  
if a ≠ 0  
   a x b = a x 0 = 0  
   as the multiplicative cancellation  
   b = 0  
else a = 0  
   a x b = 0 x b = 0  
   it is true  
so If a x b = 0 then a = 0 or b = 0

## 1.3 In the lectures we have tried to pin down the properties of addition and multiplication on the natural numbers and the integers. For both sets of numbers we also have a notion of comparison, written as a ≤ b. Try to find the core properties of this order relation and how it interacts with addition and multiplication. Here is an example: If a ≤ b and b ≤ a then a = b

if a + c ≤ b + c and b + c ≤ a + c then a = b  
if a·c ≤ b·c and b·c ≤ a·c and c is non-zero then a = b

## 1.4 In this exercise we explore a way of defining the integers from the natural numbers, inspired by the real-world example of “savings” versus “debt”.

### (a) An integer is defined as a pair (c,d) of natural numbers (which, if we had subtraction already, we would think of as c − d). When answering the following questions, use only addition, multiplication, and comparison of natural numbers.

#### (i) When would you say that a pair (c,d) represents a positive integer (i.e., you are in credit)?

if (c,d) is a positive integer  
(c,d) > 0  
c - d > 0  
c > d

#### (ii) When would you say that a pair (c,d) represents a negative integer (i.e., you are in debt)?

if (c,d) is a negative integer  
(c,d) < 0  
c - d < 0  
c < d

#### (iii) When would you say that a pair (c,d) represents zero?

if (c,d) is 0  
(c,d) = 0  
c - d = 0  
c = d

### (b) Given two pairs (c,d) and (c0 ,d0 ), when would you say that they represent the same integer? When would you say that (c,d) is less than or equal to (c0 ,d0 )? (Remember to only use addition in your answer, not subtraction.)

(c,d) = (c',d')  
c - d = c' - d'  
c + d' = c' + d

### (c) Using only addition and multiplication of natural numbers, define the operations

#### (i) addition

(c,d) + (c',d') = (c+c',d+d')

#### (ii) negative, and

Negative of (c,d) is (d,c)  
(c,d) + (d, c) = 0  
(c,d) + (d, c) = (c+d,c+d)

#### (iii) multiplication.

(c,d) x (c',d')  
= (c-d) x (c'-d')  
= cc' - cd' - dc' - dd'  
= (cc'+dd', cd'+c'd)





