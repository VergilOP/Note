# Table of contents

- [Data Representation](#data-representation)
  - [Explain why `user-defined date types` are necessary \[2\]](#explain-why-user-defined-date-types-are-necessary-2)
  - [State **two** other `composite data types` \[2\]](#state-two-other-composite-data-types-2)
  - [`File organisation`](#file-organisation)
  - [Explain why ... cannot be accurately represented when increasing `Mantissa` and decreasing `Exponent` \[3\]](#explain-why--cannot-be-accurately-represented-when-increasing-mantissa-and-decreasing-exponent-3)
  - [State the effect of increasing the size of the `exponent` \[1\]](#state-the-effect-of-increasing-the-size-of-the-exponent-1)
  - [State why some `binary representations` can lead to rounding errors \[1\]](#state-why-some-binary-representations-can-lead-to-rounding-errors-1)
  - [Describe one problem that can occur when `floating-point numbers` are not normalised \[2\]](#describe-one-problem-that-can-occur-when-floating-point-numbers-are-not-normalised-2)
  - [Explain the trade-off between using either a large number of bits for the `mantiss`, or a large number of bits for the `exponent` \[3\]](#explain-the-trade-off-between-using-either-a-large-number-of-bits-for-the-mantiss-or-a-large-number-of-bits-for-the-exponent-3)
  - [What is `Overflow` \[1\]](#what-is-overflow-1)
  - [What is `Underflow` \[1\]](#what-is-underflow-1)

Data Representation
------------------------------

### Explain why `user-defined date types` are necessary \[2\]
> w19_33_Q5

- No suitable data type is provided by the language used
- The programmer needs specify a new data type
- ...that meets the requirements of the application

### State **two** other `composite data types` \[2\]
> w19_33_Q5

- Array
- List
- Set
- Collection
- Class
- Stack
- Queue
- Linked list
- Dictionary

### `File organisation`
> s20_33_Q2

- Sequential: Master file for paying each employee every month
- Random: Customer username and password file
- Serial: Text file, File for recording the temperature every hour

### Explain why ... cannot be accurately represented when increasing `Mantissa` and decreasing `Exponent` \[3\]
> s20_33_Q1

- Exponent too large to fit in ... bits as a two's complement number
- Exponent will turn negative
- ...therefore, point moves the wrong way
- Value will be approx ...

### State the effect of increasing the size of the `exponent` \[1\]
> w19_33_Q1

- Only the range is increased/no effect on precision

### State why some `binary representations` can lead to rounding errors \[1\]
> w19_33_Q1

- There is no exact binary conversion for some numbers
- More bits are needed to store the number than are available

### Describe one problem that can occur when `floating-point numbers` are not normalised \[2\]
> w20_33_Q1

- Precision lost
- Redundant leading zeros in the mantissa
- Bits lost off right-hand end
- Multiple representations of a single number

### Explain the trade-off between using either a large number of bits for the `mantiss`, or a large number of bits for the `exponent` \[3\]
> s19_33_Q1

- The trade-off is between range and precision
- Any increase in the number of bits for the mantissa, means fewer bits available for the exponent
- More bits used for the mantissa will result in better precision
- More bits used for the exponent will result in a large range of numbers
- Fewer bits used for the mantissa will result in worse precision
- Fewer bits used for the exponent will result in a smaller range of numbers

### What is `Overflow` \[1\]
> w19_33_Q1

- ***Overflow*** can occur in the exponent of a floating-point number, when the exponent has become too large to be represented using the number of bits available.

### What is `Underflow` \[1\]
> w19_33_Q1

- A calculation results in a number so small that is cannot be represented by the number of bits available. This is called ***Underflow***.

