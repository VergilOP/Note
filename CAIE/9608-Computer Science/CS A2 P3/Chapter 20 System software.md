# Table of contents

- [System software](#system-software)
  - [Explain how `RPN` is used by an interpreter to evaluate expressions. \[2\]](#explain-how-rpn-is-used-by-an-interpreter-to-evaluate-expressions-2)
  - [Complete the statements about a `virtual machine` \[4\]](#complete-the-statements-about-a-virtual-machine-4)
  - [What is meant by `Page` \[1\]](#what-is-meant-by-page-1)
  - [What is meant by `Page frame` \[1\]](#what-is-meant-by-page-frame-1)
  - [What is meant by `Page table` \[1\]](#what-is-meant-by-page-table-1)
  - [Explain why an operating system needs to use `scheduling algorithms` \[3\]](#explain-why-an-operating-system-needs-to-use-scheduling-algorithms-3)
  - [What is meant by an `interrupt` \[1\]](#what-is-meant-by-an-interrupt-1)

System software
---------------

### Explain how `RPN` is used by an interpreter to evaluate expressions. \[2\]
> s20_33_Q6

- Expressions are always evaluated left to right
- Each operator uses the two previous values on the stack
- Popping two previous values and pushing the new value on the previous value

### Complete the statements about a `virtual machine` \[4\]
> w20_33_Q5

- A virtual machine is ***Software/a program*** that emulates a ***Physical/different*** computer system
- A virtual machine allows multiple ***Guest*** operating systems to run on one computer using a ***Host*** operating system

### What is meant by `Page` \[1\]
> w19_33_Q6

- Virtual Memory is divided into blocks of a fixed size

### What is meant by `Page frame` \[1\]
> w19_33_Q6

- the main memory is divided into page frames of the same size as a page

### What is meant by `Page table` \[1\]
> w19_33_Q6

- the Page table shows the mapping of pages to page frames

### Explain why an operating system needs to use `scheduling algorithms` \[3\]
> w19_33_Q6

- To allow multiprogramming to take place
- To ensure fair usage of the processor
- To ensure fair usage of peripherals
- To ensure fair usage of memory
- To ensure higher priority tasks are executed sooner
- To ensure all processes have the opportunity to finish

### What is meant by an `interrupt` \[1\]
> w19_33_Q6

- A **signal** from a software **source** or hardware device seeking the **attention of the processor**


