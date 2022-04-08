# Table of contents

- [Processor and computer architecture](#processor-and-computer-architecture)
  - [What is `RISC(Reduced instruction set computer)` \[1\]](#what-is-riscreduced-instruction-set-computer-1)
  - [What is `CISC(Complex instruction set computer)` \[1\]](#what-is-cisccomplex-instruction-set-computer-1)
  - [What is `Pipelining` \[1\]](#what-is-pipelining-1)
  - [What is meant by `MISD` \[2\]](#what-is-meant-by-misd-2)
  - [What is meant by `SIMD` \[2\]](#what-is-meant-by-simd-2)
  - [What is meant by `SISD` \[2\]](#what-is-meant-by-sisd-2)
  - [What is meant by `MIMD` \[3\]](#what-is-meant-by-mimd-3)
  - [State three characteristics of massively `parallel computers` \[3\]](#state-three-characteristics-of-massively-parallel-computers-3)
  - [Explain why code optimisation is necessary \[2\]](#explain-why-code-optimisation-is-necessary-2)

Processor and computer architecture
-----------------------------------

### What is `RISC(Reduced instruction set computer)` \[1\]
> s20_33_Q5

- A processor with a few simple fixed-length instructions that have a few instruction formats is called a **RISC** processor

### What is `CISC(Complex instruction set computer)` \[1\]
> s20_33_Q5

- A processor with many complex variable-length instructions that has many instruction formats is called a **CISC** processor

### What is `Pipelining` \[1\]
> s20_33_Q5

- Instruction-level parallelism, applied to the execution of instructions during the fetch-execute cycle, is called **Pipelining**

### What is meant by `MISD` \[2\]
> w19_33_Q9

- There are several processors
- Each processor executes different sets of instructions on one set of data at the same time

### What is meant by `SIMD` \[2\]
> w19_33_Q9

- The processor has several ALUs
- Each ALU executes the same set of instructions on different sets of data at the same time

### What is meant by `SISD` \[2\]
> w19_33_Q9

- There is only one processor \[2\]
- The processor executes one set of instructions on one set of data

### What is meant by `MIMD` \[3\]
> w19_33_Q9

- There are several processors
- Each processor executes a different set of instructions
- Each processor operates on different sets of data

### State three characteristics of massively `parallel computers` \[3\]
> w19_33_Q9

- A large number of processors
- Collaborative processing
- Network infrastructure
- Communication using a message interface

### Explain why code optimisation is necessary \[2\]
> s19_33_Q4

- Optimisation means that the code will have fewer instructions
- Optimised code occupies less space in memory
- Fewer instructions reduces the execution time of the program

