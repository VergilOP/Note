# LI Operating Systems and Systems Programming
---
Mock Exam January 2023

## Note

Answer ALL questions. Each question will be marked out of 20. The paper will be marked out of 60, which will be rescaled to a mark out of 100.

## Question 1

The question is about pointers and memory management in C

1. Are there any problems when the following program executes? If so, explain what it will cause for the list of available memory blocks.
    ```c
    int main()
    {
        int *A = (int *) malloc(sizeof(int));
        scanf("%d",A);
        free(A);
        free(A);
        return 0;
    }
    ```
    > There are double free problem for `A`  
    > The address of `A` will be added in the list of available memory blocks twice and the next two memory allocations will have the same address

2. A programmer has written the following function with the aim to free an array of 10 random integers (int). There is a serious problem with this code. Explain what is wrong, and how it can be fixed. Use this to write a correct version of the function without changing the function-signature and function `randomArray`.
    ```c
    int* randomArray(void)
    {
        int *array, i;
        array = (int *) malloc(10*sizeof(int));
        for(i = 0; i<10; ++i)
            array[i] = rand();
        return array;
    }

    int main()
    {
        int *foo(void);
        int *array,i;

        foo = &randomArray;
        array = foo();
        free(array);
        return 0;
    }
    ```
    > `int *foo(void);` is used incorrectly, it is `the function returns the pointer of type int` but not `the pointer of function that returns pointer of int`  
    > it should be `int* (*foo)(void);`

3. Consider the following two C functions `matrix1` and `matrix2`. Both of them store the values of the matrixs. Which one of them will be able to exploit memory hierarchy and thus achieve faster computation time in matrix multiplication? Explain your answer. The matrixs are entered in row-major order
    ```c
    void matrix1(void)
    {
        int A[5][5], B[5][5], i, j;

        for (i = 0; i < 5; i++) {
            for (j = 0; j < 5; j++)
                scanf("%d", &A[i][j]);
        }

        for (i = 0; i < 5; i++) {
            for (j = 0; j < 5; j++)
                scanf("%d", &B[i][j]);
        }

        matrixMultiplication(A,B);
    }
    ```

    ```c
    void matrix2(void)
    {
        int A[5][5], B[5][5], i, j;

        for (i = 0; i < 5; i++) {
            for (j = 0; j < 5; j++)
                scanf("%d", &A[i][j]);
        }

        for (i = 0; i < 5; i++) {
            for (j = 0; j < 5; j++)
                scanf("%d", &B[j][i]);
        }

        matrixMultiplication(A,B);
    }
    ```

    Here is the `matrixMultiplication()`

    ```c
    void matrixMultiplication(int a[][5], int b[][5])
    {
        int c[5][5], i, j, k;

        for (i = 0; i < 5; i++)
            for (j = 0; j < 5; j++) {
                c[i][j] = 0;
                for (k = 0; k < 5; k++)
                    c[i][j] += a[i][k] * b[k][j];
            }

        printf("Matrix result:\n");
        for (i = 0; i < 5; i++) {
            for (j = 0; j < 5; j++) {
                printf("%d\t", c[i][j]);
            }
            printf("\n");
        }
    }
    ```
    > Modern computers with memory hierarchy try to speedup computation by applying the principles of temporal and spatial locality. Thus, when a program tries to read one int object from the main memory, other adjacent int objects are also brought to the cache.  
    > The function `matrix1` stores the matrix elements of `A` and `B` along the rows. Whereas `matrix2` stores the matrix elements of `A` along the rows and the matrix elements of `B` along the columns.  
    > Since, the `matrixMultiplication` reads matrix `A` in the row-major order and matrix `B` in the row-major order to achieve multiplication, `matrix2` offers better spatial locality  compared to `matrix1`, and thus offers better performance.  
    > Hence, `matrix2` will be more efficient  

## Question 2

1. The question is about Main and Virtual Memory. Provide a brief answer
   
   1. Where does `bootstrap` store and what's the function of it?
        > Bootstrap typically stored in ROM or EPROM, generally known as firm ware  
        > It will initializes all aspects of the system

   2. In `process states`, under what conditions do the following occur:
      1. running to ready
            > pre-empted

      2. running to waiting
            > I/O or event wait

      3. waiting to ready
            > I/O or event completion

   3. What's the difference between `paging` and `segmentation`? List at least three differences.
        > **In individual Memory**
        > - In Paging, we break a process address space into blocks known as pages.
        > - In the case of Segmentation, we break a process address space into blocks known as sections.
        > 
        > **Memory Size**
        > - The pages are blocks of fixed size.
        > - The sections are blocks of varying sizes.
        > 
        > **Speed**	
        > - This technique is comparatively much faster in accessing memory.
        > - This technique is comparatively much slower in accessing memory than Paging.
        >
        > **Size**
        > - The available memory determines the individual page sizes.
        > - The user determines the individual segment sizes.
        > 
        > **Fragmentation**
        > - The Paging technique may underutilize some of the pages- thus leading to internal fragmentation.
        > - The Segmentation technique may not use some of the memory blocks at all. Thus, it may lead to external fragmentation.

   4. Consider  a  demand-paged  computer  system  where  the  degree  of multiprogramming  is  currently  fixed  at  4.  The  system  was  recently  measured  to  determine `utilization  of  CPU`  and  `the  paging  disk`.  The  results  are  one  of  the  following  alternatives.  For each case, what is happening? 
      1. CPU utilization 13 percent; disk utilization 97 percent 
            > Thrashing: the CPU is `swapping pages` to and from disk.

      2. CPU utilization 87 percent; disk utilization 3 percent 
            > At least one `CPU-intensive` programming is running.  

      3. CPU utilization 13 percent; disk utilization 3 percent 
            > `Underused` system with spare capacity.  

2. Briefly describe the benifits and drawbacks of having microkernel in the kernel.
    > - Easier to `develop microkernel extensions`
    > - Easier to `port` the operating system `to new architectures`
    > - More `reliable` (less code is running in kernel mode) - if a device driver fails, it `can be re-loaded`
    > - More `secure`, since kernel is `less-complex` and therefore less likely to have security holes.
    > - The system can `recover from a failed device driver`


3. Consider the following piece of kernel code. The intention is that program processes according to the inputs`('I', 'S' or others)`. The device provides two functions: `show_table` will output the current values of counters, `increase_counter` will increase the counters by `1` when they are called. It should handle with concurrency.
    ```c
    #define BUFFERLENGTH 256
    #define INCREASE_COUNTER 'I'
    #define SHOW_COUNTER 'S'

    #define PROC_ENTRY_FILENAME "kernelWrite"

    static struct proc_dir_entry *Our_Proc_File;

    int counter1 = 0;
    int counter2 = 0;

    /* displays the kernel table - for simplicity via printk */
    void show_table (void) {
        int tmp1;
        int tmp2;
        tmp1 = counter1;
        tmp2 = counter2;
        printk (KERN_INFO "kernelWrite:The counters are %d, %d\n", tmp1, tmp2);

    }

    void increase_counter (void) {
        counter1++;
        counter2++;
    }

    /* This function reads in data from the user into the kernel */
    ssize_t kernelWrite (struct file *file, const char __user *buffer, size_t count, loff_t *offset) {

        printk (KERN_INFO "kernelWrite entered\n");
    
        switch (buffer) {
          case INCREASE_COUNTER:
        	increase_counter ();
        	break;
          case SHOW_COUNTER:
        	show_table ();
            break;
        }
        return count;
    }
    ```
    This kernel code compiles incorrectly. Identify these errors and suggest remedies. If you think critical sections are required, it is sufficient to indicate begin and end of a critical section, and whether you would use semaphores or spinlocks to protect the critical section.
    > The errors and fixes are:
    > - It doesn't handle with invalid input in `switch` in `kernelWrite`.     
    > - The data must be copied from `buffer`, which is in user space, to a buffer in kernel space eg. via `get_user`.
    > - If a critical section happens within `increase_counter` or `show_table`, spinlocks must be used to protect this critical section. If a critical section happens within `kernelWrite`, semaphores should be used.
    > 
    > Other solutions are OK as long as they address the problems.

## Question 3

1. What are the `criteria` to measure the success of shceduling in CPU? List at least four.
    > - CPU utilisation
	> - Throughput: Number of processes completed within a given time
	> - Turnaround time: Time it takes for each process to be executed
	> - Waiting time: Amount of time spent in the ready-queue
	> - Response time: time between submission of request and production of first response

2. Predict all possible outputs that the following C program will print to the console and briefly explain your answer. What will be the state of parent process? Briefly explain the behaviour of the program if we comment out the line number 29.
    ```c
    #include <stdio.h>
    #include <stdlib.h>
    #include <unistd.h>
    #include <errno.h>

    int main () {
        pid_t pid;

        pid = vfork();
        if (pid == -1)    {   
            fprintf(stderr, "can't fork, error %d\n", errno);
            exit(EXIT_FAILURE);
        }

        if (pid == 0) {
            while (i < 10) {
                printf("child: %d\n", i);
	            i++;
            }
            sleep(10);
            exit(0);  
        }
        else {
            int status;
            while (i < 10) {
                printf("parent: %d\n", i);
	            i++;
            }
            wait (&status); /* Line 29 */
            printf("PARENT: Child's exit code is: %d\n", WEXITSTATUS(status));
            exit(0);
        }
        return 0;
    }
    ```
    > If the fork() succeeds, `child: ..` will be printed ten times and `PARENT: Child's exit code is: ..` will be printed at the end. If the fork() fails, `can't fork, error ..` will be printed. Parent process will be in wait state.   
    > If we comment out line 29, the child process is still executing after the parent is completed(orphan process).

3. Your computer system uses `First-Come, First-Served(FCFS)` scheduler. What will happen to performance for these two possible situations and explain why?

    | Process | Type      | Arrival time | Burst time |
    |---------|-----------|--------------|------------|
    | P1      | CPU Bound | 0            | 2          |
    | P2      | I/O Bound | 1            | 1          |
    | P3      | I/O Bound | 2            | 2          |
    | P4      | CPU Bound | 3            | 1          |

    | Process | Type      | Arrival time | Burst time |
    |---------|-----------|--------------|------------|
    | P1      | CPU Bound | 0            | 200        |
    | P2      | I/O Bound | 1            | 180        |
    | P3      | I/O Bound | 2            | 210        |
    | P4      | CPU Bound | 3            | 190        |
    > For the first situation, performance will decrease because of the too many context switches  
    > For the second situation, performance will also decrease as process can monopolise CPU which means starvation for other processes.

4. Consider a concurrent system with two processes `p0` and `p1`. Assume s and q are the semaphore. Describe what the problem is for this code and provide a solution to fix the error.
    ```
    Semaphore s=1, q=1
    
    process p0 {     
       wait(s);
       wait(q);
       ...        
       signal(s);
       signal(q);
    }
    
    process p1 {     
       wait(q);
       wait(s);
       ...        
       signal(q);
       signal(s);
    }
    ```
    > Both thread get stalled indefinitely, so there is deadlock.  
    > ```
    > Semaphore s=1, q=1
    > 
    > process p0 {     
    >    wait(s);
    >    wait(q);
    >    ...        
    >    signal(s);
    >    signal(q);
    > }
    > 
    > process p1 {     
    >    wait(s);
    >    wait(q);
    >    ...        
    >    signal(s);
    >    signal(q);
    > }
    > ```