# LI Operating Systems and Systems Programming
---
Mock Exam January 2023

## Note

Answer ALL questions. Each question will be marked out of 20. The paper will be marked out of 60, which will be rescaled to a mark out of 100.

## Question 1

The question is about pointers and memory management in C

1. Are there any problems when the following program executes? If so, explain what it leads to.
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

2. A programmer has written the following function with the aim to free an array of 10 random integers (int). There is a serious problem with this code. Explain what is wrong, why it is a problem, and how it can be fixed. Use this to write a correct version of the function without changing the function-signature. 
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

3. Consider the following two C functions `matrix1` and `matrix2`. Both of them store the values of the matrixs. Which one of them will be able to exploit memory hierarchy and thus achieve faster computation time in matrix multiplication? Explain your answer. 
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

## Question 2

1. The question is about Main and Virtual Memory. Provide a brief answer
   
   1. Where does `bootstrap` store and what's the function of it?

   2. In `process states`, under what conditions do the following occur:
      1. running to ready

      2. running to waiting

      3. waiting to ready

   3. What's the difference between `paging` and `segmentation`? List at least three differences.

   4. Consider  a  demand-paged  computer  system  where  the  degree  of multiprogramming  is  currently  fixed  at  4.  The  system  was  recently  measured  to  determine `utilization  of  CPU`  and  `the  paging  disk`.  The  results  are  one  of  the  following  alternatives.  For each case, what is happening? 
      1. CPU utilization 13 percent; disk utilization 97 percent 

      2. CPU utilization 87 percent; disk utilization 3 percent 

      3. CPU utilization 13 percent; disk utilization 3 percent 

2. Briefly describe the benifits of having microkernel in the kernel.

3. Consider the following piece of kernel code. The intention is that program processes according to the inputs`('I', 'S' or others)`. The device provides two functions: `show_table` will output the current values of counters, `increase_counter` will increase the counters by `1` when they are called. It should handle with concurrency.
    ```c
    #define BUFFERLENGTH 256
    #define INCREASE_COUNTER 'I'
    #define SHOW_COUNTER 'S'

    #define PROC_ENTRY_FILENAME "kernelWrite"



    DECLARE_RWSEM(counter_sem); /* semaphore to protect counter access */

    static struct proc_dir_entry *Our_Proc_File;

    int counter1 = 0;
    int counter2 = 0;

    /* displays the kernel table - for simplicity via printk */
    void show_table (void) {

        int tmp1;
        int tmp2;

        down_read (&counter_sem); /* lock for reading */
        tmp1 = counter1;
        tmp2 = counter2;
        up_read (&counter_sem); /* unlock reading */
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

## Question 3

1. What are the `criteria` to measure the success of shceduling in CPU? List at least four.

2. Predict all possible outputs that the following C program will print to the console and briefly explain your answer. What will be the state of parent process? Briefly explain the behaviour of the program if we add `sleep(30);` in `line number 30`.
    ```c
    #include <stdio.h>
    #include <stdlib.h>
    #include <unistd.h>
    #include <errno.h>

    int main () {
        pid_t pid;

        pid = vfork();
        if (pid == -1)    {   
          /* Error:
           * When fork() returns -1, an error happened
           * (for example, number of processes reached the limit).
           */
          fprintf(stderr, "can't fork, error %d\n", errno);
          exit(EXIT_FAILURE);
       }

        if (pid == 0) {
    	/* child process - just exec */
    	char *argv[2];
    	argv[0] = "helloWorld";
    	argv[1] = NULL;
    	execv ("helloWorld", argv);
    	fprintf(stderr, "can't execute, error %d\n", errno);
    	exit(1);
        }
        else {
    	/* parent process - just exit */
        /*Line 30*/
    	printf ("Executing parent process\n");
    	exit (0);
        }
    }
    ```

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
       signal(s);
       signal(q);
    }
    ```