# Resit Solution

## Quesiton 1

1. The following data structure in C defines dynamically allocated matrix:
    ```c
    1 struct matrix˙t {
    2 int * elems;
    3 int n_cols, n_rows;
    4 };
    ```
    Assume that the member elems points to a sufficiently large array to contain `n_rows*n_cols` int elements. Consider a function that sets all elements with the value val, implemented as follows:
    ```c
    5 void fill(struct matrix_t * m, int val){
    6 for (int j = 0; j < m->n_cols; j++)
    7   for (int i = 0; i < m->n_rows; i++)
    8       set(m, i, j, val);
    9 }
    ```
    The function fill relies on a function set with the following signature:
    ```c
    void set(struct matrix_t * m, int row, int col, int val);
    ```
    Provide a C based implementation of the set function that sets the element at coordinates row and col to value val and that makes fill be as cache efficient as possible.
    > ```c
    > void set(struct matrix_t *m, int row, int col, int val){
    >   m->elems[row * m->n_cols + col] = val;
    > }
    > ```
    > This implementation of the set function will be cache efficient because it accesses elements in a row-major order, which achieves spactial locality. The data will be loaded to cache and it will improve the performance

2. Show a valid memory layout and content on the stack for the variables a,b, and c at the end of execution of the following C code:
    ```c
    1 char a = 10;
    2 int b[4];
    3 int * c = b;
    4 while (c != b + 4)
    5 *(c++) = a++;
    6 printf("%lu\n", (unsigned long) b);
    ```
    Assume that the value printed on screen is 2000, that numbers are represented in little endian, and that we are on a 64 bits machine. Each cell of the grid below corresponds to one byte in memory, bytes are contiguous from left to right and continue in the lower grid. Note that the cells available below are more than necessary, so it is up to you to decide where to begin the sequence of bytes. For simplicity, write in each cell the decimal representation of the respective byte.
    > | 13 | 10 | 0 | 0 | 0 | 11 | 0 | 0 | 0 | 12 | 0 | 0 | 0 | 13 | 0 | 0 | 0 | The address of b (8 bytes because of 64 bits machine) |

3. Interpret the struct of point (a) as a C struct definition and consider the code below (assume N ROWS and N COLS are some global integer positive constants)
    ```c
    1 void f(struct matrix_t b) {
    2 // ...
    3 }
    4 
    5 int main() {
    6   struct matrix_t a;
    7   matrix_t_init(&a, N_ROWS, N_COLS);
    8   f(a);
    9   return 0;
    10 }
    ```

    1. Define a constructor with arguments int n rows, int n cols and the initialize function that allocates an appropriate amount of memory. Also, define an appropriate destructor that prevents the code above from leaking.
        > ```c
        > struct matrix_t{
        >     int* elems;
        >     int n_rows, n_cols;
        > }
        > 
        > void matrix_t_init(struct matrix_t *matrix, int n_rows, int n_cols) {
        >     matrix->elems = malloc(n_rows * n_cols * sizeof(int));
        >     matrix->n_cols = n_cols;
        >     matrix->n_rows = n_rows;
        > }
        > 
        > void matrix_t_destroy(struct matrix_t *matrix) {
        >     free(matrix->elems);
        > }
        > ```

    2. Define a copy constructor that ensures an appropriate pass-by-value to function f, that is, b equals to a when entering f but modifications to b do not affect a.
        > ```c
        > struct matrix_t_copy{
        >     int* elems;
        >     int n_rows, n_cols;
        > }
        > 
        > void matrix_t_copy_init(struct matrix_t *copy, struct matrix_t *matrix) {
        >     copy->elems = malloc(matrix.n_rows * matrix.n_cols * sizeof(int));
        >     memcpy(copy->elems, matrix->elems, matrix->n_rows * matrix->n_cols * sizeof(int));
        >     copy->n_cols = matrix->n_cols;
        >     copy->n_rows = matrix->n_rows;
        > }
        > ```

    3. Assuming that pass-by-value works as intended, describe how to modify the code above so that changes to b directly apply to a. Briefly, justify your answer.
        > ```c
        > struct matrix_t_copy{
        >     int* elems;
        >     int *n_rows, *n_cols;
        > }
        > 
        > void matrix_t_copy_init(struct matrix_t *copy, struct matrix_t *matrix) {
        >     copy->elems = matrix->elems;
        >     copy->n_cols = &matrix->n_cols;
        >     copy->n_rows = &matrix->n_rows;
        > }
        > ```

## Question 2

1. You have a multi-threaded application that is running on a multicore system. You have just added some code to implement mutual exclusion on some critical sections of the code. Now the system behaves correctly but real-time performance is 50% slower than before. Provide at least two reasons that are affecting performance.
    > 1. If multiple threads are trying to access the same critical section at the same time, they will have to wait for the lock to be released before they can enter. This can lead to a higher number of context switches, resulting in slower performance.
    > 2. When critical sections are protected by locks, only one thread can execute the critical section at a time. This reduces the number of parallelism in the system, leading to slower performace.

2. A computer lab is utilised to teach the Systems Programming module as well as perform CPU-intensive operations in the background for research purposes. The CPU was generally in use for nearly 100% of the time before updating the memory intensive IDE that is used for programming, and the response time was short. The response time has increased greatly since the change, and throughput has decreased significantly. Give a possible cause for this behaviour and a solution that doesn’t require any additional hardware.
    > One possible cause could be that the new version of the IDE is more memory intensive, causing more frequent swapping between the main memory and the hard disk. This would increase the amount of time the CPU spends waiting for data to be loaded from the hard disk, reducing its overall utilization.
    > One solution could be to optimize the memory usage of the IDE by increasing the size of the main memory. Which can reduce the swapping between the main memory and the hard disk.

3. The following `device_write` function is part of a device driver implementation for a character device which implements a simple way of message passing. The device driver writes to the device to store the message in kernel space and adds it to the list if the message is below the maximum size, and the limit of the size of all messages wouldn’t be surpassed with this message. If the message is too big, `-EINVAL` is returned, and if the limit of the size of all messages was surpassed, `-EAGAIN` is returned. This kernel code compiles correctly, but does not work as intended. Identify these errors and suggest remedies. If you think critical sections are required, provide an implementation of the critical section. Briefly justify your answer.
    ```c
    1   static ssize_t device_write(struct file * filp,
    2   const char __user * buff, size_t len, loff_t * off){
    3       char * kernelBuffer;
    4       struct msg_t * msg;
    5       printf(KERN_INFO "write %d\n", (int) len);
    6       
    7       if (len > MSGSIZE){
    8           return -1;
    9       }
    10
    11      // prepare new list element first
    12      kernelBuffer = malloc(len, GFP_KERNEL);
    13      if (!kernelBuffer){
    14          return -ENOMEM;
    15      }
    16      
    17      msg = malloc(sizeof(struct msg_t), GFP_KERNEL);
    18      if (!msg){
    19          kfree(kernelBuffer);
    20          return -ENOMEM;
    21      }
    22      
    23      msg -> buf = kernelBuffer;
    24      if (copy_to_user(msg -> buf, buff, len)){
    25          kfree(msg);
    26          kfree(kernelBuffer);
    27          return -EFAULT;
    28      }
    29      msg -> next = NULL;
    30      msg -> size = len;
    31      
    32      // add element to the list
    33      if (len + overall_size > max_size) {
    34          kfree(msg);
    35          kfree(kernelBuffer);
    36          return -1;
    37      }
    38      msg -> next = messages;
    39      messages = msg;
    40      if (lastMsg == NULL) lastMsg = msg;
    41      overall_size = overall_size + len;
    42      return len;
    43  }
    ```
    > 1. On line 8, the return value should be -EINVAL instead of -1.
    > 2. On line 24, the function `copy_to_user` is used to copy data from the kernel space to user space, but it should be `copy_from_user` to copy data from user space to kernel space.
    > 3. On line 36, the return value should be -EAGAIN instead of -1.
    > 4. On line 12 and 17, the function kfree is used to free memory allocated with malloc, but it should be used to free memory allocated with kmalloc. 
    > 
    > There is no critical section implemented to protect the shared variables overall_size and messages from concurrent access by multiple threads. One way to implement a critical section is to use a spinlock to prevent other threads from accessing these variables until the current thread has finished updating them.

## Question 3

1. Give three examples of when a context-switch between processes is performed. What are the actions taken by a kernel during the context-switch?
    > 1. Pre-empt: When a higher priority process becomes runnable and the current process is not finished executing.
    > 2. I/O or event: When the current process makes a system call or triggers some other event that causes the kernel to perform some action on its behalf
    > 3. Terminated: When the time slice allocated for the current process has expired and another process needs to be scheduled
    > 
    > During a context switch, the kernel performs the following actions:
    > 1. Saves the current process's register values and stack pointer
    > 2. Restores the register values and stack pointer for the new process
    > 3. Updates the process control block for the current process to reflect its new state (waiting, running)
    > 4. Updates the process control block for the new process to reflect its new state (running)
    > 5. Performs any necessary cleanup or initialization for the new process (resetting signal handlers)

2. Consider three compute bound processes with 10, 6 and 4 units of burst time. Let us assume that these processes arrive at 0, 2 and 6 units of time respectively. For the Shortest Remaining Time First (SRTF), how many context switches are needed? Do not consider the context switches at time 0 or at the end.
    > There would be 3 context switches needed for SRTF.   
    > The `first` context switch would occur at time 2 when the second process arrives.  
    > The `second` context switch would occur at time 8 when the second process finished and the third process starts.   
    > The `third` and final context switch would occur at time 12 when the third process finishes executing.
    > 
    > Time: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20   
    > Process: P1 P1 P2 P2 P2 P2 P2 P2 P3 P3 P3 P3 P1 P1 P1 P1 P1 P1 P1 P1

3. The C program shown below is compiled and run on a UNIX machine. Modify the program such that instead of the parent, the child process executes the execlp() while the parent process waits for the child process to complete. Provide the updated code.
    ```c
    1 #include ¡sys/types.h¿
    2 #include ¡sys/wait.h¿
    3 #include ¡stdio.h¿
    4 #include ¡unistd.h¿
    5   int main()
    6   {
    7       int pid;
    8       /* fork a child process */
    9       pid = fork();
    10      if (pid ¡ 0) { /* error occurred */
    11          fprintf(stderr, ”Fork Failed”);
    12          return 1;
    13      }else{
    14          /* Parent Process */
    15          execlp(”/bin/ls”,”ls”,NULL);
    16      }
    17      return 0;
    18  }
    ```
    > ```c
    > #include <sys/types.h>
    > #include <sys/wait.h>
    > #include <stdio.h>
    > #include <unistd.h>
    > int main()
    > {
    >     int pid;
    >     /* fork a child process */
    >     pid = fork();
    >     if (pid < 0) { /* error occurred */
    >         fprintf(stderr, "Fork Failed");
    >         return 1;
    >     } else if (pid == 0) { /* Child Process */
    >         execlp("/bin/ls", "ls", NULL);
    >     } else { /* Parent Process */
    >         wait(NULL);
    >     }
    >     return 0;
    > }
    > ```

4. Consider a concurrent system with three processes A, B and C. The system receives multiple requests, and places them in a request queue that is accessible by all the three processes A, B, and C. For each request, we would like to enforce an order such that the request must first be processed by A, then C, then A again and finally B before it can be removed and discarded from the queue. A (semaphore based) solution to synchronize A, B and C is given in (Table 1). Discuss whether the proposed solution is correct. If not, then provide a deadlock free solution.
    | Process A | Process B | Process C |
    | --- | --- | --- |
    | signal(a1done) | wait(a1done) | signal(cdone) |
    | signal(a2done) | wait(a2done) | |
    > The proposed solution is incorrect because it is possible for Process C finishes before Process A finished which doesn't achieve the given sequence  
    > It should be:
    > | Process A | Process B | Process C |
    > | --- | --- | --- |
    > | signal(a1done) | wait(a2done) | wait(a1done) |
    > | wait(cdone) |  | signal(cdone)|
    > |signal(a2done)| | |

5. Consider a system with three frames of memory, and the following sequence of page accesses: 1,2,3,4,1,2. When do page faults occur using FIFO, LRU and Optimal Page replacement algorithms? Briefly justify your answer.
    > For FIFO:
    > | 1    | 2    | 3    | 4    | 1    | 2    |
    > |------|------|------|------|------|------|
    > | 1    | 1    | 1    | 4    | 4    | 3    |
    > |      | 2    | 2    | 2    | 1    | 1    |
    > |      |      | 3    | 3    | 3    | 2    |
    > | Miss | Miss | Miss | Miss | Miss | Miss |
    > There are 6 page faults
    >
    > For LRU:
    > | 1    | 2    | 3    | 4    | 1    | 2    |
    > |------|------|------|------|------|------|
    > | 1    | 1    | 1    | 4    | 4    | 3    |
    > |      | 2    | 2    | 2    | 1    | 1    |
    > |      |      | 3    | 3    | 3    | 2    |
    > | Miss | Miss | Miss | Miss | Miss | Miss |
    > There are 6 page faults
    >
    > For Optimal:
    > | 1    | 2    | 3    | 4    | 1   | 2   |
    > |------|------|------|------|-----|-----|
    > | 1    | 1    | 1    | 1    | 1   | 1   |
    > |      | 2    | 2    | 2    | 2   | 2   |
    > |      |      | 3    | 4    | 3   | 2   |
    > | Miss | Miss | Miss | Miss | Hit | Hit |
    > There are 4 page faults