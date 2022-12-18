
#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include <unistd.h>

#include "bst.h"

Node *root=NULL;
Node *root_balanced=NULL;

#include "unique_rng.c"
#include "serve_client.c"

#ifdef CYCLE_TEST
#include "cpucycles.c"
#endif

int f_verbose = 0;

void clean(){
  /*****************************************************/
  /******   Free resources before program ends  ********/
  /*****************************************************/

  root=freeSubtree(root);
  root_balanced = freeSubtree(root_balanced);
  root= NULL;
  root_balanced = NULL;
  return;
}

#ifdef CYCLE_TEST
// DO NOT add any test functions here, as they will not be compiled
void test_cycle(){
  unsigned int COUNTER, REPEAT=1000;
  unsigned int i, r;
  float avg, avg_balanced;

  init_rand();

  // Create a random BST rooted at 'root'
  for(i=0; i<10000; i++){
    r = unique_random_number();
    root=addNode(root, r);
  }

  long int CLOCK_total, CLOCK_end, CLOCK_start;
  // Create a balanced BST from the unbalanced BST
  root_balanced = balanceTree(root);


  // Cycle count for avg on unbalanced BST
  CLOCK_total=0;
  for(COUNTER=0; COUNTER<REPEAT; COUNTER++)
    {
      CLOCK_start = cpucycles();
      avg = avgSubtree(root);
      CLOCK_end = cpucycles();
      CLOCK_total = CLOCK_total + CLOCK_end - CLOCK_start;
    }
  printf("Avg cycle count for unbalanced BST = %ld\n", CLOCK_total/REPEAT);
  printf("Avg of unbalanced BST = %f\n", avg);

  // Cycle count for avg on balanced BST
  CLOCK_total=0;
  for(COUNTER=0; COUNTER<REPEAT; COUNTER++)
    {
      CLOCK_start = cpucycles();
      avg_balanced = avgSubtree(root_balanced);
      CLOCK_end = cpucycles();
      CLOCK_total = CLOCK_total + CLOCK_end - CLOCK_start;
    }
  printf("Avg cycle count for balanced BST = %ld\n", CLOCK_total/REPEAT);
  printf("Avg of balanced BST = %f\n", avg_balanced);

  printf("Difference in avgs = %f\n", avg - avg_balanced);

  clean();

}
#else
void test_task12(){
  unsigned int i,r;
  float avg, avg_balanced;
  const float epsilon = 0.001;
  float failed = 0;
  init_rand();
  // Create a random BST rooted at 'root'
  for(i=0; i<10000; i++){
    r = unique_random_number();     // This will give you the same set of random numbers every time
    root=addNode(root, r);
  }

  /*****************************************************/
  /******   Part 1 of Exercise 2 Starts here    ********/
  /*****************************************************/

  printf("/******** TEST OF PART 1 ********/\n\n");

  // Create a balanced BST from the unbalanced BST
  root_balanced = balanceTree(root);

  // Avg on unbalanced BST

  avg = avgSubtree(root);

  printf("Avg of unbalanced BST = %f\n", avg);

  // Sum on balanced BST
  avg_balanced = avgSubtree(root_balanced);

  printf("Avg of balanced BST = %f\n", avg_balanced);

  printf("Difference in avgs = %f\n", avg - avg_balanced);
  destroy_rand();

  failed = (avg - avg_balanced) + (avg - 28906.039062);
  if(((failed > epsilon) || (failed < -epsilon)) && f_verbose)
    {
      printf("\n/******** START DEBUG MODE *********/\n");

      printf("\nUnbalanced tree:\n");
      printf("label1\n");
      displaySubtree(root);
      printf("label2\n");
      printf("\nBalanced tree:\n");
      printf("label3\n");
      displaySubtree(root_balanced);
      printf("label4\n");
      printf("\n/******** END DEBUG MODE ********/\n");
    }

  printf("\n /******** END OF PART 1 ********/\n\n");

  clean();
}

void test_task34(){

  const float epsilon = 0.001;
  printf("/******** TEST OF PART 2 ********/\n\n");
  unsigned int i;
  char *client_names[5] = {"client1_commands", "client2_commands", "client3_commands",
                           "client4_commands", "client5_commands"};

  pthread_t threads[6];

  /*****************************************************/
  /******   Part 2 of Exercise 2 Starts here    ********/
  /*****************************************************/

  // spawn all threads
  pthread_create(&threads[0], NULL, (void *) ServeClient, client_names[0]);
  pthread_create(&threads[1], NULL, (void *) ServeClient, client_names[1]);
  pthread_create(&threads[2], NULL, (void *) ServeClient, client_names[2]);
  pthread_create(&threads[3], NULL, (void *) ServeClient, client_names[3]);
  pthread_create(&threads[4], NULL, (void *) ServeClient, client_names[4]);
  pthread_create(&threads[5], NULL, (void *) downtime, NULL);


  // join all readers
  for (i = 0; i < 6; i++) {
    pthread_join(threads[i], NULL);
  }

  // The tree should only have one node now
  int count = countNodes(root);
  float avg = avgSubtree(root);
  printf("count: %d\n", count);
  printf("avg: %f\n", avg);
  if (count == 1 && ((avg - 1) < epsilon) && ((avg - 1) > -epsilon)){
    printf("Test for Part2 seems OK\n");
  } else{
    printf("Test for Part2 fail\n");
  }

  // Free the tree
  clean();
}

// TODO: You could add more test functions here

#endif
int main(int argc, char *argv[]){

  if(argc == 2){
    if(strcmp(argv[1],"-v") == 0)
      {
        f_verbose = 1;
      }
  }

#ifdef CYCLE_TEST
  // DO NOT add any test functions here, as they will not be compiled
  test_cycle();
#else
  test_task12();
  test_task34();

  // TODO: You could call your test functions at here

#endif
  return 0;
}
