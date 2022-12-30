#define RAND_PERIOD 65536
unsigned int *rand_array;

int init_rand(){
	int i;
	unsigned int r1, r2, temp;
	rand_array = (unsigned int*) malloc(RAND_PERIOD*sizeof(unsigned int));
	
	for(i=0; i<RAND_PERIOD; i++)
		rand_array[i] = i;
	
	// Shuffle array
	for(i=0; i<RAND_PERIOD; i++){
		r1 = rand() % RAND_PERIOD;
		r2 = rand() % RAND_PERIOD;
		temp = rand_array[r1];
		rand_array[r1] = rand_array[r2];
		rand_array[r2] = temp;
	}
	return 0;
}

int destroy_rand(){
	free(rand_array);
	return 0;
}

unsigned int unique_random_number(){
	static int rand_array_index=0;
	unsigned int temp;
	temp = rand_array[rand_array_index];
	rand_array_index++;
	return temp;
}
