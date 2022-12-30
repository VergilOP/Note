#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include <unistd.h>

#define __BST_H_

pthread_rwlock_t lock = PTHREAD_RWLOCK_INITIALIZER;

extern Node* root;

void DealWithCommand(char line[],char *clientCommands) {
	//Initialize the variable which used to store the commands and parameter
	char* commandP = strtok(line, " ");
	char* parameterP = strtok(NULL, " ");
	
	//Deal with different commands
	if (strcmp(commandP,"addNode") == 0) {
		//lock the thread, if locked, wait
		pthread_rwlock_wrlock(&lock);

		root = addNode(root, atoi(parameterP));
		printf("%s insertNode %d\n",clientCommands, atoi(parameterP));

		pthread_rwlock_unlock(&lock);
	}
	else if (strcmp(commandP,"removeNode") == 0) {
		//lock the thread, if locked, wait
		pthread_rwlock_wrlock(&lock);

		root = removeNode(root, atoi(parameterP));
		printf("%s deleteNode %d\n",clientCommands, atoi(parameterP));

		pthread_rwlock_unlock(&lock);
	}
	else if (strcmp(commandP,"countNodes") == 0) {
		//lock the thread, if locked, wait
		pthread_rwlock_rdlock(&lock);
		printf("%s countNodes = %d\n",clientCommands,countNodes(root));

		pthread_rwlock_unlock(&lock);
	}
	else if (strcmp(commandP,"avgSubtree") == 0) {
		//lock the thread, if locked, wait
		pthread_rwlock_rdlock(&lock);
		printf("%s avgSubtree = %f\n",clientCommands,avgSubtree(root));

		pthread_rwlock_unlock(&lock);
	}	
	else {
		printf("Invalid Command\n");
	}
}

void *downtime() {
	//Record the times
	int Times = 0;

	//If sleep 3 times, then break
	while (Times != 3){
		//lock the thread, if locked, wait
		pthread_rwlock_wrlock(&lock);

		Node* Temp = root;
		root = balanceTree(root);
		freeSubtree(Temp);
		Times++;

		pthread_rwlock_unlock(&lock);

		sleep(2);
	}
	return NULL;
}

void *ServeClient(char *clientCommands) {
	char line[30];
	FILE* fp = NULL;

	if ((fp = fopen(clientCommands, "r")) == NULL) {
		//If loading file failed, report ERROR
		printf("Error!!!\n");
		exit(1);
	}
	
	//Copy the ClientName, avoid overwrite
	char client[20];
	strcpy(client, clientCommands);
	char* clientName = strtok(client,"_");
	
	//Continue read line untill the end of file(eof)
	while (!feof(fp)) {
		//Read whole line of the file
		fscanf(fp, "%[^\n]", line);

		//To avoid repeated last line
		if(!feof(fp)){
			DealWithCommand(line,clientName);

			//Move to the next line
			fgetc(fp);
		}
	}
	

	//close file at the end
	fclose(fp);
	
	return NULL;
}

