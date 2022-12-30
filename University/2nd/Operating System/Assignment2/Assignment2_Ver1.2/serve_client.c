#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include <unistd.h>

#define __BST_H_

extern Node* root;
int count = 1;

void DealWithCommand(char line[],char *clientCommands) {
	//Initialize the variable which used to store the commands and parameter
	char command[20];
	char parameter[20];
	char* commandP = command;
	char* parameterP = parameter;

	//split a line to command and parameter(if it has)
	commandP = strtok(line, " ");
	parameterP = strtok(NULL, " ");
	
	//Deal with different commands
	if (strcmp(commandP,"addNode") == 0) {
		root = addNode(root, atoi(parameterP));
		printf("[%s] insertNode %d\n",clientCommands, atoi(parameterP));
	}
	else if (strcmp(commandP,"removeNode") == 0) {
		root = removeNode(root, atoi(parameterP));
		printf("[%s] deleteNode %d\n",clientCommands, atoi(parameterP));
	}
	else if (strcmp(commandP,"countNodes") == 0) {
		printf("[%s] countNodes = %d\n",clientCommands,countNodes(root));
	}
	else if (strcmp(commandP,"avgSubtree") == 0) {
		printf("[%s] avgSubtree = %f\n",clientCommands,avgSubtree(root));
	}	
	else {
		printf("Invalid Command\n");
	}
}

void *downtime() {
	//balanceTree();
}

void *ServeClient(char *clientCommands) {
	char line[100];

	FILE* fp = NULL;

	if ((fp = fopen(clientCommands, "r")) == NULL) {
		//If loading file failed, report ERROR
		printf("Error!!!\n");
		exit(1);
	}
	
	//Continue read line untill the end of file(eof)
	while (!feof(fp)) {
		//Read whole line of the file
		fscanf(fp, "%[^\n]", line);

		//To avoid repeated last line
		if(!feof(fp)){
			DealWithCommand(line,clientCommands);

			//Move to the next line
			fgetc(fp);
		}
	}
	
	//close file at the end
	fclose(fp);
}

