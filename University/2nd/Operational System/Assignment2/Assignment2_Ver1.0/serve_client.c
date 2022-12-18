#include<pthread.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include <unistd.h>

#define __BST_H_

extern Node* root;
int count = 1;

void DealWithCommand(char line[]) {
	char command[20];
	char parameter[20];
	int parameters;
	char* commandP = command;
	char* parameterP = parameter;

	commandP = strtok(line, " ");
	parameterP = strtok(NULL, " ");
	
	printf("command is %s\n", commandP);
	
	if (strcmp(commandP,"addNode") == 0) {
		addNode(root, atoi(parameterP));
		printf("Parameter is %d\n",atoi(parameterP));
	}
	else if (strcmp(commandP,"removeNode") == 0) {
		removeNode(root, atoi(parameterP));
		printf("Parameter is %d\n",atoi(parameterP));
	}
	else if (strcmp(commandP,"countNodes") == 0) {
		countNodes(root);
	}
	else if (strcmp(commandP,"avgSubtree") == 0) {
		avgSubtree(root);
	}	
}

void *downtime() {
	//balanceTree();
}

void *ServeClient(char *clientCommands) {
	char line[100];

	FILE* fp = NULL;

	if ((fp = fopen(clientCommands, "r")) == NULL) {
		printf("Error!!!\n");
		exit(1);
	}
	
	while (!feof(fp)) {
		fscanf(fp, "%[^\n]", line);
		if(!feof(fp)){
		printf("!This is %d and line is %s\n",count++, line);
		DealWithCommand(line);
		fgetc(fp);
		}
	}
	
	fclose(fp);
}

