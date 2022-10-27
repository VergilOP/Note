/*
-- Concept only code --
This program introduces the basic concepts of implementing linkedlist in C.
An actual implementation will require taking care of different test cases.
*/
 
#include<stdio.h>
#include<stdlib.h>

typedef struct node{
	int data;
	struct node* next;
} node;

typedef struct list{
  node * head;
} list;

int append(list *l, int data){


	//goto the end of list
	node* current = l->head;
	if(current != NULL){	// Check if list is empty
		while(current->next != NULL){
			current = current->next;
		}
	}


	//allocate memory
	node* new = (node*)malloc(sizeof(struct node));
	if(new == NULL){
		return -1;
	}
	new->data = data;
	new->next = NULL;
	
	//append element
	if(current != NULL){ // Check if list is empty 
		current->next = new;
	}else{
		l->head = new;
	}
  
	return 0;
}

int print(list *l){

	node *current = l->head;

	while(current != NULL){
		printf("%d--> ", current->data);
		current = current->next;
	}
	printf("EndList\n");
	return 0;
}

int destroy(list *l){
	node *current = l->head;
	node *temp;

	while(current != NULL){
		temp = current->next;
		free(current);
		current = temp;
	}
	
	return 0;
}		

int main(){

	list *l = malloc(sizeof(list));
	l->head = NULL;

	append(l, 1);
	append(l, 2);
	append(l, 3);
	append(l, 4);

	print(l);

	destroy(l);
	free(l);

	return 0;
}

