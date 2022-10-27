#include <iostream>
using namespace std;

struct node {
    int data;
    node *next;

		// constructor for node
    node(int value){
		data = value;
		next = NULL;
	}		
};

class linkedlist {
	node *head;
	
	public:
	linkedlist(){ 
		head = NULL;
	}	

	int append(int data);
	int print();
	~linkedlist();
};

int linkedlist::append(int data){

	//goto the end of list
	node* current = head;
	if(current != NULL){	// Check if list is empty
		while(current->next != NULL){
			current = current->next;
		}
	}

	//allocate memory
	//node* new_node = (node*)malloc(sizeof(struct node));
	node* new_node = new (nothrow) node(data);
	if(new_node == NULL){
		return -1;
	}
	//new_node->next = NULL;
	
	//append element
	if(current != NULL){ // Check if list is empty 
		current->next = new_node;
	}else{
		head = new_node;
	}
  
	return 0;
}

int linkedlist::print(){

	node *current = head;

	while(current != NULL){
		cout << current->data << "-->"; 
		current = current->next;
	}
	cout << "EndList\n";
	return 0;
}

linkedlist::~linkedlist(){
	node *current = head;
	node *temp;

	while(current != NULL){
		temp = current->next;
		//free(current);
		delete current;
		current = temp;
	}
}


int main() {
	int value;
	linkedlist list;

	list.append(1);
	list.append(2);
	list.append(3);
	list.append(4);

	list.print();

	return 0;	
}
