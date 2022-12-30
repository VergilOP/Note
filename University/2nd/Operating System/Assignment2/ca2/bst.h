struct Node{
	int data;
	struct Node *left;
	struct Node *right;
};
typedef struct Node Node;

Node* addNode(Node *root, int value);
Node* freeSubtree(Node *N);
int countNodes(Node *N);
Node* removeNode(Node* root, int value);
void displaySubtree(Node *N);
float avgSubtree(Node *N);
Node* balanceTree(Node* root);
