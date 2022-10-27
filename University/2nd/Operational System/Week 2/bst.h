#ifndef _BST_H_
#define _BST_H_

typedef struct _Node Node;

Node *addNode(Node *root, int value);

Node *removeNode(Node *root, int value);

void displaySubtree(Node *N);

int numberLeaves(Node *N);

Node *removeSubtree(Node *root,int value);

int nodeDepth (Node *R, Node *N);

#endif
