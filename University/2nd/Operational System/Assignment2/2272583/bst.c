#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>
#include <stdbool.h>
#include <unistd.h>
#include "bst.h"

/*

Place for the BST functions from Exercise 1.

*/

/*
   Helper functions
   {
    Node* find_parent(Node* root, int value);       //Used to find the parent Node of the target
    Node* mk_node(int value);                       //Used to create a new node
    bool is_ordered(Node* root);                    //Used to check whether in order or not
    void StoreToList(Node* root, int* List);        //Used to store the data from BST to List
    Node* AddToNew(int* List,int left, int right);  //Used to create a balance tree based on list
    float AccBST(Node* root);                       //Used to get the sum of the BST
   }
 */
static int Index = 0;

Node* find_parent(Node* root, int value) {
    assert(root != NULL);
    assert(value != root->data);

    Node* next = value > root->data ? root->left : root->right;

    if (next == NULL || next->data == value)
        return root;
    else
        return find_parent(next, value);
}

Node* mk_node(int value) {
    Node* node = (Node*)malloc(sizeof(Node));
    assert(node != NULL);
    node->data = value;
    node->left = node->right = NULL;
    return node;
}

bool is_ordered(Node* root) {
    if (root == NULL)
        return true;
    if (root->left && root->left->data < root->data)
        return false;
    if (root->right && root->right->data > root->data)
        return false;
    return true;
}

void StoreToList(Node* root, int* List) {
	if (!root) return;

	//traverse the tree using inorder
	StoreToList(root->right, List);
	List[Index++] = root->data;
	StoreToList(root->left , List);
}

Node* AddToNew(int* List,int left, int right) {
	//Arrive to the bottom
	if (left > right) {
		return NULL;
	}

	int mid = (left + right) / 2;

	Node* NewRoot = (Node*)malloc(sizeof(Node));

	NewRoot->data = List[mid];
	NewRoot->left = AddToNew(List, mid+1, right);
	NewRoot->right = AddToNew(List, left, mid - 1);

	return NewRoot;
}

float AccBST(Node* root) {
    if (root == NULL) return 0;

    return root->data + AccBST(root->right) + AccBST(root->left);
}

/*
    The exercise 2 contents
*/
Node* addNode(Node* root, int value) {

    if (root == NULL)
        return mk_node(value);

    if (value == root->data)
        return root;

    Node* parent = find_parent(root, value);
    Node* child = value > parent->data ? parent->left : parent->right;
    assert(child == NULL || child->data == value);

    if (child == NULL) {
        // value not found, then insert and return node
        child = mk_node(value);
        if (value > parent->data)
            parent->left = child;
        else
            parent->right = child;

        return root;
    }
    else {
        // value found, then return null
        return root;
    }
}

Node* freeSubtree(Node* root) {
    if (root == NULL)
        return root;

    freeSubtree(root->left);
    freeSubtree(root->right);
    free(root);
    
    return root;
}

int countNodes(Node* N)
{
    int Num = 0;
    if (N != NULL) {
        Num = countNodes(N->left) + countNodes(N->right) + 1;
    }
    return Num;
}

Node* removeNode(Node* root, int value) {
    assert(is_ordered(root));
	
    // empty tree
    if (root == NULL)
        return NULL;

    // find node with value 'value' and its parent node
    Node* parent, * node;
    if (root->data == value) {
        parent = NULL;
        node = root;
    }
    else {
        parent = find_parent(root, value);
        node = value > parent->data ? parent->left : parent->right;
    }
    assert(node == NULL || node->data == value);

    // value not found
    if (node == NULL)
        return root;

    // re-establish consistency
    Node* new_node;
    if (node->left == NULL) {
        // node has only right child, then make right child the new node
        new_node = node->right;
    }
    else {
        // otherwise make right child the rightmost leaf of the subtree rooted in the left child
        // and make the left child the new node
        Node* rightmost = new_node = node->left;
        while (rightmost->right != NULL)
            rightmost = rightmost->right;
        rightmost->right = node->right;
    }

    free(node);

    Node* new_root;
    if (parent == NULL) {
        // if deleted node was root, then return new node as root
        new_root = new_node;
    }
    else {
        // otherwise glue new node with parent and return old root
        new_root = root;
        if (value > parent->data)
            parent->left = new_node;
        else
            parent->right = new_node;
    }

    assert(is_ordered(new_root));

    return new_root;
}

void displaySubtree(Node* N) {
    if (N == NULL) return;

    displaySubtree(N->right);
    printf("%d \n", N->data);
    displaySubtree(N->left);
}

float avgSubtree(Node* N)
{
    //Initialize  Sum
    float Sum = 0;

    int length = countNodes(N);                     //The length of the list  

    //If the BST is empty
    if(length == 0){
    	return 0;
    }
    Sum = AccBST(N);

    return Sum / length;
}

// This functions converts an unbalanced BST to a balanced BST
Node* balanceTree(Node* root) {
    //If the root is empty, do not need to balance
	if (root == NULL) {
		return NULL;
	}

    //Get the length and create a new list
	int length = countNodes(root);
	int* List = (int*)malloc(sizeof(int) * length);

	StoreToList(root,List);

	Node* Result = AddToNew(List, 0, length - 1);

	free(List);

	Index = 0;

	return Result;
}
