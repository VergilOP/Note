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
 */
static int Index = 0;
static float Sum = 0;

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
    if (root == NULL) return;

    //traverse the tree using inorder
    StoreToList(root->right, List);
    List[Index++] = root->data;
    StoreToList(root->left , List);
}

Node* AddToNew(int* List, int length,Node* N) {
    //Return condition
    if (length == 0) {
        return NULL;
    }
    if (length == 1) {
        addNode(N, List[0]);
        return NULL;
    }
    
    //Return pointer
    Node* Temp = addNode(N, List[length/2]);

    //Deal with left tree
    AddToNew(List, length/2,Temp);

    //Deal with right tree
    if (length % 2 == 1) {
        //Use temp can decrease the comparation in addNode();
        AddToNew(List + length / 2 + 1, length / 2      ,Temp);
    }
    else if (length % 2 == 0) {
        //Use temp can decrease the comparation in addNode();
        AddToNew(List + length / 2 + 1, (length / 2) - 1,Temp);
    }

    return Temp;
}

float AccBST(Node* root) {
    if (root == NULL) return 0;

    AccBST(root->right);
    Sum += root->data;
    AccBST(root->left);

    return Sum;
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
    //Initialize the Index and Sum
    Index = 0;
    Sum = 0;

    int length = countNodes(N);                     //The length of the list  

    //If the BST is empty
    if(length == 0){
    	return 0;
    }
    AccBST(N);

    return Sum / length;
}

// This functions converts an unbalanced BST to a balanced BST
Node* balanceTree(Node* root){
    //Initialize the Index
    Index = 0;

    int length = countNodes(root);                  //The length of the list  
    int* List = (int*)malloc(sizeof(int)*length);   //The address of the new list
    assert(List != NULL);                           //If NULL then break
    memset(List, 0, sizeof(int) * length);          //Initialize the memory

    // Use inorder to store the data to list
    StoreToList(root, List);

    Node* result = AddToNew(List, length, NULL);
    
    //free the memory
    free(List);

    return result;
}
