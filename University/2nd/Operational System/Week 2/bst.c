#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "bst.h"

void freeSubtree(Node* N);

typedef struct  _Node
{
    int             value;      //Value of current node
    struct _Node*   left;       //Address of left node
    struct _Node*   right;      //Address of right node
}Node;

Node* addNode(Node* root, int value)
{
    //Create a node and allocate memory
    Node*   NodeInsert  = (Node*)malloc(sizeof(Node));
    NodeInsert->value   = value;
    NodeInsert->left    = NULL;
    NodeInsert->right   = NULL;

    //When its argument root is NULL, create a root node
    if (root == NULL)
        return NodeInsert;

    Node*   NextNode;   // NextNode     : to find the target
    Node*   ParentNode; // ParentNode   : parent node of target

    // Initialization
    // Null for ParentNode(a flag of single root)
    NextNode    = root;
    ParentNode  = NULL;

    while (NextNode != NULL)    //Flag of reaching the leaf
    {
        if (value < NextNode->value)        //Smaller means the target is on the right side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->right;
        }
        else if (value > NextNode->value)   //Larger means the target is on the left side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->left;
        }
        else if (value == NextNode->value)  //In the case of duplicate, return NULL
        {
            return NULL;
        }
    }

    //Do not know the target is left or right leaf of parentnode
    if (value > ParentNode->value)
        ParentNode->left    = NodeInsert;
    else if (value < ParentNode->value)
        ParentNode->right   = NodeInsert;

    return NodeInsert;
}

Node* removeNode(Node* root, int value)
{
    if (root == NULL)  //Nothing happends if the root is Null
        return root;

    Node* NextNode;     // NextNode: to find the target
    Node* ParentNode;   // ParentNode: parent node of target
    Node* TempNode;     // TempNode: to find the leave of target node

    NextNode    = root;    
    ParentNode  = NULL;  // Null for ParentNode(a flag of target is the root)
    TempNode    = root;

    //Get the ParentNode for target
    while (value != NextNode->value)
    {
        if (NextNode->right == NULL && NextNode->left == NULL)  //Nothing happends if the value is not present
            return root;

        //Store the parentnode and move to nextnode
        if (value < NextNode->value)        //Smaller means the target is on the right side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->right;
        }
        else if (value > NextNode->value)   //Larger means the target is on the left side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->left;
        }
    }

    //4 situations(LR,LN,NR,NN-N for NULL)
    if (NextNode->right != NULL && NextNode->left != NULL)      //Left-Right
    {
        //Get the largest node in right nodes
        TempNode = NextNode->right;
        while (TempNode->left != NULL)
            TempNode = TempNode->left;
        TempNode->left = NextNode->left;

        if (ParentNode == NULL) //When the target is the root
        {
            root = root->right;
            free(NextNode);
            return root;
        }

        //No information about the left or right node of parentnode is the target
        if (ParentNode->left == NextNode)
            ParentNode->left = NextNode->right;
        if (ParentNode->right == NextNode)
            ParentNode->right = NextNode->right;

        free(NextNode);
    }
    else if (NextNode->right == NULL && NextNode->left != NULL) //Left-Null
    {
        if (ParentNode == NULL) //When the target is the root
        {
            root = root->left;
            free(NextNode);
            return root;
        }

        //No information about the left or right node of parentnode is the target
        if (ParentNode->left == NextNode)
            ParentNode->left = NextNode->left;
        if (ParentNode->right == NextNode)
            ParentNode->right = NextNode->left;

        free(NextNode);
    }
    else if (NextNode->right != NULL && NextNode->left == NULL) //Null-Right
    {
        if (ParentNode == NULL) //When the target is the root
        {
            root = root->right;
            free(NextNode);
            return root;
        }

        //No information about the left or right node of parentnode is the target
        if (ParentNode->left == NextNode)
            ParentNode->left = NextNode->right;
        if (ParentNode->right == NextNode)
            ParentNode->right = NextNode->right;

        free(NextNode);
    }
    else if (NextNode->right == NULL && NextNode->left == NULL) //Null-Null
    {
        if (ParentNode == NULL) //When the target is the root
        {
            free(root);
            return NULL;
        }

        //No information about the left or right node of parentnode is the target
        if (ParentNode->left == NextNode)
            ParentNode->left = NULL;
        if (ParentNode->right == NextNode)
            ParentNode->right = NULL;

        free(NextNode);
    }

    return root;
}

void displaySubtree(Node* N)
{
    if (N != NULL)
    {
        //As the larger value is on the left leave so sequence is right->parent->left
        displaySubtree(N->right);
        printf("%d\n", N->value);
        displaySubtree(N->left);
    }
    else
        return;
}

int numberLeaves(Node* N)
{
    if (N != NULL)
    {
        if (N->left == NULL && N->right == NULL)
            return 1;
        else
            return numberLeaves(N->left) + numberLeaves(N->right);
    }
    else
        return 0;
}

Node* removeSubtree(Node* root, int value)
{
    if (root == NULL)   //Nothing happen if root is null
        return NULL;

    Node* NextNode;     // NextNode: to find the target
    Node* ParentNode;   // ParentNode: parent node of target

    // Initialization
    NextNode    = root;
    ParentNode  = NULL; // Null for ParentNode(a flag of target is root)

    //Get the ParentNode for target
    while (value != NextNode->value)
    {
        if (NextNode->right == NULL && NextNode->left == NULL)  //Nothing happends if the value is not present
            return root;

        if (value < NextNode->value)        //Smaller means the target is on the right side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->right;
        }
        else if (value > NextNode->value)   //Larger means the target is on the left side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->left;
        }
    }

    if (ParentNode == NULL) //When the target is the root
    {
        freeSubtree(NextNode);
        return NULL;
    }

    //No information about the left or right leaf of parentnode is the target
    if (ParentNode->left == NextNode)
        ParentNode->left = NULL;
    if (ParentNode->right == NextNode)
        ParentNode->right = NULL;

    freeSubtree(NextNode);

    return root;
}

void freeSubtree(Node* N)
{
    if (N != NULL)
    {
        //As the larger value is on the left leave so sequence is right->parent->left
        Node* LeftNode = N->left;    
        Node* RightNode = N->right;

        freeSubtree(LeftNode);
        free(N);
        freeSubtree(RightNode);
    }
}

int nodeDepth(Node* root, Node* N)
{
    if (N == NULL || root == NULL)
        return -1;

    Node*   NextNode;     // NextNode: to find the target
    Node*   ParentNode;   // ParentNode: parent node of target
    int     count;

    //Initialization
    NextNode    = root;
    count       = 0;
    ParentNode  = NULL;

    while (N->value != NextNode->value)
    {
        if (NextNode->right == NULL && NextNode->left == NULL)  //return -1 if the value is not present
            return -1;

        if (N->value < NextNode->value)         //Smaller means the target is on the right side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->right;
        }
        else if (N->value > NextNode->value)    //Larger means the target is on the left side
        {
            ParentNode  = NextNode;
            NextNode    = NextNode->left;
        }
        ++count;
    }

    if (ParentNode == NULL) //if the target is the root
        return count;

    //Ensure the N belong to the root
    if (ParentNode->left == N || ParentNode->right == N)
        return count;
    else
        return -1;
}
