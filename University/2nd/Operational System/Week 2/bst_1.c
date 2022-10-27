#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

#include "bst.h"

int main(){
  Node *a, *b, *c, *d, *e, *f;
  a = addNode(NULL, 42);
  b = removeNode(a, 42);
  c = addNode(a, 90);
  b = removeNode(a, 42);
  //d = addNode(a, 100);
  //e = addNode(a, 23);
  //f = removeNode(a,42);
  
  //a = addNode(NULL,1);
  //b = addNode(a,2);
  
  displaySubtree(b);

  return 0;
}

typedef struct  _Node
{
  int value;		//Value of current node
  struct _Node *left;	//Address of left node
  struct _Node *right; //Address of right node
}Node;

Node *addNode(Node *root, int value)
{
  if(root == NULL)
  {
    //Create a node and allocate memory
    Node *NodeInsert = (Node *)malloc(sizeof(Node));
    NodeInsert->value = value;
    NodeInsert->left  = NULL;
    NodeInsert->right = NULL;
    
    //Replace the Null
    root = NodeInsert;
    return root;
  }
  else
  {
    if(value == root->value)
      return NULL;
    
    if(value > root->value)
    {
      if(root->left == NULL)
      {
        //Create a node and allocate memory
        Node *NodeInsert = (Node *)malloc(sizeof(Node));
        NodeInsert->value = value;
        NodeInsert->left  = NULL;
        NodeInsert->right = NULL;
        
        //Replace the Null
        root->left = NodeInsert;
        return root->left;
        
        //If use addNode() here, the new value will give to NULL which is not in the BST
      }
      else
      {
        return addNode(root->left ,value);
      }
    }
    else 
    {
      if(root->right == NULL)
      {
        //Create a node and allocate memory
        Node *NodeInsert = (Node *)malloc(sizeof(Node));
        NodeInsert->value = value;
        NodeInsert->left  = NULL;
        NodeInsert->right = NULL;
        
        //replace the NULL
        root->right = NodeInsert;
        return root->right;
        
        //Same reason
      }
      else
      {
          return addNode(root->right,value);
      }
      
    }
  }
}

/*
The recursion for remove(Failed)

Node* removeNode(Node *root,int value)
{
  //Store the ParentNode and the NextNode
  Node *NextNode;
  Node *ParentNode;
  
  if(root != NULL)
  {
    //If the root is not the target
    if(value != root->value)
    {
      if(value > root->value)
      {
        //record the parentnode
        ParentNode = root;
        removeNode(root->left,value);
      }
      if(value < root->value)
      {
        //record the parentnode
        ParentNode = root;
        removeNode(root->right,value);
      }
    }
    
    if(value == root->value)
    {
      
      if(root->right != NULL)
      {
        NextNode = root->right;
        while(NextNode->left != NULL)
          NextNode = NextNode->left;
        NextNode->left = root->left;
        
        //Target is not the root with right node
        if(ParentNode != NULL)
        {
          if(ParentNode->left != NULL && ParentNode->left == root)
            ParentNode->left = root->right;
          if(ParentNode->right != NULL && ParentNode->right == root)
            ParentNode->right = root->right;
        }
        else
        {
          root = root->right;
        }
      }
      else
      {
        //target is not the root with left node but right node
        if(root->left != NULL && ParentNode != NULL)
        {
          NextNode = root->left;
          //printf("%d\n",ParentNode->value);
          
          if(ParentNode->left == root)
            ParentNode->left = NextNode;
          else
            ParentNode->right = NextNode;
        }
        else
        {
          //target is the root with left node but right node
          if(root->left != NULL && ParentNode == NULL)
            root = root->left;
          //target is the root without left node and right node
          if(root->left == NULL && ParentNode == NULL)
            root = NULL;
        }
      }
    }
  }
  
  return root;
}
*/

Node* removeNode(Node *root,int value)
{
  // NextNode: to find the target
  // ParentNode: parent node of target
  // TempNode: to find the leave of target node
  Node *NextNode;
  Node *ParentNode;
  Node *TempNode;
  
  // Initialize
  // Null for ParentNode(a flag of single leave)
  NextNode = root;
  ParentNode = NULL;
  TempNode = root;
  
  //Get the ParenNode for target
  while(value != NextNode->value)
  {
    //if the value is not existed, then break
    if(NextNode->right == NULL && NextNode->left == NULL)
      return NULL;
  
    if(value < NextNode->value)
    {
      ParentNode = NextNode;
      NextNode = NextNode->right;
    }else if(value > NextNode->value)
    {
      ParentNode = NextNode;
      NextNode = NextNode->left;
    }else
      break;
    
  }
  
  //4 situations(LR,LN,NR,NN-N for NULL)
  if(NextNode->right != NULL && NextNode->left != NULL)
  {
    TempNode = NextNode->right;
    while(TempNode->left != NULL)
      TempNode = TempNode->left;
    TempNode->left = NextNode->left;
    
    if(ParentNode == NULL)
    {
      root = NextNode->right;
      return root;
    }
    
    if(ParentNode->left == NextNode)
      ParentNode->left = NextNode->right;
    if(ParentNode->right == NextNode)
      ParentNode->right = NextNode->right;
      
  }
  
  if(NextNode->right == NULL && NextNode->left != NULL)
  {
    if(ParentNode == NULL)
    {
      root = NextNode->left;
      return root;
    }
  
    if(ParentNode->left == NextNode)
      ParentNode->left = NextNode->left;
    if(ParentNode->right == NextNode)
      ParentNode->right = NextNode->left;
  }
  
  if(NextNode->right != NULL && NextNode->left == NULL)
  {
    if(ParentNode == NULL)
    {
      root = NextNode->right;
      return root;
    }
  
    if(ParentNode->left == NextNode)
      ParentNode->left = NextNode->right;
    if(ParentNode->right == NextNode)
      ParentNode->right = NextNode->right;
  }
  
  if(NextNode->right == NULL && NextNode->left == NULL)
  {
    if(ParentNode == NULL)
    {
      root = NULL;
      return root;
    }
    
    if(ParentNode->left == NextNode)
      ParentNode->left = NULL;
    if(ParentNode->right == NextNode)
      ParentNode->right = NULL;
  }
  
  return root;
}

void displaySubtree(Node *N)
{
  if(N != NULL)
  {
    displaySubtree(N->right);
    printf("%d\n",N->value);
    displaySubtree(N->left);
  }
  else
    return;
}

Node *removeSubtree(Node *root, int value)
{
  if(root != NULL)
  {
    if(value == root->value)
      root = NULL;
    else 
    {
      if(value > root->value)
      {
        if(value == root->left->value)
          root->left = NULL;
          
          //if Use removeSubtree() here, the NULL will be removed,but this value still in BST(as NULL is not in BST)
        else
          removeSubtree(root->left ,value);
      }
      else
      {
        if(value == root->right->value)
          root->right = NULL;
          
          //Same reason
        else
          removeSubtree(root->right,value);
      }
    }
  }
  return root;
}
