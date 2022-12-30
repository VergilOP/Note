#include <stdio.h>
#include <assert.h>

#include "bst.h"

int main() {
  Node *a, *b;
  a = addNode(NULL, 42);
  b = removeNode(a, 42);
  assert(b == NULL);

  a = addNode(NULL, 1);
  b = addNode(a, 2);

  displaySubtree(a);
  int c = numberLeaves(a);
  assert(c == 1);

  c = nodeDepth(a,b);
  assert(c == 1);

  a = removeSubtree(a, 2);

  return 0;
}
