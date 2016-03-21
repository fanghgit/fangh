#include <iostream>

using namespace std;

template <class T>
class BinaryTree
{
public:
  T object;
  BinaryTree<T> *l;
  BinaryTree<T> *r;

  BinaryTree() : l(NULL), r(NULL) {}
  BinaryTree(const T &ob, BinaryTree<T> *l, BinaryTree<T> *r) : object(ob), l(l), r(r) {}  bool operator< (BinaryTree <T> &r)
  {
     return object < r.object;
  }
  void PT(char c[], int index);
  T& getObject()
  {
    return object;
  }
    
};

template <class T>
void BinaryTree<T>::PT(char c[], int index)
{
  if(l)
  {
    c[index] = '0';
    l->PT(c, index + 1);
  }

  if(r)
  {
    c[index] = '1';
    r->PT(c, index + 1);
  }
  
  if(!l && !r)
  {
    object.print();
    for(int i = 0; i < index; i++)
      cout << c[i];
    cout << endl;
  }
}


