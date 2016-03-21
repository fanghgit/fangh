#include <iostream>
#include <climits>
#include "LeafNode.h"
#include "InternalNode.h"
#include "QueueAr.h"

using namespace std;


LeafNode::LeafNode(int LSize, InternalNode *p,
  BTreeNode *left, BTreeNode *right) : BTreeNode(LSize, p, left, right)
{
  values = new int[LSize];
}  // LeafNode()

void LeafNode::addToLeft(int value, int last)
{
  leftSibling->insert(values[0]);

  for(int i = 0; i < count - 1; i++)
    values[i] = values[i + 1];

  values[count - 1] = last;
  if(parent)
    parent->resetMinimum(this);
} // LeafNode::ToLeft()

void LeafNode::addToRight(int value, int last)
{
  rightSibling->insert(last);

  if(value == values[0] && parent)
    parent->resetMinimum(this);
} // LeafNode::addToRight()

void LeafNode::addToThis(int value)
{
  int i;

  for(i = count - 1; i >= 0 && values[i] > value; i--)
      values[i + 1] = values[i];

  values[i + 1] = value;
  count++;

  if(value == values[0] && parent)
    parent->resetMinimum(this);
} // LeafNode::addToThis()


void LeafNode::addValue(int value, int &last)
{
  int i;

  if(value > values[count - 1])
    last = value;
  else
  {
    last = values[count - 1];

    for(i = count - 2; i >= 0 && values[i] > value; i--)
      values[i + 1] = values[i];
    // i may end up at -1
    values[i + 1] = value;
  }
} // LeafNode:: addValue()


//function1
void LeafNode::removeFromThis(int value)
{
    int i;
    for (i = 0; i < count; i++){
        if(values[i] == value){
            count--;
            break;
        }
    }
    int j;
    for (j = i; j < count; j++){
        values[j] = values[j+1];
    }
    
}





int LeafNode::getMaximum()const
{
  if(count > 0)  // should always be the case
    return values[count - 1];
  else
    return INT_MAX;
} // getMaximum()


int LeafNode::getMinimum()const
{
  if(count > 0)  // should always be the case
    return values[0];
  else
    return 0;

} // LeafNode::getMinimum()


LeafNode* LeafNode::insert(int value)
{
  int last;

  if(count < leafSize)
  {
    addToThis(value);
    return NULL;
  } // if room for value

  addValue(value, last);

  if(leftSibling && leftSibling->getCount() < leafSize)
  {
    addToLeft(value, last);
    return NULL;
  }
  else // left sibling full or non-existent
    if(rightSibling && rightSibling->getCount() < leafSize)
    {
      addToRight(value, last);
      return NULL;
    }
    else // both siblings full or non-existent
      return split(value, last);
}  // LeafNode::insert()

void LeafNode::print(Queue <BTreeNode*> &queue)
{
  cout << "Leaf: ";
  for (int i = 0; i < count; i++)
    cout << values[i] << ' ';
  cout << endl;
} // LeafNode::print()



BTreeNode* LeafNode::remove(int value)
{   // To be written by students
    removeFromThis(value);
    if(count >= (leafSize + 1)/2){
        if(parent){
            parent->resetMinimum(this);
        }
        return NULL;
    }
    
    if(leftSibling && leftSibling->getCount() > (leafSize + 1)/2){
        int tmpvalue = leftSibling->getMaximum();
        leftSibling->remove(tmpvalue);
        insert(tmpvalue);
        parent->resetMinimum(this);
        parent->resetMinimum(leftSibling);
        return NULL;
    }
    
    if(leftSibling){
        return mergeLeft();
    }
    
    if(rightSibling && rightSibling->getCount() > (leafSize + 1)/2){
        int tmpvalue = rightSibling->getMinimum();
        rightSibling->remove(tmpvalue);
        insert(tmpvalue);
        parent->resetMinimum(this);
        parent->resetMinimum(rightSibling);
        return NULL;
    }
    if(rightSibling){
        return mergeRight();
    }
    if(!parent && count == 1){
        return this;
    }
    
  return NULL;  // filler for stub
}  // LeafNode::remove()



LeafNode* LeafNode::split(int value, int last)
{
  LeafNode *ptr = new LeafNode(leafSize, parent, this, rightSibling);


  if(rightSibling)
    rightSibling->setLeftSibling(ptr);

  rightSibling = ptr;

  for(int i = (leafSize + 1) / 2; i < leafSize; i++)
    ptr->values[ptr->count++] = values[i];

  ptr->values[ptr->count++] = last;
  count = (leafSize + 1) / 2;

  if(value == values[0] && parent)
    parent->resetMinimum(this);
  return ptr;
} // LeafNode::split()


BTreeNode* LeafNode::mergeRight()
{
    BTreeNode* tmp = rightSibling;
    for(int i = count; i < (count + rightSibling->getCount()); i++)
    {
        values[i] = ((LeafNode*) rightSibling)->values[i - count];
    }
    count = count + rightSibling->getCount();
    
    if(rightSibling->getRightSibling()){
        rightSibling->getRightSibling()->setLeftSibling(this);
    }
    this->setRightSibling(rightSibling->getRightSibling());
    return tmp;
}

BTreeNode* LeafNode::mergeLeft()
{
    BTreeNode* tmp = leftSibling;
    for(int i = (count + leftSibling->getCount() - 1); i >= leftSibling->getCount(); i--)
    {
        values[i] = values[i - leftSibling->getCount()];
    }
    for(int i = leftSibling->getCount() - 1; i >= 0; i--){
        values[i] = ((LeafNode*) leftSibling)->values[i];
    }
    count = count + leftSibling->getCount();
    
    if(leftSibling->getLeftSibling()){
        leftSibling->getLeftSibling()->setRightSibling(this);
    }
    this->setRightSibling(leftSibling->getLeftSibling());
    return tmp;
}







