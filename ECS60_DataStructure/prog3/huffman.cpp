//
//  main.cpp
//  p3
//
//  Created by fangh on 1/31/16.
//  Copyright © 2016 fangh. All rights reserved.
//

#include <iostream>
#include <fstream>
#include "BinaryTree.h"

using namespace std;

class obs{
public:
    obs(int count = 1, char character = NULL): count(count), character(character) {}
    void print(){
        cout << character << "   " << count << " ";
    }
    
    int count;
    char character;
    
};


template <class Object>
class ListNode
{
public:
    ListNode( const Object & theElement = Object( ), ListNode * n = NULL, int count = 1 )
    : element( theElement ), count(count), next( n ) { }
    ListNode(int count) : count(count) { }
    void insert( const Object & x){
        if( x == element){
            count++;
        }
        else{
            if(next){
                next->insert(x);
            }
            else{
                next = new ListNode(x);
            }
        }
    }
    
    void resetBT(){
        BT = new BinaryTree<obs>(obs(count, element), NULL, NULL);
    }
    void print(){
        cout << element << endl;
        cout << count << endl;
        if(next){
            next->print();
        }
    }
    Object element;
    int count = 1;
    BinaryTree<obs>* BT;
    ListNode *next;
    
};

template <class Object>
class List
{
public:
    List( )
    {
        header = new ListNode<Object>;
    }
    List( const List<Object> & rhs )
    {
        header = new ListNode<Object>;
        *this = rhs;
    }

    bool isEmpty( ) const
    {
        return header->next == NULL;
    }

    void insert( const Object & x ){
        if(header->next){
            header->next->insert(x);
        }
        else{
            header->next = new ListNode<char>(x);
        }
    }
    void print(){
        header->next->print();
    }
    
    void resetBT(){
        ListNode<Object>* tmp = header;
        while(tmp->next){
            tmp->next->resetBT();
            tmp = tmp->next;
        }
    }
    
    const List & operator=( const List & rhs );
    ListNode<Object> *header;
};


ListNode<char>* searchmin(List<char> & List){
    ListNode<char>* min = List.header->next;
    ListNode<char>* index = List.header->next;
    while(index){
        if(min->count > index->count){
            min = index;
        }
        index = index->next;
    }
    
    index = List.header->next;
    ListNode<char>* prev = List.header;
    while(index){
        if(min == index){
            prev->next = index->next;
            break;
        }
        index = index->next;
        prev = prev->next;
    }
    return min;
}


void update(List<char> & List){
    ListNode<char>* left = searchmin(List);
    ListNode<char>* right = searchmin(List);
    ListNode<char>* result = new ListNode<char>(left->count + right->count);
    result->BT = new BinaryTree<obs>(obs(left->count + right->count), left->BT, right->BT);
    result->next = List.header->next;
    List.header->next = result;
}


int main(int argc, const char * argv[]) {
    
    //char *dir = getcwd(NULL, 0);
    //cout << dir << endl;
    ifstream inf(argv[1]);
    
    /*
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    else{
        cout << "Success!" << endl;
    }
     */
    
    List<char> myList;
    char character;
    while(inf >> noskipws >> character){
        myList.insert(character);
    }
    
    myList.resetBT();
    //myList.print();
    
    while(myList.header->next->next){
        update(myList);
        //cout << "************" << endl;
        //myList.print();
    }
    
    
    //cout << "****R*****" << endl;
    //obs tmp = myList.header->next->BT->getObject();
    //cout << "char = " << tmp.character << " count = " << tmp.count << endl;
    char s[] = {};
    myList.header->next->BT->PT(s, 0);

    return 0;
}
