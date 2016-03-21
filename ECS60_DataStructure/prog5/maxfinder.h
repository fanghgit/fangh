// Author: Sean Davis
#ifndef maxfinderH
#define maxfinderH
#include "NetDriver.h"
#include <iostream>

using namespace std;





class Address{
public:
    char address[16] = " ";
    int num;
    long long index;
    bool info = true;
};


class pt{
public:
    int index = 0;
    int capacity = 0;
    pt() {}
    pt(int i, int c): index( i ), capacity ( c ) {}
};


class HashTable{
public:
    Address ele[10007];
    int size;
    HashTable(int s = 10007): size(s) {}
    
    void insert( Address element ){
        int pos = element.index % size;
        int i = 1;
        while(true){
            if(ele[pos].info){
                for(int j = 0; j < 16; j++){
                    ele[pos].address[j] = element.address[j];
                }
                ele[pos].index = element.index;
                ele[pos].info = false;
                ele[pos].num = element.num;
                break;
            }
            pos = pos + 2 * i - 1;
            if(pos >= size){
                pos = pos - size;
            }
            i++;
        }
        //cout << "pos: " << pos << endl;
    }
    
    long long convert(char *ip){
        long long result = 0;
        for(int i = 0; i < 16; i++){
            if(ip[i] != '\0'){
                if(ip[i] == '.'){
                    continue;
                }
                else{
                    result = (ip[i] - 48) + result*10;
                }
            }
            else{
                return result;
            }
        }
        return result;
    }
    
    bool compare(char *c1, char *c2, int size){
        for(int i = 0; i < size; i++){
            if(c1[i] == '\0' && c2[i] == '\0'){
                return true;
            }
            else if(c1[i] == '\0' || c2[i] == '\0'){
                return false;
            }
            else if(c1[i] != c2[i] ){
                return false;
            }
        }
        return true;
    }
    
    int search( char *ip ){
        long long target = convert(ip);
        //cout << "target: " << target << endl;
        int pos = target % size;
        //cout << "pos: " << pos << endl;
        //cout << "gege" << endl;
        //cout << "search pos: " << pos << endl;
        int i = 1;
        while(true){
            if(compare(ele[pos].address, ip, 16)){
                return ele[pos].num;
            }
            pos = pos + 2 * i - 1;
            if(pos >= size){
                pos = pos - size;
            }
            //cout << "i: " << i << endl;
            i++;
        }
    }
    
    
};


class ListNode{
public:
    pt point;
    ListNode* next = NULL;
    ListNode() {}
    ListNode(pt p, ListNode* n = NULL): point( p ), next( n ) {}
};

class List{
public:
    ListNode *header = new ListNode;
    void insert(pt item){
        //ListNode* tmp = header->next;
        //header->next = new ListNode(item);
        //header->next->next = tmp;
        
        /*
        ListNode* tmp = header;
        while(tmp->next && tmp->next->point.capacity >= item.capacity){
            tmp = tmp->next;
        }
        if(!tmp->next){
            tmp->next = new ListNode(item);
        }
        else{
            ListNode * tmp2 = tmp->next;
            tmp->next = new ListNode(item);
            tmp->next->next = tmp2;
        }
         */
        ListNode* tmp = header;
        while(tmp->next){
            tmp = tmp->next;
        }
        tmp->next = new ListNode(item);

    }
};


class vertex{
public:
    List neigthbours;
    int index;
    bool known = false;
    int path = -1;
    int cp = 0;
    //bool operator<(const vertex & v) const {return dist < v.dist;}
    //bool operator>(const vertex & v) const {return dist > v.dist;}
    //bool operator==(const vertex & v) const {return dist == v.dist;}
    void print(){
        ListNode *tmp = neigthbours.header->next;
        while(tmp){
            cout << "index = " << tmp->point.index << ", capacity = " << tmp->point.capacity << "; ";
            tmp = tmp->next;
        }
    }
};


class corrdinate{
public:
    int src = 0;
    int dest = 0;
};



class MaxFinder
{
public:
  const Computer *computers;
  int numComputers;
  int numTerminals;
  Address *add;
    
  MaxFinder(const Computer *computers, int numComputers, int numTerminals);
  bool Update(int start, vertex *ver, int numComputers);
  //bool compare(char *c1, const char *c2, int size);
  int calcMaxFlow(Edge *edges, int numEdges);
  //corrdinate find(Edge edge, int numComputers);

  int dfs(int start, vertex *ver, int numComputers);
  //long long convert(char *ip);
    
}; // class MaxFinder




#endif
