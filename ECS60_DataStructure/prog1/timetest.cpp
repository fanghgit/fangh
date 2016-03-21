#include <iostream>
#include <string>
#include "CPUTimer.h"
#include "LinkedList.h"
#include "CursorList.h"
#include "StackAr.h"
#include "StackLi.h"
#include "QueueAr.h"
#include "SkipList.h"
#include <fstream>

using namespace std;

vector<CursorNode <int> > cursorSpace(500000);

void RunList(char *fileName)
{
    List <int> list;
    ListItr<int> listItr = list.zeroth();
    ifstream inf(fileName);
    
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    
    char comm, s[512];
    int value;
    inf.getline(s, 512);
    
    while(inf >> comm >> value)
    {
        if(comm == 'i')
            list.insert(value, listItr);
        else
            list.remove(value);
    }
}


void RunCursorList(char *filename)
{
    CursorList<int> list(cursorSpace);
    CursorListItr<int> listItr = list.zeroth();
    ifstream inf(filename);
    
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    
    char comm, s[512];
    int value;
    
    inf.getline(s, 512);
    
    while(inf >> comm >> value)
    {
        if(comm == 'i')
            list.insert(value, listItr);
        else
            list.remove(value);
    }
}

void RunStackAr(char *filename)
{
    StackAr<int> stack(500000);
    ifstream inf(filename);
    
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    
    char comm, s[512];
    int value;
    
    inf.getline(s, 512);
    
    while(inf >> comm >> value)
    {
        if(comm == 'i')
            stack.push(value);
        else
            stack.pop();
    }
    
}

void RunStackLi(char *filename)
{
    StackLi<int> stack;
    ifstream inf(filename);
    
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    
    char comm, s[512];
    int value;
    
    inf.getline(s, 512);
    
    while(inf >> comm >> value)
    {
        if(comm == 'i')
            stack.push(value);
        else
            stack.pop();
    }
    
}

void RunQueueAr(char *filename)
{
    Queue<int> queue(500000);
    ifstream inf(filename);
    
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    
    char comm, s[512];
    int value;
    
    inf.getline(s, 512);
    
    while(inf >> comm >> value)
    {
        if(comm == 'i')
            queue.enqueue(value);
        else
            queue.dequeue();
    }
    
}


void RunSkipList(char *filename)
{
    SkipList<int> list(0, 500000);
    ifstream inf(filename);
    
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit(EXIT_FAILURE);
    }
    
    char comm, s[512];
    int value;
    
    inf.getline(s, 512);
    
    while(inf >> comm >> value)
    {
        if(comm == 'i')
            list.insert(value);
        else
            list.deleteNode(value);
    }
}


int getChoice()
{
    int choice;
    do
    {
        cout << "      ADT Menu\n";
        cout << "0. Quit\n";
        cout << "1. LinkedList\n";
        cout << "2. CursorList\n";
        cout << "3. StackAr\n";
        cout << "4. StackLi\n";
        cout << "5. QueueAr\n";
        cout << "6. SkipList\n";
        cout << "Your choice >> ";
        cin >> choice;
        
        if (choice < 0 || choice > 6)
        {
            cout << "Your choice is not between 0 and 6.\n";
            cout << "Please try again.\n\n";
        }
        
    } while (choice < 0 || choice > 6);
    
    return choice;
}

int main()
{
    char filename[40];
    int choice;
    
    CPUTimer ct;
    
    cout << "Filename >> ";
    cin >> filename;
    
    do
    {
        choice = getChoice();
        ct.reset();
        
        switch (choice)
        {
            case 1: RunList(filename); break;
            case 2: RunCursorList(filename); break;
            case 3: RunStackAr(filename); break;
            case 4: RunStackLi(filename); break;
            case 5: RunQueueAr(filename); break;
            case 6: RunSkipList(filename);break;
        }
        
        cout << "CPU time: " << ct.cur_CPUTime() << endl;
    } while (choice > 0);
    
}
