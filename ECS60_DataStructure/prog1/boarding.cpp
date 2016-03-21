#include <fstream>
#include <iostream>
#include "QueueAr.h"
#include "StackAr.h"
using namespace std;

enum status {readyToMove, storage1, storage2, moveout, movein};

class passenger
{
public:
    passenger( int i = 0  ,char c = '\0'): rowNumber(i), seat(c) {}
    int rowNumber;
    char seat;
    bool isEmpty() {return rowNumber == 0;}
    void setrowNumber(int numberInput) {rowNumber = numberInput;}
    void setseat(char seatInput) {seat = seatInput;}
    void remove() {rowNumber = 0; seat = '\0';}
};

class position
{
public:
    passenger A;
    passenger B;
    passenger C;
    passenger D;
    passenger E;
    passenger F;
    bool isFull()
    {
        return !A.isEmpty() && !B.isEmpty() && !C.isEmpty() && !D.isEmpty() && !E.isEmpty() && !F.isEmpty();
    }
    void setPassenger(char ch, passenger value)
    {
        switch(ch)
        {
            case 'A': {A = value; break;}
            case 'B': {B = value; break;}
            case 'C': {C = value; break;}
            case 'D': {D = value; break;}
            case 'E': {E = value; break;}
            case 'F': {F = value; break;}
        }
    }
};


class row
{
public:
    row(int i = 1, status status = readyToMove, Queue<passenger> p = Queue<passenger>(3)) : rowNumber( i ), rowStatus(status), aislePassengers(p) {}
    int rowNumber;
    status rowStatus;
    Queue<passenger> aislePassengers;
    position allpositions;
};


void OutAndIn(row & row){
    switch (row.aislePassengers.getFront().seat)
    {
        case 'A':
            if(!row.allpositions.B.isEmpty())
            {
                row.aislePassengers.enqueue(row.allpositions.B);
                row.allpositions.B.remove();
                row.rowStatus = moveout;
                break;
            }
            else if(!row.allpositions.C.isEmpty())
            {
                row.aislePassengers.enqueue(row.allpositions.C);
                row.allpositions.C.remove();
                row.rowStatus = moveout;
                break;
            }
            else{
                row.allpositions.A = row.aislePassengers.dequeue();
                if(row.aislePassengers.isEmpty()){
                    row.rowStatus = readyToMove;
                }
                else{
                    row.rowStatus = movein;
                }
                break;
            }
        case 'F':
            if(!row.allpositions.E.isEmpty())
            {
                row.aislePassengers.enqueue(row.allpositions.E);
                row.allpositions.E.remove();
                row.rowStatus = moveout;
                break;
            }
            else if(!row.allpositions.D.isEmpty())
            {
                row.aislePassengers.enqueue(row.allpositions.D);
                row.allpositions.D.remove();
                row.rowStatus = moveout;
                break;
            }
            else{
                row.allpositions.F = row.aislePassengers.dequeue();
                if(row.aislePassengers.isEmpty()){
                    row.rowStatus = readyToMove;
                }
                else{
                    row.rowStatus = movein;
                }
                break;
            }
        case 'B':
            if(!row.allpositions.C.isEmpty())
            {
                row.aislePassengers.enqueue(row.allpositions.C);
                row.allpositions.C.remove();
                row.rowStatus = moveout;
                break;
            }
            else{
                row.allpositions.B = row.aislePassengers.dequeue();
                if(row.aislePassengers.isEmpty()){
                    row.rowStatus = readyToMove;
                }
                else{
                    row.rowStatus = movein;
                }
                break;
            }
        case 'E':
            if(!row.allpositions.D.isEmpty())
            {
                row.aislePassengers.enqueue(row.allpositions.D);
                row.allpositions.D.remove();
                row.rowStatus = moveout;
                break;
            }
            else{
                row.allpositions.E = row.aislePassengers.dequeue();
                if(row.aislePassengers.isEmpty()){
                    row.rowStatus = readyToMove;
                }
                else{
                    row.rowStatus = movein;
                }
                break;
            }
        case 'C': {
            row.allpositions.C = row.aislePassengers.dequeue();
            if(row.aislePassengers.isEmpty()){
                row.rowStatus = readyToMove;
            }
            else{
                row.rowStatus = movein;
            }
            break;
        }
        case 'D': {
            row.allpositions.D = row.aislePassengers.dequeue();
            if(row.aislePassengers.isEmpty()){
                row.rowStatus = readyToMove;
            }
            else{
                row.rowStatus = movein;
            }
            break;
        }
    }
}



void move(row & row, class row & nextRow){
    switch (row.rowStatus){
        case readyToMove:{
            if(!row.aislePassengers.isEmpty() && row.aislePassengers.getFront().rowNumber == row.rowNumber){
                row.rowStatus = storage1;
                break;
            }
            else{
                if(!row.aislePassengers.isEmpty() && nextRow.aislePassengers.isEmpty()){
                    nextRow.aislePassengers.enqueue(row.aislePassengers.dequeue());
                    break;
                }
                else{
                    break;
                }
            }
        }
        case storage1:{
            row.rowStatus = storage2;
            break;
        }
        case moveout: case movein: case storage2:{
            OutAndIn(row);
            break;
        }
    }
}


int moveToNext( Queue<row> & rows, Queue<passenger> & passengers){
    int NumberFull = 0;
    row fakerow;
    row tmp;
    tmp = rows.dequeue();
    if(!tmp.aislePassengers.isEmpty())
    {
        if(!tmp.aislePassengers.isEmpty() && tmp.aislePassengers.getFront().rowNumber == tmp.rowNumber){
            move(tmp, fakerow);
        }
    }
    if(tmp.allpositions.isFull()){
        NumberFull++;
    }
    row tmp2;
    
    for(int i = 0; i < 47; i++){
        tmp2 = rows.dequeue();
        move(tmp2, tmp);
        if(tmp2.allpositions.isFull()){
            NumberFull++;
        }
        rows.enqueue(tmp);
        tmp = tmp2;
    }
    if(tmp.aislePassengers.isEmpty()){
        if(!passengers.isEmpty()){
            tmp.aislePassengers.enqueue(passengers.dequeue());
        }
    }
    rows.enqueue(tmp);
    return NumberFull;
}

int totaltime(Queue<passenger> & passengers){
    Queue<row> rows = Queue<row>(48);
    
    for(int i = 48; i > 0; i--)
    {
        rows.enqueue(row(i));
    }
    int time = 5;
    
    int numberOfFull = 0;
    while(numberOfFull < 48){
        time += 5;
        numberOfFull = moveToNext(rows, passengers);
    }
    time -= 5;
    return time;
}



int main(int argc, const char ** argv) {
    Queue<passenger> passengers_backtofront = Queue<passenger>(288);
    Queue<passenger> passengers_random = Queue<passenger>(288);
    Queue<passenger> passengers_outsidein = Queue<passenger>(288);
    ifstream inf(argv[1]);   
    /*
    if(!inf)
    {
        cerr << "File could not be opened" << endl;
        exit( EXIT_FAILURE);
    }
     */
    int rowNumber;
    char seat;
    int index = 0;
    while(index < 288 && inf >> rowNumber >> seat)
    {
        passenger passenger(rowNumber, seat);
        passengers_backtofront.enqueue(passenger);
        index++;
    }
    
    while(index < 576 && inf >> rowNumber >> seat)
    {
        passenger passenger(rowNumber, seat);
        passengers_random.enqueue(passenger);
        index++;
    }
    
    while(inf >> rowNumber >> seat)
    {
        passenger passenger(rowNumber, seat);
        passengers_outsidein.enqueue(passenger);
        index++;
    }

    int time;
    time = totaltime(passengers_backtofront);
    cout << "Back to front: " << time << endl;
    time = totaltime(passengers_random);
    cout << "Random: " << time << endl;
    time = totaltime(passengers_outsidein);
    cout << "Outside in: " <<  time << endl;
    
    
    return 0;
}
