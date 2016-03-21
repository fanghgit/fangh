// Author Sean Davis
#ifndef defragmenterH
  #define defragmenterH

#include "DefragRunner.h"
#include "mynew.h"

//int extern currentRAM;
int ArrSize = 14500;
class ArrNode{
public:
    int index = -1;
    DiskBlock *obj;
    ArrNode(){};
    ArrNode(int i, DiskBlock* object): index( i ), obj( new DiskBlock(*object) ) {}
    //ArrNode(int i, DiskBlock* obj){
     //   index = i;
     //   obj = new DiskBlock(*obj);
        //obj = tmp;
        //delete tmp;
    //}
};


void ArrInsert(ArrNode * arrDisk, DiskBlock * obj, int index, int * trans_array, int &occupied){
    for(int i = 0; i < ArrSize; i++){
        if(arrDisk[i].index == -1){
            //cout << "being inserted = " << obj->getNext() << endl;
            //cout << "before " << arrDisk[i].obj->getFileBlockNum() << endl;
            arrDisk[i] = ArrNode(index, obj);
            //cout << "i = " << i << " after " << arrDisk[i].obj->getFileBlockNum() << endl;
            //cout << "i = " << i << endl;
            //cout << arrDisk[i].obj->getFileBlockNum() << endl;
            trans_array[index] = -(i + 1);
            occupied++;
            break;
        }
    }
}



//class Node{
//public:
//    Node(int i, DiskBlock* obj): index( i ), obj ( obj ) {}
//    Node( ){ };
//    int index = -1;
//    DiskBlock* obj;
//    int operator%(const int &i) const {return (index % i);}
//    bool operator==(const Node & node) const {return index == node.index;}
//    bool operator!=(const Node & node) const {return index != node.index;}
//};


class Defragmenter
{
public:
//structure 1

  Defragmenter(DiskDrive *diskDrive){
      //initializeNew();
      //currentRAM = 0;

      int occupied = 0;
      int empty = 0;
      int trans_array[diskDrive->getCapacity()];
      //trans_array = {0};
      bool bool_array[diskDrive->getCapacity()];
      //bool bool_array2[diskDrive->getCapacity()];
      ArrNode arrDisk[14500];
      
      for(int i = 0; i < diskDrive->getCapacity(); i++){
          trans_array[i] = 0;
          bool_array[i] = false;
      }
      
      
      int numberOfFiles = 0;
      for( int i = 0; i < diskDrive->getNumFiles(); i++){
          numberOfFiles += diskDrive->directory[i].getSize();
      }
      
      for(int i = numberOfFiles + 2; i < diskDrive->getCapacity(); i++){
          if(!diskDrive->FAT[i]){
              empty++;
          }
      }
      cout << "number of Files" << numberOfFiles << endl;
      cout << "empty " << empty << endl;
      
      
      int count = 2;
      for( int i = 0; i < diskDrive->getNumFiles(); i++){
          //int index = count;
          int index = diskDrive->directory[i].getFirstBlockID();
          
          while(trans_array[index] > 0){
              index = trans_array[index];
          }
          
          diskDrive->directory[i].setFirstBlockID(count);
          
          if(trans_array[index] < 0){
              DiskBlock * tmp = arrDisk[ -trans_array[index] - 1].obj;
              arrDisk[-trans_array[index] - 1].index = -1;
              occupied--;
              
              //cout << arrDisk[2].obj->getFileBlockNum() << endl;
              //for(int j = 0; j < 10; j++){
              //    cout << "j = " << j << "test " << arrDisk[j].obj->getFileBlockNum() << endl;
              //}
              if(diskDrive->FAT[count] && !bool_array[count]){
                  //bool_array[index] = true;
                  DiskBlock * tmp2 = diskDrive->readDiskBlock(count);
                  ArrInsert(arrDisk, tmp2, count, trans_array, occupied);
                  delete tmp2;
              }
              
              diskDrive->writeDiskBlock(tmp, count);
              
              index = tmp->getNext();
              delete tmp;
              
          }
          else{
              if(index != count){
                  if(occupied < ArrSize){
                      DiskBlock * tmp2 = diskDrive->readDiskBlock(index);
                      bool_array[index] = true;
                      if(index >= numberOfFiles + 2){
                          empty++;
                      }
                      
                      if(diskDrive->FAT[count] && !bool_array[count]){
                          DiskBlock * tmp = diskDrive->readDiskBlock(count);
                          ArrInsert(arrDisk, tmp, count, trans_array, occupied);
                          delete tmp;
                      }
                      
                      diskDrive->writeDiskBlock(tmp2, count);
                      index = tmp2->getNext();
                      
                      delete tmp2;
                  }
                  else{
                      swap(diskDrive, trans_array, bool_array, count, index, numberOfFiles, empty);
                  }
              }
              else{
                  DiskBlock* tmp = diskDrive->readDiskBlock(index);
                  index = tmp->getNext();
                  delete tmp;
                  //bool_array[count] = true;
              }
          }
          //bool_array[count] = true;
          //trans_array[count] = index;
          
          count++;
          
          while( index != 1){
              //cout << "index = " << index << endl;
              //cout << "*********************" << endl;
              //for(int k = 0; k < diskDrive->getCapacity(); k++){
              //    cout << k << ": " << trans_array[k] << " ";
              //}
              //cout << "\n\n";
              //cout << "****full****" << endl;
              //for(int k = 0; k < 110; k++){
              //    cout << k << ": " << arrDisk[k].index << " ";
              //}
              //cout << "\n";
              
              
              while(trans_array[index] > 0){
                  index = trans_array[index];
              }
              //cout << "block = " << block << "index = " << index << endl;
              if(trans_array[index] < 0){
                  //cout << "test" << endl;
                  //cout << "test" << -trans_array[index] - 1 << endl;
                  DiskBlock * tmp = arrDisk[ -trans_array[index] - 1].obj;
                  arrDisk[-trans_array[index] - 1].index = -1;
                  occupied--;
                  if(diskDrive->FAT[count] && !bool_array[count]){
                      //bool_array[index] = true;
                      DiskBlock * tmp2 = diskDrive->readDiskBlock(count);
                      ArrInsert(arrDisk, tmp2, count, trans_array, occupied);
                      delete tmp2;
                  }
                  diskDrive->writeDiskBlock(tmp, count);
                  index = tmp->getNext();
                  delete tmp;
              }
              else{
                  if(index != count){
                      if(occupied < ArrSize){
                          DiskBlock * tmp2 = diskDrive->readDiskBlock(index);
                          bool_array[index] = true;
                          if(index >= numberOfFiles + 2){
                              empty++;
                          }
                          
                          if(diskDrive->FAT[count] && !bool_array[count]){
                              //bool_array[index] = true;
                              DiskBlock * tmp = diskDrive->readDiskBlock(count);
                              ArrInsert(arrDisk, tmp, count, trans_array, occupied);
                              delete tmp;
                          }
                          
                          diskDrive->writeDiskBlock(tmp2, count);
                          index = tmp2->getNext();
                          delete tmp2;
                      }
                      else{
                          swap(diskDrive, trans_array, bool_array, count, index, numberOfFiles, empty);
                      }
                  }
                  else{
                      DiskBlock* tmp = diskDrive->readDiskBlock(index);
                      index = tmp->getNext();
                      delete tmp;
                      //bool_array[count] = true;
                  }
              }
              count++;
              //cout << "count " << count << endl;
              //for(int i = 0; i < diskDrive->getCapacity(); i++){
              //    cout << i << ": " << trans_array[i] << "  ";
                  
              //}
              //cout << "\n";
              
       
              //diskDrive->print();
          }
      }
      diskDrive->print();

      //cout << "test result " << test << endl;
      
    
  }
  
    void swap(DiskDrive *diskDrive, int *trans_array, bool *bool_array, int &i, int &j, int numberOfFiles, int&empty){
        if(i < (numberOfFiles+2) && empty > 0){
            if(diskDrive->FAT[i] && !bool_array[i]){
                swap1(diskDrive, trans_array, bool_array, i, j, numberOfFiles, empty);
            }
            else{
                diskDrive->FAT[i] = true;
                bool_array[j] = true;
            }
        }
        else{
            DiskBlock* tmp2 = diskDrive->readDiskBlock(j);
            //cout << "FAT " << diskDrive->FAT[i] << endl;
            //cout << "bool_array " << !bool_array[i] << endl;
            if(diskDrive->FAT[i] && !bool_array[i]){
                DiskBlock* tmp = diskDrive->readDiskBlock(i);
                diskDrive->writeDiskBlock(tmp, j);
                delete tmp;
            }
            else{
                diskDrive->FAT[i] = true;
                bool_array[j] = true;
                if(i >= numberOfFiles + 2){
                    empty++;
                }
            }
            trans_array[i] = j;
            j = tmp2->getNext();
            //if(j != 1 ){
            //    tmp2->setNext(i + 1);
            //}
            diskDrive->writeDiskBlock(tmp2, i);
            delete tmp2;
        }
        
    }
    
    void swap1(DiskDrive *diskDrive, int *trans_array, bool *bool_array, int &i, int&j, int numberOfFiles, int&empty){
        for(int k = numberOfFiles + 2; k < diskDrive->getCapacity(); k++){
            if(bool_array[ k ] || !diskDrive->FAT[ k ]){
                DiskBlock* tmp2 = diskDrive->readDiskBlock(j);
                DiskBlock* tmp = diskDrive->readDiskBlock(i);
                diskDrive->writeDiskBlock(tmp, k);
                diskDrive->writeDiskBlock(tmp2, i);
                trans_array[i] = k;
                j = tmp2->getNext();
                
                bool_array[j] = true;
                diskDrive->FAT[j] = true;
                
                bool_array[k] = false;
                diskDrive->FAT[k] = false;
                empty--;
                delete tmp;
                delete tmp2;
                break;
            }
        }
    }

    
//structure 2
 /*
    Defragmenter(DiskDrive *diskDrive){
        bool bool_array[diskDrive->getCapacity()];
        Node item_not_find;
        //if (item_not_find.index == -1)
        //    cout << "hehe" << endl;
        QuadraticHashTable<Node> myHT(item_not_find, 100);
        
        int count = 2;
        for( int i = 0; i < diskDrive->getNumFiles(); i++){
            int index = diskDrive->directory[i].getFirstBlockID();
            diskDrive->directory[i].setFirstBlockID(count);
            
            Node target_node = Node(index, new DiskBlock);
            //cout <<  "break" << endl;
            Node find_result = myHT.find(target_node);
            //cout << find_result.index << endl;
            
            if(diskDrive->FAT[count] && !bool_array[count]){
                myHT.insert(Node( count, diskDrive->readDiskBlock(count)));
            }
            else{
                diskDrive->FAT[count] = true;
            }
            
            if(find_result.index == -1){
                if(count != index){
                    bool_array[index] = true;
                    DiskBlock *tmp = diskDrive->readDiskBlock(index);
                    diskDrive->writeDiskBlock(tmp, count);
                    index = tmp->getNext();
                    delete tmp;
                    
                }
                else{
                    DiskBlock *tmp = diskDrive->readDiskBlock(index);
                    index = tmp->getNext();
                    delete tmp;
                }
            }
            else{
                diskDrive->writeDiskBlock(find_result.obj, count);
                index = find_result.obj->getNext();
                myHT.remove(find_result);
            }
            
            
            count++;
            
            while(index != 1){
                target_node = Node(index, new DiskBlock);
                find_result = myHT.find(target_node);
                
                if(diskDrive->FAT[count] && !bool_array[count]){
                    myHT.insert(Node( count, diskDrive->readDiskBlock(count)));
                }
                
                if(find_result.index == -1){
                    if(count != index){
                        bool_array[index] = true;
                        DiskBlock *tmp = diskDrive->readDiskBlock(index);
                        diskDrive->writeDiskBlock(tmp, count);
                        index = tmp->getNext();
                        delete tmp;
                    }
                    else{
                        DiskBlock *tmp = diskDrive->readDiskBlock(index);
                        index = tmp->getNext();
                        delete tmp;
                    }
                }
                else{
                    diskDrive->writeDiskBlock(find_result.obj, count);
                    index = find_result.obj->getNext();
                    myHT.remove(find_result);
                }
                
                count++;
            }
            //if(i == 0)
            //    break;
            //diskDrive->print();
            
            
        }
  
        while(count < diskDrive->getCapacity()){
            //cout << "count = " << count << endl;
            if(diskDrive->FAT[count]){
                DiskBlock *tmp = new DiskBlock;
                diskDrive->writeDiskBlock(tmp, count);
                delete tmp;
            }
            count++;
        }
  
  
        //diskDrive->print();
        
    }
    */
    

    
}; // class Defragmenter
#endif
