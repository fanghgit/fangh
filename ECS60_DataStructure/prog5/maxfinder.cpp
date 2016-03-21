// Author: Sean Davis

#include "NetDriver.h"
#include "maxfinder.h"
#include <iostream>

using namespace std;

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

MaxFinder::MaxFinder(const Computer *c, int nc,
  int nt)
{
    computers = c;
    numComputers = nc;
    numTerminals = nt;
    add = new Address[numComputers];
    for(int i = 0; i < numComputers; i++){
        for(int j = 0; j < 16; j++){
            add[i].address[j] = computers[i].address[j];
        }
        add[i].index = convert(add[i].address);
        add[i].info = true;
        add[i].num = i;
    }
} // MaxFinder()




int MaxFinder::calcMaxFlow(Edge *edges, int numEdges)
{
    //CPUTimer ctt;
    //ctt.reset();
    corrdinate find_corr;
    
    vertex *ver = new vertex[numComputers];
    for( int i = 0; i < numComputers; i++){
        ver[i].index = i;
        //cout << "dist " << ver[i].dist << endl;
    }
    
    
    HashTable HT;
    for( int i = 0; i < numComputers; i++){
        HT.insert(add[i]);
    }
    //cout << "hehe " << endl;
    
    
    for(int i = 0; i < numEdges; i++){
        //cout << "hehe " << endl;
        find_corr.src = HT.search(edges[i].src);
        find_corr.dest = HT.search(edges[i].dest);
        //find_corr = find(edges[i], numComputers);
        //cout << edges[i].src << " " << edges[i].dest << endl;
        //dist_array[find_corr.src][find_corr.dest] = -edges[i].capacity;
        ver[find_corr.src].neigthbours.insert( pt(find_corr.dest, edges[i].capacity) );
        //inverse edge
        ver[find_corr.dest].neigthbours.insert( pt(find_corr.src, 0) );
    }
    //cout << "CPU for initial: " << ctt.cur_CPUTime() << endl;
    //cout << ver[i].neighbours.
    
    //for(int i = 0; i < numComputers; i++){
    //    ver[i].print();
    //    cout << "\n";
    //}
    

    
    int tmp = 0;
    int result = tmp;
    
    int count = 0;
    //int max = 0;
    
    for(int i = 0; i < numTerminals; i++){
        tmp = dfs(i, ver, numComputers);
        //count++;
        result += tmp;
        while(tmp != 0){
            tmp = dfs(i, ver, numComputers);
            result += tmp;
            //count++;
            //if(count > 1000)
            //    goto finish;
        }
    }
    
    
    
    
    
    
    //int count = 0;
    tmp = 0;
    int max = 0;
    //int result = max;
    
    //for(int i = 0; i < numTerminals; i++){
    //    tmp = dfs(i, ver, numComputers);
    //    count++;
    //    result += tmp;
    //    if(max < tmp)
    //        max = tmp;
    //}

    //int max = dfs(0, ver, numComputers);
    
    while(max != 0){
        max = 0;
        for(int i = 0; i < numTerminals; i++){
            tmp = dfs(i, ver, numComputers);
            //count++;
            result += tmp;
            if(max < tmp)
                max = tmp;
        }
    }
    
    
    //cout << "count = " << count << endl;

    return result;  // bad result :(
} // calcMaxFlow()




bool MaxFinder::Update(int start, vertex *ver, int numComputers){
    ListNode* tmp = ver[start].neigthbours.header->next;

    if(start == (numComputers - 1)){
        return true;
    }
    else{
        tmp = ver[start].neigthbours.header->next;
        while(tmp){
            if(!ver[tmp->point.index].known && tmp->point.capacity > 0){
                //cout << "change" << tmp->point.index << endl;
                ver[tmp->point.index].known = true;
                ver[tmp->point.index].path = start;
                ver[tmp->point.index].cp = tmp->point.capacity;
                if(Update(tmp->point.index, ver, numComputers))
                    return true;
            }
            tmp = tmp->next;
        }
        return false;
    }
    
}



/*

bool MaxFinder::Update(int start, vertex *ver, int numComputers){
    ListNode* tmp = ver[start].neigthbours.header->next;
    //cout << "start: " << start << endl;
    bool mark = false;
    while(tmp){
        //cout << "try" << endl;
        //cout << tmp->point.index << endl;
        if(!ver[tmp->point.index].known && tmp->point.capacity > 0){
            //cout << "find" << endl;
            //cout << "change" << endl;
            mark = true;
            //ver[tmp->point.index].known = true;
            //ver[tmp->point.index].path = start;
            if(tmp->point.index == (numComputers - 1)){
                ver[tmp->point.index].path = start;
                ver[tmp->point.index].cp = tmp->point.capacity;
                return true;
            }
        }
        tmp = tmp->next;
    }
    
    if(!mark){
        return false;
    }
    else{
        tmp = ver[start].neigthbours.header->next;
        while(tmp){
            if(!ver[tmp->point.index].known && tmp->point.capacity > 0){
                //cout << "change" << tmp->point.index << endl;
                ver[tmp->point.index].known = true;
                ver[tmp->point.index].path = start;
                ver[tmp->point.index].cp = tmp->point.capacity;
                if(Update(tmp->point.index, ver, numComputers))
                    return true;
            }
            tmp = tmp->next;
        }
        return false;
    }
    
}
*/



int MaxFinder::dfs(int start, vertex *ver, int numComputers){
    //initialization vertexes
    //vertex *ver = new vertex[numComputers];
    
    int maxflow;
    for(int i = 0; i < numComputers; i++){
        ver[i].known = false;
        ver[i].path = -1;
        ver[i].cp = 0;
    }
    ver[start].known = true;
    
    if(Update(start, ver, numComputers)){
        //cout << "hehe" << endl;
        //for(int i = 0; i < numComputers; i++){
        //    cout << "index: " << ver[i].index << ", path: " << ver[i].path << ", cp = " << ver[i].cp << endl;
        //}
    
        vertex tmp = ver[numComputers - 1];
        int src = tmp.path;
        int dest = tmp.index;
        maxflow = tmp.cp;
        while(src != -1){
            if(src != start && maxflow > ver[src].cp){
                //cout << "cp: " << ver[src].cp << endl;
                maxflow = ver[src].cp;
            }
            tmp = ver[src];
            dest = src;
            src = tmp.path;
        }
    
    
    
        tmp = ver[numComputers - 1];
        src = tmp.path;
        dest = tmp.index;
    
    
        while(src != -1){
            //cout << "hehe" << endl;
            ListNode *tmpNode = ver[src].neigthbours.header->next;
            while(tmpNode){
                if(tmpNode->point.index == dest){
                    tmpNode->point.capacity -= maxflow;
                    //cout << "src = " << src << ", dest = " << dest << ", capacity = " << tmpNode->point.capacity << endl;
                    break;
                }
                tmpNode = tmpNode->next;
            }
            ListNode *tmpNode2 = ver[dest].neigthbours.header->next;
            while(tmpNode2){
                if(tmpNode2->point.index == src){
                    tmpNode2->point.capacity += maxflow;
                    break;
                }
                tmpNode2 = tmpNode2->next;
            }
        
        
            //if(src != start && maxflow < ver[src].cp)
            //    maxflow = ver[src].cp;
        
        
        
            //if(src == start){
            //    cout << "first path " << tmpNode->point.capacity << endl;
            //}
            tmp = ver[src];
            dest = src;
            src = tmp.path;
        
        //cout << "test: " << src << endl;
        
        }

        //for(int i = 0; i < numComputers; i++){
        //    ver[i].print();
        //    cout << "\n";
        //}
    }
    else{
        maxflow = 0;
    }
    
    
    /*
    BinaryHeap<vertex> BH(numComputers + 10);
    //insert
    for(int i = 0; i < numComputers; i++){
        BH.insert(ver[i]);
    }
    //BH.buildHeap();
    //Can be combined with remove
    vertex tmp = BH.findMin();
    
    while(tmp.index != (numComputers - 1))     //check if the destination is found
    {
        //cout << "index: " << tmp.index << ", path: " << tmp.path << ", cp = " << tmp.cp << endl;
        
        BH.deleteMin();
        //cout << "currentSize = " << BH.currentSize << endl;
        ListNode *tmpnode = tmp.neigthbours.header->next;
        while(tmpnode){
            for(int j = 1; j < (BH.currentSize + 1); j++){
            
                if(BH.array[j].index == tmpnode->point.index){
                    //check first
                    if(BH.array[j].dist == 1)
                        BH.array[j].dist = 0;
                    //update
                    if(tmpnode->point.capacity != 0  && (tmpnode->point.capacity + tmp.dist) < BH.array[j].dist){
                        BH.array[j].dist = tmpnode->point.capacity + tmp.dist;
                        if(tmpnode->point.index != start){
                            BH.array[j].path = tmp.index;
                            ver[tmpnode->point.index].path = tmp.index;
                        }
                            
                        BH.array[j].cp = tmpnode->point.capacity;
                        ver[tmpnode->point.index].cp = tmpnode->point.capacity;
                    }
                    
                    break;
                }
            }
            
            tmpnode = tmpnode->next;
        }
        BH.buildHeap();
        
        if(tmp.index == start){
            vertex tmp2 = BH.findMin();
            maxflow = -tmp2.cp;
            //cout << "initial cp: " << maxflow << endl;
        }
        
        tmp = BH.findMin();
        
        if(maxflow > (-tmp.cp)){
            maxflow = -tmp.cp;
        }
        
        //break rule
        if(tmp.dist == 0)
            break;
    }
    //cout << "index: " << tmp.index << ", path: " << tmp.path << ", cp = " << tmp.cp << endl;
    int dest = tmp.index;
    int src = tmp.path;
    //maxflow = -tmp.cp;
    
    */
    
    /*
    vertex tmpver = tmp;
    int src1 = tmpver.path;
    while(src1 != -1){
        if(src1 != start && maxflow > -ver[src1].cp)
            maxflow = -ver[src1].cp;
        tmpver = ver[src1];
        src1 = tmpver.path;
    }
     */
    //cout << "src = " << src << endl;
    //cout << "dest = " << dest << endl;
    
    //cout << "**********************************" << endl;
    //for(int i = 0; i < numComputers; i++){
    //    cout << "index: " << ver[i].index << ", path: " << ver[i].path << ", cp = " << ver[i].cp << endl;
    //}
    
    //cout << "**********************************" << endl;
    
    
    
    
    /*
    while(src != -1){
        //cout << "hehe" << endl;
        ListNode *tmpNode = ver[src].neigthbours.header->next;
        while(tmpNode){
            if(tmpNode->point.index == dest){
                tmpNode->point.capacity -= maxflow;
                //cout << "src = " << src << ", dest = " << dest << ", capacity = " << tmpNode->point.capacity << endl;
                break;
            }
            tmpNode = tmpNode->next;
        }
        ListNode *tmpNode2 = ver[dest].neigthbours.header->next;
        while(tmpNode2){
            if(tmpNode2->point.index == src){
                tmpNode2->point.capacity += maxflow;
                break;
            }
            tmpNode2 = tmpNode2->next;
        }
        */
        
        //if(src != start && maxflow > -ver[src].cp)
        //    maxflow = -ver[src].cp;
        
        
        
        //if(src == start){
        //    cout << "first path " << tmpNode->point.capacity << endl;
        //}
    
    
    /*
        tmp = ver[src];
        dest = src;
        src = tmp.path;
     */
     //cout << "test: " << src << endl;
        
    //}
    
    //cout << "maxflow = " << maxflow << endl;
    return maxflow;
}



