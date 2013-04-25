


void prefetchRead3(const void *addr){
    __builtin_prefetch(addr, 0, 3) ;
    

}

void prefetchWrite3(const void *addr){
     __builtin_prefetch(addr,1,3);
    



}