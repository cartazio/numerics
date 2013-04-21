// #include<immintrin.h>


//    __builtin_prefetch(&a, rw, locality);
/******
compatible with both clang and gcc

&a is the address of a >=32byte cache line (64bytes on most modern intel chips)
rw is 0,1 valued, 0 denotes read only location, 1 denotes writing to location
locality takes the values 0,1,2,3 which are hints about how much locality

quoting the gcc manual:

This function is used to minimize cache-miss latency by moving data into a cache before it is accessed. You can insert calls to __builtin_prefetch into code for which you know addresses of data in memory that is likely to be accessed soon. If the target supports them, data prefetch instructions are generated. If the prefetch is done early enough before the access then the data will be in the cache by the time it is accessed.

The value of addr is the address of the memory to prefetch. There are two optional arguments, rw and locality. The value of rw is a compile-time constant one or zero; one means that the prefetch is preparing for a write to the memory address and zero, the default, means that the prefetch is preparing for a read. The value locality must be a compile-time constant integer between zero and three. A value of zero means that the data has no temporal locality, so it need not be left in the cache after the access. A value of three means that the data has a high degree of temporal locality and should be left in all levels of cache possible. Values of one and two mean, respectively, a low or moderate degree of temporal locality. The default is three.


******/

/*
    likewise, approach things as follow:
    initially no vectorization
        then sse2-sse4.2
        then investigate avx (doubtful that it'd help here)

*/