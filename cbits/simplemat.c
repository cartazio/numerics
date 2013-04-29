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


/*
3 2x2 matrics, row major, row major, col major repsectively
stride of 1, ie no bs!

*/

#define restrict __restrict
typedef  __attribute__((__aligned__(16)))  double doubleAl; 

static inline void  SimpleMatMult2x2( doubleAl  *restrict res,
     doubleAl *restrict leftM, doubleAl *restrict rightM){
        res[0] = res[0] +( leftM[0] * rightM [0] + leftM[1] * rightM[1]   );
        res[1] = res[1] +(leftM[0] * rightM[2] + leftM[1] * rightM[3]);
        res[2] = res[2] +(leftM[2] * rightM[0]+ leftM[3] * rightM[1]);
        res[3] = res[3] +(leftM[2] * rightM[2]+ leftM[3] * rightM[3]);

    } 

 static inline void copyFromTo(doubleAl *from, doubleAl *to, int len){
    int i= 0;
    for( i =0 ; i < len; i ++){
        to[i]= from[i];

    }

}


void SimpleMatMult4x4( doubleAl * restrict res,doubleAl * restrict leftM,  doubleAl *restrict rightM){
    // double res[16];
    // double leftM[16];
    // double rightM[16];

// intialize these things

    // copyFromTo(resMat,res,16);
    // copyFromTo(leftMat,leftM, 16);
    // copyFromTo(rightMat,rightM,16);

    int i = 0;
    for(i=0;i<4;i++){
                    // quadrant 1
                    __builtin_prefetch(leftM+4,0);  // 1
                    __builtin_prefetch(rightM+4,0); // 2
                    SimpleMatMult2x2(res,leftM, rightM);
    
                    __builtin_prefetch(res+4,1);  // 3
                    __builtin_prefetch(rightM+8,0); // 4
                    SimpleMatMult2x2(res, leftM+4,rightM + 4 );
    
                    // quadrant 2
                    __builtin_prefetch(rightM+12,0); // 5
                    SimpleMatMult2x2(res + 4, leftM, rightM + 8);
                    __builtin_prefetch(res+8,1); // 6
                    __builtin_prefetch(leftM+8, 0); // 7
                    SimpleMatMult2x2(res+4, leftM + 4 , rightM + 12);
    
                    //quadrant 3
                    __builtin_prefetch(leftM+12, 0); // 8
                    SimpleMatMult2x2(res + 8, leftM + 8 , rightM);
                    __builtin_prefetch(res + 12,1);  // 9, yes i've done all the prefetches
                    SimpleMatMult2x2(res+8, leftM+12, rightM + 4);
    
                    //quadrant 4
                    SimpleMatMult2x2(res + 12 , leftM+ 8, rightM + 8);
                    SimpleMatMult2x2(res + 12 , leftM + 12, rightM + 12);
    }
    //write the results back, hopefully clang can do the write coalescing this way!!
    // copyFromTo(res,resMat,16);



    // speculative next bunch prefetch hinting, using level 2 rather than 3, not sure if theres a diff
    // but gives it lower relative priority

//  i now can compute the next location stuff, use it in a new version
    __builtin_prefetch(res + 16, 1, 0);
    __builtin_prefetch(leftM+16,0, 0);
    __builtin_prefetch(rightM+16,0, 0);

    // __builtin_prefetch(res + 16 + 4, 1, 3);
    // __builtin_prefetch(res + 16 + 8, 1,3);
    // __builtin_prefetch(res+ 16 + 12, 1, 3);

    // __builtin_prefetch(leftM + 16 + 4, 0, 3);
    // __builtin_prefetch(leftM + 16 + 8, 0,3);
    // __builtin_prefetch(leftM + 16 + 12, 0, 3);

    // __builtin_prefetch(rightM + 16 + 4, 0, 3);
    // __builtin_prefetch(rightM + 16 + 8, 0,3);
    // __builtin_prefetch(rightM+ 16 + 12, 0, 3);

}




