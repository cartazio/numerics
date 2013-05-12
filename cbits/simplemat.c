
// #include <emmintrin.h>
#include <immintrin.h>
// #if defined (__SSE4_2__) || defined (__SSE4_1__)
// for some reason i can't get anything newer than sse4.2 work with both gcc-clang and clang
 // #include <smmintrin.h>
// #endif


//    __builtin_prefetch(&a, rw, locality);


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

// typedef __v4df vec4df ;
// typedef __v2df vec2df ; 
// typedef double scalarDouble;


/*

-- aligned packed load
__m128d _mm_load_pd (double const * p);

--aligned packed store
 __mm_store_pd(double * p, __m128d a);

--add
 __m128d _mm_add_pd (__m128d a, __m128d b)

--- mult
 __m128d _mm_mul_pd (m128d a, m128d b)

-- dot product
 __m128d _mm_dp_pd ( __m128d a, __m128d b, const int mask);
*/
#define restrict __restrict

/*
__m256d


DPPD: __m128d _mm_dp_pd ( __m128d a, __m128d b, const int mask);
*/

__v4df avxid(__v4df in){
    return in ; 
}

typedef  __attribute__((__aligned__(16)))  double doubleAl; 

inline void  SimpleMatMult2x2( doubleAl  *restrict res,
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


void SimpleMatMult4x4( doubleAl * restrict resF,doubleAl * restrict leftMF,  doubleAl *restrict rightMF
    // should macroize all the variations so its easier to 
                    // ,doubleAl *nextRes, doubleAl *nextLeft,doubleAl nextRight
                    ){
    double res[16];
    double leftM[16];
    double rightM[16];

// intialize these things

    // copyFromTo(resF,res,16);
    // copyFromTo(leftMatF,leftM, 16);
    // copyFromTo(rightMatF,rightM,16);

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

    //write the results back, hopefully clang can do the write coalescing this way!!
    copyFromTo(res,resF,16);



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

void SimpleMatMult4x4Prefetcher( doubleAl * restrict res,doubleAl * restrict leftM,  doubleAl *restrict rightM
    // should macroize all the variations so its easier to 
                    ,int nextRes, int nextLeft,int nextRight
                    ){
    // double res[16];
    // double leftM[16];
    // double rightM[16];

// intialize these things

    // copyFromTo(resMat,res,16);
    // copyFromTo(leftMat,leftM, 16);
    // copyFromTo(rightMat,rightM,16);

    // quadrant 1
    // __builtin_prefetch(leftM+4,0);  // 1
    // __builtin_prefetch(rightM+4,0); // 2
    SimpleMatMult2x2(res,leftM, rightM);

    // __builtin_prefetch(res+4,1);  // 3
    // __builtin_prefetch(rightM+8,0); // 4
    SimpleMatMult2x2(res, leftM+4,rightM + 4 );

    // quadrant 2
    // __builtin_prefetch(rightM+12,0); // 5
    SimpleMatMult2x2(res + 4, leftM, rightM + 8);
    // __builtin_prefetch(res+8,1); // 6
    // __builtin_prefetch(leftM+8, 0); // 7
    SimpleMatMult2x2(res+4, leftM + 4 , rightM + 12);

    //quadrant 3
    // __builtin_prefetch(leftM+12, 0); // 8
    SimpleMatMult2x2(res + 8, leftM + 8 , rightM);
    // __builtin_prefetch(res + 12,1);  // 9, yes i've done all the prefetches
    SimpleMatMult2x2(res+8, leftM+12, rightM + 4);

    //quadrant 4
    SimpleMatMult2x2(res + 12 , leftM+ 8, rightM + 8);
    SimpleMatMult2x2(res + 12 , leftM + 12, rightM + 12);

    //write the results back, hopefully clang can do the write coalescing this way!!
    // copyFromTo(res,resMat,16);



    // speculative next bunch prefetch hinting, using level 2 rather than 3, not sure if theres a diff
    // but gives it lower relative priority

//  i now can compute the next location stuff, use it in a new version
    int i = 0 ;
    for(i = 0 ; i < 4; i ++){
        __builtin_prefetch(res + nextRes + (4* i), 1, 0);
        __builtin_prefetch(leftM+nextLeft+ (4* i),0, 0);
        __builtin_prefetch(rightM+nextRight+ (4* i),0, 0);
    }
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

void SimpleRowRowColMatrixMultViaDots(doubleAl  *restrict  res,doubleAl   *restrict  leftM,  doubleAl   *restrict rightM , int nSize   ){
    int row = 0 ;
    int col = 0;
    int ix = 0;
    double accum = 0.0; 
    for(row = 0 ; row < nSize ; row ++ ){
        for(col=0; col < nSize ; col ++){
            accum = 0.0; 
            for(ix = 0 ;  ix < nSize; ix ++ ){
                accum += leftM[row + ix * nSize] *  rightM[col +  nSize * ix]; 
            }
            res[row + nSize * col] = accum ;
        }


    }

}
