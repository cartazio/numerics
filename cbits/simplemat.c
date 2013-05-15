
// #include <emmintrin.h>
#include <immintrin.h>
// #if defined (__SSE4_2__) || defined (__SSE4_1__)
// for some reason i can't get anything newer than sse4.2 work with both gcc-clang and clang
 // #include <smmintrin.h>
// #endif





/*

-- aligned packed load VMOVAPD
__m128d _mm_load_pd (double const * p);

--aligned packed store
 void  _mm_store_pd(double * p, __m128d a);

--add
 __m128d _mm_add_pd (__m128d a, __m128d b)

--- mult
 __m128d _mm_mul_pd (m128d a, m128d b)

-- dot product
 __m128d _mm_dp_pd ( __m128d a, __m128d b, const int mask);
*/
#define restrict __restrict
typedef  __attribute__((__aligned__(16)))  double doubleAl; 

/*


BLENDPD: __m128d _mm_blend_pd (__m128d v1, __m128d v2, const int mask);
*/

/*
__m256d

DPPD: __m128d _mm_dp_pd ( __m128d a, __m128d b, const int mask);



for the dot product masking, an 8 bit (byte) mask is used,
the lower nibble is where to place the result in the vector (which subset of the entries)
the upper nibble is which entries to add to dot product
we use the full nibble in the 4 entry  (floats) case, and the lower part of a 
nibble in the 

(0b11110001)0xF1 = 4d float dot, put result in the first slot
0xFF = 4d float dot, put result in all 4 slots

(0b00110001)0x31 = 2d dot (eg for doubles), put result in first slot
0x32 = 2d dot,  put result in 2nd slot

*/

// this is ok for stand alone 2x2, can do better for 4x4 if carefule
inline void  matMultAvx2x2(double*  res, double*  leftM ,double* rightM){
    __m128d resRow1 = _mm_load_pd(res); // 1
    __m128d resRow2= _mm_load_pd(res + 2) ;  //2
    __m128d leftRow1 = _mm_load_pd(leftM); //3 
    __m128d leftRow2= _mm_load_pd(leftM+2); //4 
    __m128d rightCol1 = _mm_load_pd(rightM); // 5 
    __m128d rightCol2 = _mm_load_pd(rightM+2); // 6 
    // i'm  using 6 xmm registers at this point, plenty left!
    __m128d res11 =  _mm_dp_pd(leftRow1,rightCol1,0x31); // 7
    __m128d res12 = _mm_dp_pd(leftRow1,rightCol2,0x32);  // 8
    __m128d res21 = _mm_dp_pd(leftRow2, rightCol1, 0x31); // 9 
    __m128d res22 = _mm_dp_pd(leftRow2,rightCol2,0x32); // 10
    resRow1 =  res11 + res12 ; // for some reason i don't trust that this does the expected thing
    resRow2 = res21 + res22 ; 
    _mm_store_pd(res, resRow1);
    _mm_store_pd(res+2,resRow2);

}





/* only welldefined in the AVXMatMult4x4 codes, and thats ok
i think theres JUST enough xmm registers for this to squeek by
*/
#define dotProd4x4RowBlock(ix) { \
    resRowLeft = _mm_load_pd(resF + (ix)) ; \
    resRowRight = _mm_load_pd(resF + 2 + (ix)); \
    lMatRowLeft = _mm_load_pd(leftMF + (ix) ) ; \
    lMatRowRight= _mm_load_pd(leftMF + (ix) + 2) ; \
    resRowLeft += _mm_dp_pd(lMatRowLeft, rMatCol1Up, 0x31) + _mm_dp_pd(lMatRowRight, rMatCol1Down, 0x31); \
    resRowLeft += _mm_dp_pd(lMatRowLeft, rMatCol2Up, 0x32) + _mm_dp_pd(lMatRowRight, rMatCol1Down, 0x32); \
    resRowRight += _mm_dp_pd(lMatRowLeft, rMatCol3Up, 0x31) + _mm_dp_pd(lMatRowRight, rMatCol3Down, 0x31); \
    resRowRight += _mm_dp_pd(lMatRowLeft, rMatCol4Up, 0x32) + _mm_dp_pd(lMatRowRight, rMatCol4Down, 0x32); \
    _mm_store_pd(resF + (ix), resRowLeft); \
    _mm_store_pd(resF + 2 + (ix),resRowRight); \
}

// void AVXMatMult4x4( doubleAl * restrict resF,doubleAl * restrict leftMF,  doubleAl *restrict rightMF){
void SimpleMatMult4x4( doubleAl * restrict resF,doubleAl * restrict leftMF,  doubleAl *restrict rightMF ){
    __m128d rMatCol1Up = _mm_load_pd(rightMF);
    __m128d rMatCol1Down = _mm_load_pd(rightMF+2);
    __m128d rMatCol2Up= _mm_load_pd(rightMF+4);
    __m128d rMatCol2Down = _mm_load_pd(rightMF+ 6);
    __m128d rMatCol3Up = _mm_load_pd(rightMF+8 );
    __m128d rMatCol3Down = _mm_load_pd(rightMF+10);
    __m128d rMatCol4Up = _mm_load_pd(rightMF+12);
    __m128d rMatCol4Down = _mm_load_pd(rightMF+14); 

    __m128d resRowLeft ;
    __m128d resRowRight ;

    __m128d lMatRowLeft ;
    __m128d lMatRowRight ;
    // at this point i'm using 12 XMM registers, have 4 xmm left!
    // which gives me a "budget" of up to 4 intermediaries at time!


    // i do this macro 4 times!

    dotProd4x4RowBlock(0) ;
    dotProd4x4RowBlock(4) ;
    dotProd4x4RowBlock(8) ; 
    dotProd4x4RowBlock(12) ; 

}


 /*void SimpleMatMult4x4( doubleAl * restrict res,doubleAl * restrict leftM,  doubleAl *restrict rightM
                    ){


// intialize these things

    // copyFromTo(resF,res,16);
    // copyFromTo(leftMatF,leftM, 16);
    // copyFromTo(rightMatF,rightM,16);

    // quadrant 1
    // __builtin_prefetch(leftM+4,0);  // 1
    // __builtin_prefetch(rightM+4,0); // 2
    matMultAvx2x2(res,leftM, rightM);

    // __builtin_prefetch(res+4,1);  // 3
    // __builtin_prefetch(rightM+8,0); // 4
    matMultAvx2x2(res, leftM+4,rightM + 4 );

    // quadrant 2
    // __builtin_prefetch(rightM+12,0); // 5
    matMultAvx2x2(res + 4, leftM, rightM + 8);
    // __builtin_prefetch(res+8,1); // 6
    // __builtin_prefetch(leftM+8, 0); // 7
    matMultAvx2x2(res+4, leftM + 4 , rightM + 12);

    //quadrant 3
    // __builtin_prefetch(leftM+12, 0); // 8
    matMultAvx2x2(res + 8, leftM + 8 , rightM);
    // __builtin_prefetch(res + 12,1);  // 9, yes i've done all the prefetches
    matMultAvx2x2(res+8, leftM+12, rightM + 4);

    //quadrant 4
    matMultAvx2x2(res + 12 , leftM+ 8, rightM + 8);
    matMultAvx2x2(res + 12 , leftM + 12, rightM + 12);

    //write the results back, hopefully clang can do the write coalescing this way!!
    // copyFromTo(res,resF,16);



}*/


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
