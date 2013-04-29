{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}

module Numerics.Simple.KernelPOC where

import Data.Primitive 
import Numerics.Simple.Bits
import Control.Monad.Primitive (touch )

import Foreign.Ptr
import Foreign
import Foreign.C.Types

import qualified Data.Vector.Storable.Mutable as SM 
{-

For matrix mult
 
R= M N
  
z|N
----
M|R|y 
  x

ie

R has a columns and  b rows (x and y). We cycle through columns more than rows ,
            this is Morton Z  (x,y) 

N has a columns and  c rows  ( x and z ).  MortonN   x z   = Morton z x

M has c columns  b rows (z and y )    

for now we have M and R as Morton Z, and N use morton N

we'll use that for all x,y that MortZ(x,y) == MortFlipN(y,x)


R(MortZ(x,y)) += M(MortZ(z,y))  * N(MortFlipN(x,z))

we have a stepCounter
we're looping over all blocks / elements in the order 
dictated by following the MortonZ order of R.

the relative number of times we increment x y and z 
are 
#z = #x * 2 = #y * 4

ie 

z+=1 every step
x+=1 every time 0 = step mod 2
y+=1 every time 0 = step mode 4


-}


foreign import ccall unsafe "simplemat.c SimpleMatMult4x4" 
    c_SimpleMatMult4x4 :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

{-# NOINLINE quadDirectSimpleC #-}
quadDirectSimpleC :: SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO ()
quadDirectSimpleC !res !leftM !rightM = 
    SM.unsafeWith res $! \a -> 
        SM.unsafeWith leftM $! \b ->
            SM.unsafeWith rightM $! \c ->  c_SimpleMatMult4x4  (castPtr a)  (castPtr b) (castPtr c)

{-# NOINLINE quadDirectSimpleWithShiftC #-}
quadDirectSimpleWithShiftC :: Int -> Int -> Int ->  SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO ()
quadDirectSimpleWithShiftC aix !bix !cix  !res !leftM !rightM  = 
    SM.unsafeWith res $! \a -> 
        SM.unsafeWith leftM $! \b ->
            SM.unsafeWith rightM $! \c ->  
                c_SimpleMatMult4x4  (a `plusPtr` (blockSizeBytes* aix))  ( b `plusPtr` (blockSizeBytes* bix)) (c `plusPtr` (blockSizeBytes* cix))
    where 
        !blockSizeBytes = 8 * 4 * 4 -- double is 64bits, 8 bytes, and we are considering 4x4 blocks

data OrdinateTriple = OTrip { x :: !Double , y :: !Double , z :: ! Double }


type Kerfun  b= Int -> Int -> Int->  b 

 {--}
-- place holder for now

-- simple looping code
{--}


--- also should try the recursive one with explicit stacky loop as
---- data structures or continuations next


---- its not the id kernel!! 
{-# INLINE basicKernel #-}
basicKernel :: Int -> Int -> Int->IOVectDouble -> IOVectDouble -> IOVectDouble -> IO ()
basicKernel  !aix !bix !cix aMat bMat cMat =  
                         do 
                            --touch aix 
                            --touch bix
                            --touch cix 
                            quadDirectSimpleWithShiftC aix bix cix aMat bMat cMat
                            return ()


dumbLooper !rMat !aMat !bMat !n = go 1
    where
        ncube = n * n * n
        go i | i <  ncube = do touch rMat ; go (i + 1)
             |  otherwise = return () 


{-# INLINE simpleLooper #-}
simpleLooper :: IOVectDouble -> IOVectDouble -> IOVectDouble-> Int  -> IO ()
simpleLooper !rMat !aMat !bMat !n = go 0 0 0  0  --- we're about to run step 0!!
    where 
        !blockedN = n `div` 4 --- 4x4
        !blockCubed = blockedN * blockedN * blockedN
        go !x !y !z   !count |  (count < blockCubed) =   --- this seems wrong, but whatever
                                          --- if we hit the bounds all at once, we win!
                         do   
                            
                            next x y z 
                            go (modN $! x+4 ) (modN $! y+2) (modN $! z+8) (count + 8)
                    | otherwise =  
                            do 
                                appKernel64 basicKernel x y z  rMat  aMat bMat 
                                return ()
        theKernel x y z =   appKernel64 basicKernel x y z  rMat  aMat bMat                             
        modN !j = mod j blockedN 
        next !x !y !z  =
                    do 
                        theKernel x y z  -- 1
                        theKernel x y (z + 1) -- 2 
                        theKernel (x+1) y (z+2) -- 3
                        theKernel (x+1) y (z+3) -- 4
                        theKernel (x+2) (y+1) (z+4) -- 5
                        theKernel (x+2) (y+1) (z+5) -- 6
                        theKernel (x+3) (y+1) (z+6) -- 7
                        theKernel (x+3) (y+1) (z+7) -- 8       
 
{-
Lets unroll the Next code so theres no branches in the incrementation!



-}


-- this way i can apply prepkernel64 multiple times! 
-- wh
{-# INLINE appKernel64  #-}
appKernel64::  (Int -> Int -> Int ->  b)  -> Int  -> Int -> Int -> b 
appKernel64 !kerf   = go 
    where 
        go !x !y !z  = kerf x y z  --- kerf ( mortonZ64 x y ) (mortonZ64 z y) (mortonFlipN64 x z )

type IOVectDouble = SM.IOVector Double 


{-
we have variables a,b,c
which correspond to the 
for simplicity lets con
-}
