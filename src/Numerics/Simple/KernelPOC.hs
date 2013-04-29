{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}

module Numerics.Simple.KernelPOC where

import Data.Primitive 
import Numerics.Simple.Bits
import Control.Monad.Primitive (touch )

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

data OrdinateTriple = OTrip { x :: !Double , y :: !Double , z :: ! Double }


type Kerfun  b= Int -> Int -> Int->  b 

 {--}
-- place holder for now

-- simple looping code
{--}

{-# INLINE idKernel #-}
idKernel :: Int -> Int -> Int->IOVectDouble -> IOVectDouble -> IOVectDouble -> IO ()
idKernel  !aix !bix !cix aMat bMat cMat =  
                         do 
                            touch aix
                            touch bix
                            touch cix
                            touch aMat
                            touch bMat
                            touch cMat
                            return ()

{-# INLINE simpleLooper #-}
simpleLooper :: IOVectDouble -> IOVectDouble -> IOVectDouble-> Int  -> IO ()
simpleLooper !rMat !aMat !bMat !n = go 0 0 0  0  --- we're about to run step 0!!
    where 
        !blockedN = n `div` 4 --- 4x4
        go !x !y !z   !count | x < (blockedN-1) && y < (blockedN-1) && z < (blockedN-1) =  
                         do   
                            appKernel64 idKernel x y z  rMat aMat bMat
                            next x y z (count + 1)
                    | otherwise =  
                            do 
                                appKernel64 idKernel x y z  rMat  aMat bMat 
                                return ()
        modN !j = mod j blockedN 
        next !x !y !z count |  mod count 4 == 0 = go (modN (x+1)) (modN (y+1)) (modN (z+1)) count
                         | mod count 2 ==0  =  go (modN (x+1)) y (modN (z+1)) count
                         | otherwise = go x y (modN  (z + 1)) count 




-- this way i can apply prepkernel64 multiple times! 
-- wh
{-# INLINE appKernel64  #-}
appKernel64::  (Int -> Int -> Int ->  b)  -> Int  -> Int -> Int -> b 
appKernel64 !kerf   = go 
    where 
        go !x !y !z  =  kerf ( mortonZ64 x y ) (mortonZ64 z y) (mortonFlipN64 x z )

type IOVectDouble = SM.IOVector Double 


{-
we have variables a,b,c
which correspond to the 
for simplicity lets con
-}
