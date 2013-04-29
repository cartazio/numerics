# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}

module Numerics.Simple.KernelPOC where

import Data.Primitive 
import Numerics.Simple.Bits


import qualified Data.Vector.Storable.Mutable as SM 
{-

For matrix mult
 
R= M N
  
c|N
----
M|R|b 
  a

ie

R has a columns and  b rows (x and y). We cycle through columns more than rows (Morton Z  y x )

N has a columns and  c rows  ( x and z ).

M has c columns  b rows (z and y )

for now we have M and R as Morton Z, and N use morton N

we'll use that for all x,y that MortZ(x,y) == MortFlipN(y,x)


R(MortZ(a,b)) += M(MortZ(c,b))  * N(MortZ(a,c))


-}

data OrdinateTriple = OTrip 


type Kerfun  b= Int -> Int -> Int->  b 
-- place holder for now


-- this way i can apply prepkernel64 multiple times! 
-- wh
appKernel64::  (Int -> Int -> Int ->  b)  -> Int  -> Int -> Int -> b 
apppKernel64 !kerf   = go 
    where 
        go !x !y !z  =  kerf ( mortonZ64 b a ) (mortonZ64 c b) (mortonZ64 a c )

type IOVectDouble = SM.IOVector Double 

squareMMultLoop :: (Kerfun (   IOVectDouble -> IOVectDouble -> IOVectDouble -> ) -> Int -> IOVectDouble -> IOVectDouble  -> IOVectDouble

{-
we have variables a,b,c
which correspond to the 
for simplicity lets con

