{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}

module Numerics.Simple.POC where

import Prelude hiding ((>>))

import Numerics.Simple.Bits
--import qualified  Data.Vector.Storable as S 
import qualified Data.Vector.Storable.Mutable as SM 




{-
convention in this module
matrix M  =  decomposes as 
 
 A | B
 -----
 C | D 

Quad a = Q (!(!a,!a),!(!a,!a))

q1 | q2
-------
q3 | q4

-}
--- lets just use the data.vector.storable unsafe slice internall

data Quad a = QD {q1:: !a,q2::  !a,q3:: !a,q4:: !a}

newtype MortonZ a = MZ a

-- well its upside down N, but whatever
newtype MortonN a = MN a

unsafeDiceMZ :: 
    SM.Storable a => 
    MortonZ (SM.MVector s a) ->  Quad(MortonZ  (SM.MVector s a))
unsafeDiceMZ  (MZ v) = 
        QD  (MZ $! SM.unsafeSlice q1Base (q1Base + lenOffset) v )
            (MZ $! SM.unsafeSlice q2Base (q2Base + lenOffset) v )
            (MZ $! SM.unsafeSlice q3Base (q3Base + lenOffset) v )
            (MZ $! SM.unsafeSlice q4Base (q4Base + lenOffset)  v )
    where
        !len = (parentLength ) >> 2 ---  divide by 4
        !parentLength = (SM.length v )
        !q1Base = 0
        !q2Base = len
        !q3Base = len << 1   --- 2* len 
        !q4Base = parentLength - len    --- (4*len - len )
        !lenOffset = len -1 

unsafeDiceMFlipN
  :: SM.Storable a =>
     MortonN (SM.MVector s a) -> Quad (MortonN  (SM.MVector s a))
unsafeDiceMFlipN (MN v) =  
        QD  (MN $!  SM.unsafeSlice q1Base (q1Base + lenOffset) v )
            (MN $! SM.unsafeSlice q2Base (q2Base + lenOffset) v )
            (MN $! SM.unsafeSlice q3Base (q3Base + lenOffset) v )
            (MN $! SM.unsafeSlice q4Base (q4Base + lenOffset)  v )
    where
        !len = (parentLength ) >> 2 ---  divide by 4
        !parentLength = (SM.length v )
        !q1Base = 0
        !q2Base = len << 1   --- MN same as MZ but with the base index for q2 and q3 swapped
        !q3Base = len    
        !q4Base = parentLength - len    --- (4*len - len )
        !lenOffset = len -1   -- (we're 0 base indexed after all)

-- i'm assuming you're giving me quadrants that are each 2x2 mats 

unsafeQuadDirectMzMn2MzMMult  resQuad readLQuad  readRQuad= undefined

-- i'm fed 3 2x2 matrices, i'll work on reading / writing them directly

unsafeDirect2x2MzMnMz (MZ resMat) (MZ leftReadMat) (MN rightReadMat) = 
        do 
            --undefined
            return 1 



{-
the code i'm writing in this module 
will only work on morton z and flip n  that are square
and whose "elements" are 2x2 blocks

size as an array would be 2 * 2 * 2^k * 2^k 
ie 4* 2^(2k)

-}                        

