{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numerics.Simple.Bits where

import Data.Bits
import Data.Word
import Prelude hiding ((>>)) 




infixl 8  << , >>
(<<):: Bits a=> a ->Int -> a
(<<) = unsafeShiftL
{-# INLINE (<<) #-}

(>>)::  Bits a=> a ->Int -> a
(>>) = unsafeShiftR
{-# INLINE (>>) #-}


{-----------------------------------

algs from 


http://graphics.stanford.edu/~seander/bithacks.html#InterleaveTableObvious

and Hackers Delight book (1st ed)

---------------------------------} 


--- | quotRemStrong taks an int, the exponent of a number of the form 
---  
unsafeQuotRemPow2  :: Int -> Int -> (# Int, Int #)
unsafeQuotRemPow2  a k  =  (# a >> k , a .&. ((1 << k) -1  ) #) 

quotRemPow2 a k | k <= (bitSize (undefined :: Int) -1 ) = unsafeQuotRemPow2 a k 
                | otherwise = error ("bad call to quotRemPow2 with args: "  ++ show a ++ " " ++ show k  )




{-| word2int and int2word should optimize to being NO OPs -}

word2int :: Word -> Int
word2int x = fromIntegral x
{-# INLINE word2int #-}

int2word :: Int->Word
int2word x = fromIntegral x
{-# INLINE int2word #-}


{-| interleaves the lowest 8 bits of x and y, 64bit only 
    interleave , result should be 16 bits
this is likewise as written  going to do the 
morton Z order on
interleave x y 
becaue want 

xy
00 -> 10 -> 01 -> 11

(note: interleave 1 0 = 1, interleave 0 1 = 2) 

-}
interleave16Bits :: Int -> Int -> Int
interleave16Bits !x !y  = word2int $! xshifted .|. yshifted
    
    where   xshifted :: Word = ((int2word x * 0x0101010101010101 .&. 0x8040201008040201) * 
                0x0102040810204081 >> 49) .&. 0x5555 
            yshifted :: Word = ((int2word y * 0x0101010101010101 .&. 0x8040201008040201) * 
                0x0102040810204081 >> 48) .&. 0xAAAA
{-# INLINE interleave16Bits #-}

{-
note: 32bit Ints give you ~ 4gb of addressable locations PER matrix/array
lets just use the tricks for that size for now, though it might 
-}

{-as writ, only shuffles the lower 32 bits, the rest are zerod out
has good instruction level parallelism  (17 cycles in ideal), but 42 in sequential risc model -}
outerShuffle32A :: Word -> Word
outerShuffle32A !x =
  case (x .&. 0x0000ff00 ) <<  8 .|. (x >> 8) .&. 0x0000FF00 .|. x  .&. 0xFF0000FF of 
    x -> case ( x .&. 0x00F000F0 ) << 4 .|. (x >> 4) .&. 0x00F000F0 .|. x .&. 0xF00FF00F  of 
      x->case (x .&.  0x0C0C0C0C )<< 2 .|.  (x >> 2) .&. 0x0C0C0C0C .|. x .&. 0xC3C3C3C3 of  
        x-> case ( (x .&. 0x22222222)  << 1  .|. (x>> 1) .&. 0x22222222 .|. x .&. 0x99999999) of 
            res -> res
{-# INLINE outerShuffle32A #-}

{-
hacker's delight shuffle code listed 
at http://www.hackersdelight.org/hdcodetxt/shuffle.c.txt
-}



{- as writ, only 32 bit,
30 cycles on sequential risc machine,
21 cycles on unlimited instruction level parallelism machine


Both versions are pretty fast, ~20ns, with some swappage 
in terms of -fllvm -03, -threaded (or not),
and what optimizations flags are pased to  llvm 

-}
{-# INLINE outerShuffle32B #-}
outerShuffle32B :: Word->Word
outerShuffle32B !x =
  case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF00)   of 
    x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F0) of 
      x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C) of
        x -> case xor2LShift 1 x (xorRShift 1 x .&. 0x22222222) of 
            res -> res 

{-make sure the shift amount is < WORD_SIZE-}
{-# INLINE xor2LShift #-}
xor2LShift :: Int-> Word -> Word ->Word 
xor2LShift sft x t  =  x `xor` t `xor` (t << sft)

{-# INLINE xorRShift #-}
xorRShift :: Int -> Word  ->Word
xorRShift sft x   = x `xor` (x >> sft )


--{-# INLINE orMaskShifts #-}
--orMaskShifts :: Int -> Word  -> Word