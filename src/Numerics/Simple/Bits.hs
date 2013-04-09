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

data TupInt = TI {-#UNPACK#-} !Int {-#UNPACK#-} !Int
--- | quotRemStrong taks an int, the exponent of a number of the form 
---  
unsafeQuotRemPow2  :: Int -> Int -> TupInt
unsafeQuotRemPow2  a k  =  TI (a >> k) ( a .&. ((1 << k) -1  ) ) 

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
so not sure if its worth it
-}

{-

--{-as writ, only shuffles the lower 32 bits, the rest are zerod out
---has good instruction level parallelism  (17 cycles in ideal), but 42 in sequential risc model
---- also seems that my 64 bit version is wrong

--- -}
outerShuffle32A :: Word -> Word
outerShuffle32A !x =
  case (x .&. 0x0000ff00 ) <<  8 .|. (x >> 8) .&. 0x0000FF00 .|. x  .&. 0xFF0000FF of 
    x -> case ( x .&. 0x00F000F0 ) << 4 .|. (x >> 4) .&. 0x00F000F0 .|. x .&. 0xF00FF00F  of 
      x->case (x .&.  0x0C0C0C0C )<< 2 .|.  (x >> 2) .&. 0x0C0C0C0C .|. x .&. 0xC3C3C3C3 of  
        x-> case ( (x .&. 0x22222222)  << 1  .|. (x>> 1) .&. 0x22222222 .|. x .&. 0x99999999) of 
            res -> res



-}

--mortonOuterShuffle 

{- 64 bit generalization of 32bit outer shuffly from hackers delight
should add conditional logic so that the step for 64 bit words is dropp
 -}
{-
---- this code is wrong, not sure where the mistake is
outerShuffle64A :: Word -> Word
outerShuffle64A !x =
--- the 16 shift should be conditional
    case (x .&. 0x00000000FFFF0000) << 16 
        .|. (x >> 16) .&. 0x00000000FFFF0000 .|. x .&. 0xFFFF00000000FFFF of

      x->case (x .&. 0x0000FF000000FF00 ) <<  8 
            .|. (x >> 8) .&. 0x0000FF000000FF00 .|. x  .&. 0xFF0000FFFF0000FF of 

        x -> case ( x .&. 0x00F000F000F000F0 ) << 4 
            .|. (x >> 4) .&. 0x00F000F000F000F0 .|. x .&. 0xF00FF00FF00FF00F  of 

          x->case (x .&.  0x0C0C0C0C0C0C0C0C )<< 2 
                .|.   (x >> 2) .&. 0x0C0C0C0C0C0C0C0C .|. x .&. 0xC3C3C3C3C3C3C3C3 of  

            x-> case ( (x .&. 0x2222222222222222)  << 1  
                .|. (x>> 1) .&. 0x2222222222222222 .|. x .&. 0x9999999999999999) of 
                    res -> res
{-# INLINE outerShuffle64A #-}

-}



{-

outerUnShuffl code

   t = (x ^ (x >> 1)) & 0x22222222;  x = x ^ t ^ (t << 1);
   t = (x ^ (x >> 2)) & 0x0C0C0C0C;  x = x ^ t ^ (t << 2);
   t = (x ^ (x >> 4)) & 0x00F000F0;  x = x ^ t ^ (t << 4);
   t = (x ^ (x >> 8)) & 0x0000FF00;  x = x ^ t ^ (t << 8);

-}

{-
hacker's delight shuffle code listed 
at http://www.hackersdelight.org/hdcodetxt/shuffle.c.txt
-}

--flipsidesA :: Word -> Word 
flipSidesA x =   (x >> (bitSize x `div` 2  ) ) .|. (x << (bitSize x `div` 2  ))
{-# INLINE flipSidesA #-}


flipSidesB x = x `rotate` (bitSize x `div` 2 )  

{- 
for the  32 bit version,
30 cycles on sequential risc machine,
21 cycles on unlimited instruction level parallelism machine


Both versions are pretty fast, ~20ns, with some swappage 
in terms of -fllvm -03, -threaded (or not),
and what optimizations flags are pased to  llvm 

-}

--outerShuffle64B :: Word->Word
outerShuffle64B :: (Num a, Bits a) => a -> a
outerShuffle64B !x =
    case xor2LShift 16 x (xorRShift 16 x  .&.  0xFFFFFFFF0000FFFF) of 
      x -> case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF000000FF00)   of 
        x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F000F000F0) of 
          x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C0C0C0C0C) of
            x -> case xor2LShift 1 x (xorRShift 1 x .&. 0x2222222222222222) of 
                res -> res 
{-# INLINABLE outerShuffle64B #-}
--outerShuffle64B :: Word->Word
{-# SPECIALIZE outerShuffle64B :: Word->Word #-}
{-# SPECIALIZE outerShuffle64B :: Int->Int #-}


outerUnShuffle64 :: (Num a, Bits a) => a -> a
outerUnShuffle64 !x =
    case xor2LShift 1 x (xorRShift 1 x .&. 0x2222222222222222) of 
      x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C0C0C0C0C) of
        x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F000F000F0) of 
          x -> case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF000000FF00)  of 
            x->  case xor2LShift 16 x (xorRShift 16 x  .&.  0xFFFFFFFF0000FFFF) of 
                    !res -> res 
{-# INLINABLE outerUnShuffle64 #-}
{-# SPECIALIZE outerUnShuffle64 :: Word->Word #-}
{-# SPECIALIZE outerUnShuffle64 :: Int->Int #-}

{-make sure the shift amount is < WORD_SIZE-}
{-# INLINE xor2LShift #-}
xor2LShift :: Bits a => Int -> a -> a -> a 
xor2LShift sft x t  =  x `xor` t `xor` (t << sft)

{-# INLINE xorRShift #-}
xorRShift :: Bits a => Int -> a -> a
xorRShift sft x   = x `xor` (x >> sft )

