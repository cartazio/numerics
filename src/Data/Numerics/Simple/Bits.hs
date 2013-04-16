{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Numerics.Simple.Bits
    -- this module is changing enough that its not worth
    -- being explicit about imports 
    -- (outerShuffle64A
    --                        ,outerUnShuffle64B
    --                        ,outerShuffle64B,outerUnShuffle64A,idShuffleA) 
    where

import Data.Bits
import Data.Word
import Prelude hiding ((>>)) 
import Data.List (foldl')
import Data.Typeable
import Data.Data 


import Data.Numerics.Simple.Util





infixl 8  << , >>
(<<):: Bits a=> a ->Int -> a
(<<) = unsafeShiftL
{-# INLINE (<<) #-}

(>>)::  Bits a=> a ->Int -> a
(>>) = unsafeShiftR
{-# INLINE (>>) #-}






uncheckedQuotRemPow2  :: Int -> Int -> TupInt
uncheckedQuotRemPow2  a k  =  TI (a >> k) ( a .&. ((1 << k) -1  ) ) 

quotRemPow2 a k | k <= (bitSize (undefined :: Int) -1 ) = uncheckedQuotRemPow2 a k 
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



--as writ, only shuffles the lower 32 bits, the rest are zerod out
---has good instruction level parallelism  (17 cycles in ideal), but 42 in sequential risc model
---- also seems that my 64 bit version is wrong

{- previously  -}
outerShuffle32A :: Word -> Word
outerShuffle32A !x =
  case (x .&. 0x0000FF00 ) <<  8 .|. (x >> 8) .&. 0x0000FF00 .|. x  .&. 0xFF0000FF of 
    x -> case ( x .&. 0x00F000F0 ) << 4 .|. (x >> 4) .&. 0x00F000F0 .|. x .&. 0xF00FF00F  of 
      x->case (x .&.  0x0C0C0C0C )<< 2 .|.  (x >> 2) .&. 0x0C0C0C0C .|. x .&. 0xC3C3C3C3 of  
        x-> case ( (x .&. 0x22222222)  << 1  .|. (x>> 1) .&. 0x22222222 .|. x .&. 0x99999999) of 
            res -> res



outerShuffle64A :: Word -> Word
outerShuffle64A !x =
--- the 16 shift should be conditional
    case      ((x .&. 0x00000000FFFF0000) << 16 )
     .|. ((x>>16) .&. 0x00000000FFFF0000) .|. (x .&. 0xFFFF00000000FFFF) of
      
      x->  case ((x .&. 0x0000FF000000FF00 ) <<  8 )
       .|. (x >> 8) .&. 0x0000FF000000FF00 .|. (x  .&. 0xFF0000FFFF0000FF) of 

        x -> case (( x .&. 0x00F000F000F000F0 ) << 4 )
          .|. (x >> 4) .&. 0x00F000F000F000F0 .|. (x .&. 0xF00FF00FF00FF00F ) of 

          x->case   ((x .&.  0x0C0C0C0C0C0C0C0C )<< 2 )
            .|. (x >> 2) .&. 0x0C0C0C0C0C0C0C0C .|.( x .&. 0xC3C3C3C3C3C3C3C3) of  

            x-> case   ( (x .&. 0x2222222222222222)  << 1 ) 
                .|. (x>> 1) .&. 0x2222222222222222 .|. (x .&. 0x9999999999999999) of 
                    !res -> res
{-# INLINE outerShuffle64A #-}


-- Outer Unshuffle 64A have 

outerUnShuffle64A :: Word -> Word
outerUnShuffle64A !x= 
    case (x >> 1) .&. 0x2222222222222222 .|. 
            ( x .&. 0x2222222222222222) << 1 .|. (x .&. 0x9999999999999999) of
      x-> case   ((x .&.  0x0C0C0C0C0C0C0C0C )<< 2 )
        .|. (x >> 2) .&. 0x0C0C0C0C0C0C0C0C .|.( x .&. 0xC3C3C3C3C3C3C3C3) of
        x-> case (( x .&. 0x00F000F000F000F0 ) << 4 )
          .|. (x >> 4) .&. 0x00F000F000F000F0 .|. (x .&. 0xF00FF00FF00FF00F ) of 
          x-> case ((x .&. 0x0000FF000000FF00 ) <<  8 )
           .|. (x >> 8) .&. 0x0000FF000000FF00 .|. (x  .&. 0xFF0000FFFF0000FF) of 
            x->  case      ((x .&. 0x00000000FFFF0000) << 16 )
             .|. ((x>>16) .&. 0x00000000FFFF0000) .|. (x .&. 0xFFFF00000000FFFF) of
                !res-> res 
{-# INLINE outerUnShuffle64A #-}

---- LLVM's optimizor 'proved' that this is the identity function for me :) 
idShuffleA  x= outerUnShuffle64A $ outerShuffle64A x


{-
micro Benchmarks indicate outerShuffle64A  (mean 8.794 μs) is about 10-30 percent
faster than outerShuffle64B  ( mean 11.85 μs )

-}




hilbIx2XYa :: Int -> Word ->  TupWord
hilbIx2XYa order  = 
    \ix -> 
        let
            go :: (Word,Word,Word)-> Int -> (Word,Word,Word)
            go (!x, !y, !state  ) !stepI =   ( x <<1 .|. (0x936c >> row ) .&. 1 
                        ,y <<1 .|. (0X39C6  >> row ) .&. 1 
                         , (  0x3E6B94C1 >> (2*row))  .&. 3   )
                    where !row = word2int $! 4*state .|. (ix >> stepI) .&. 3

            in
             case foldl' go (0,0,0) [2*order -2, -2 .. 0] of
                            (!x,!y,_) -> TW x y    
{-# INLINABLE  hilbIx2XYa #-}                    

{-  
i'm writing hilbS2XYa this way because I want to make sure
that can get specialized code when i know the rank/"order"
of the hilbert curve. THis is kinda a doubly nested worker wrapper transform
(not quite, but in spirit :), should also compare with the direct version too)

-}


{- Lam \& Shapiro algorithm for  taking the hilbert index -}

hilbIx2XYbLS :: Int ->Word ->TupWord
hilbIx2XYbLS order = 
    \ix -> let 
                go :: (Word,Word)-> Int -> (Word,Word )
                go (!x,!y) !stepI  | sa `xor` sb == 0 = 
                                        case prepender (y `xor` (complement sa)) (x `xor` (complement sa)) of
                                                res@(!x,!y)-> res 

                                  | otherwise =  
                                        case   prepender x y   of 
                                                    !res -> res 
                                     
                    where   
                            --- note sa and sb are only nonzero (possibly nonzero)
                            ---- in the 1 bit, always zero everywhere else 
                            !sa  = (ix >> (stepI +1)) .&. 1
                            !sb  = (ix >> stepI) .&. 1 
                            --- should prepender just capture sa and sb, or pass them?
                            --- should bench both at some point
                            prepender :: Word -> Word -> (Word , Word )
                            prepender !x !y   =  
                                    case (x >> 1 .|.  sa << (bitSize x -1) ,  
                                          y >> 1 .|. (sa `xor` sb) << (bitSize x -1)) of 
                                        res@(!x,!y) -> res 
            in 
                case foldl' go (0,0) [0,2 .. 2 * order - 1 ] of 
                    (x,y) -> TW (x >> normalize) (y>> normalize )
                                where 
                                    !normalize =(bitSize x  - order) 



--- branchless lam shapiro 
hilbIx2XYbLSBranchless :: Int ->Word ->TupWord
hilbIx2XYbLSBranchless order ix = 
     case foldl' go (0,0) [0,2 .. 2 * order - 1 ] of 
                    (!x,!y) -> TW x y 
        where 
                go :: (Word,Word)-> Int -> (Word,Word )
                go (!x,!y) !stepI   =   case   prepender xFixed yFixed   of 
                                                    !res -> res 
                                     
                    where   
                            --- note sa and sb are only nonzero (possibly nonzero)
                            ---- in the 1 bit, always zero everywhere else 
                            !bitSwap = (sa `xor` sb) -1 
                            !bitCompl = - (sa .&. sb)
                            !xp = x `xor` y
                            !xFixed = xp `xor` yFixed
                            !yFixed = y `xor` (x .&. bitSwap ) `xor` bitCompl 
                            !sa  = (ix >> (stepI +1)) .&. 1
                            !sb  = (ix >> stepI) .&. 1 
                            --- should prepender just capture sa and sb, or pass them?
                            --- should bench both at some point
                            prepender :: Word -> Word -> (Word , Word )
                            prepender !x !y   =  
                                    case (x >> 1 .|.  sa << (bitSize x -1) ,  
                                          y >> 1 .|. (sa `xor` sb) << (bitSize x -1)) of 
                                        res@(!x,!y) -> res 

               



--- I really want to figure out the inverse of this version
--- becaue then i'd have loop free branchless xy -> hilbert ix 
--- based upon parallel prefix 32 version in hackers delight hilbert ix -> x y 
--- algorithm
hilbIx2XYParPrefix :: Int ->Word ->TupWord
hilbIx2XYParPrefix order  ix = 
    case ix .|. (0x5555555555555555 << (2*order) ) of 
     !ix -> case (ix >> 1) .&. 0x5555555555555555 of 
      !ixr -> case ((ix .&. 0x5555555555555555 ) + ixr ) `xor` 0x5555555555555555 of
       !cix-> case cix `xor` (cix >> 2) of
        !cix -> case cix `xor` (cix >> 4) of 
         !cix -> case cix `xor` (cix >> 8) of 
          !cix -> case cix `xor` (cix >> 16) of
           !cix -> case cix `xor` (cix >> 32) of  -- this is the only mod aside from 0x5555555555555555 for 64 bit!
            !cix-> case cix .&. 0x5555555555555555 of 
             !swap -> case (cix >> 1) .&. 0x5555555555555555 of
              !comp -> case  (ix .&.  swap) `xor` comp  of
               !t -> case ix `xor` ixr `xor` t `xor` (t << 1) of
                 !ix -> case ix .&. ((1<< (2*order))-1 ) of 
                  !ix -> case  outerUnShuffle64A ix of
                    !res-> TW (res >> (bitSize res `quot` 2)) (res .&. 0xFFFFFFFF) 
{-# INLINABLE hilbIx2XYParPrefix #-}

---------------
---- loopy x y -> hilbert index
-----------

hilbXY2Ix ::Int ->TupWord -> Word 
hilbXY2Ix order (TW !x !y) = 
    case foldl' go  (0,0) [order -1, -1, 0] of
        (!ix,!state)-> ix 
        where 
            go (!ix,!state) stepI = case




----------------
----- the *B outer shuffles should not be used, except to benchmark on 
------ new architectures to validate default choices have good relative perf
-----------------


outerShuffle64B :: Word -> Word 
outerShuffle64B !x =
    case xor2LShift 16 x (xorRShift 16 x  .&.  0x00000000FFFF0000) of 
        {- why am I doing  0xFFFFFFFF0000FFFF here? tired logic  -}
      x -> case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF000000FF00)   of 
        x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F000F000F0) of 
          x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C0C0C0C0C) of
            x -> case xor2LShift 1 x (xorRShift 1 x .&. 0x2222222222222222) of 
                res -> res 
{-# INLINE outerShuffle64B #-}

outerShuffle32B :: Word -> Word 
outerShuffle32B !x =  case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF00)   of 
        x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F0) of 
          x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C) of
            x -> case xor2LShift 1 x (xorRShift 1 x .&. 0x222222222) of 
                !res -> res 

--outerUnShuffle64 :: (Num a, Bits a) => a -> a
outerUnShuffle64B:: Word -> Word
outerUnShuffle64B !x =
    case xor2LShift 1 x (xorRShift 1 x .&. 0x2222222222222222) of 
      x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C0C0C0C0C) of
        x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F000F000F0) of 
          x -> case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF000000FF00)  of 
            x->  case xor2LShift 16 x (xorRShift 16 x  .&.  0x00000000FFFF0000) of 
                {- why am I doing  0xFFFFFFFF0000FFFF-}
                    !res -> res 
{-# INLINE outerUnShuffle64B #-}

outerUnShuffle32B:: Word -> Word
outerUnShuffle32B !x =
    case xor2LShift 1 x (xorRShift 1 x .&. 0x222222222) of 
      x->  case xor2LShift 2 x (xorRShift 2 x .&. 0x0C0C0C0C) of
        x-> case xor2LShift 4 x (xorRShift 4 x .&. 0x00F000F0) of 
          x -> case xor2LShift  8 x (xorRShift 8 x    .&. 0x0000FF00) of
                !res -> res 



--{-# SPECIALIZE outerUnShuffle64 :: Word->Word #-}
--{-# SPECIALIZE outerUnShuffle64 :: Int->Int #-}

{-make sure the shift amount is < WORD_SIZE-}
{-# INLINE xor2LShift #-}
--xor2LShift :: Bits a => Int -> a -> a -> a 
xor2LShift:: Int -> Word -> Word -> Word 
xor2LShift sft x t  =  x `xor` t `xor` (t << sft)

{-# INLINE xorRShift #-}
--xorRShift :: Bits a => Int -> a -> a
xorRShift :: Int -> Word -> Word 
xorRShift sft x   = x `xor` (x >> sft )



{-
hacker's delight shuffle code listed 
at http://www.hackersdelight.org/hdcodetxt/shuffle.c.txt
-}

--flipsidesA :: Word -> Word 
flipSidesA x =   (x >> (bitSize x `div` 2  ) ) .|. (x << (bitSize x `div` 2  ))
{-# INLINE flipSidesA #-}


flipSidesB x = x `rotate` (bitSize x `div` 2 )  
{-#  INLINE flipSidesB #-}

{- 
for the  32 bit version,
30 cycles on sequential risc machine,
21 cycles on unlimited instruction level parallelism machine


Both versions are pretty fast, ~20ns, with some swappage 
in terms of -fllvm -03, -threaded (or not),
and what optimizations flags are pased to  llvm 

-}

--- 64bit only for now
{-
NOTE: this is actually subtly wrong, 
-}



tup2Outer :: TupInt -> Word 
tup2Outer (TI x y) =  case  ( (xw .&. 0xFFFFFFFF) << 32 ) .|. (yw .&. 0xFFFFFFFF) of 
                    !res -> res
            where !xw = int2word x 
                  !yw = int2word y 
{-# INLINE tup2Outer #-}                  

  
outer2Tup :: Word -> TupInt
outer2Tup !w =   case TI (word2int $! ( w >> 32 ) .&. 0xFFFFFFFF )  (word2int $! (w .&. 0xFFFFFFFF) ) of 
                    !res -> res
{-# INLINE outer2Tup #-}


