{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}

module Numerics.Simple.KernelPOC where

import Prelude hiding ((>>))

--import Data.Primitive
import Numerics.Simple.Bits
import Control.Monad.Primitive ()

import Foreign.Ptr
import Foreign()
import Foreign.C.Types

import qualified Data.Vector.Storable.Mutable as SM 


import qualified  Data.Vector.Unboxed.Mutable as UM 


data Quad a = QD {q1:: !a, q2::  !a,q3:: !a,q4:: !a}
{-
layout is interpreted to be

q1, q2
q3, q4
 
-}


newtype MortonZ a = MZ a

-- well its upside down N, but whatever
newtype MortonN a = MN a

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
wrapFun :: (Ptr CDouble -> Ptr CDouble -> Ptr CDouble-> CInt -> IO ()) ->SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> Int -> IO ()
wrapFun fun am bm cm size=
   SM.unsafeWith am $! \a -> 
        SM.unsafeWith bm $! \b ->
            SM.unsafeWith cm  $! \c ->
                     do  fun  (castPtr a )(castPtr b )(castPtr c)  (fromIntegral size )
                         return ()

foreign import ccall unsafe "simplemat.c fatDotProduct" 
    c_fatDotProduct :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble-> CInt -> IO ()

fatDotProduct ::  SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> Int -> IO ()
fatDotProduct am bm cm size = wrapFun c_fatDotProduct am bm cm size


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
basicKernel  !aix !bix !cix !aMat !bMat !cMat =    
                            quadDirectSimpleWithShiftC aix bix cix aMat bMat cMat
                   




{-# INLINE simpleLooper #-}
simpleLooper :: IOVectDouble -> IOVectDouble -> IOVectDouble-> Int  -> IO ()
simpleLooper !rMat !aMat !bMat !n = go 0 0 0  0  --- we're about to run step 0!!
    where 
        !blockedN = n `div` 4 --- 4x4
        !blockCubed = blockedN * blockedN * blockedN
        go !x !y !z   !count |  (count < (blockCubed -1)) =   --- this seems wrong, but whatever
                                          --- if we hit the bounds all at once, we win!
                         do   
                            
                            next x y z 
                            go (modN $! x+4 ) (modN $! y+2) (modN $! z+8) (count + 8)
                    | otherwise =  
                            do 
                                appKernel64 basicKernel x y z  rMat  aMat bMat 
                                -- its actually correct to do the basic kernel here
                                return ()
        
        theKernel !x !y !z !ashift !bshift !cshift=  do  return ()
               --(appKernel64 ( appKernel64 prefetchKernel x y z ) (x + ashift) (y +bshift) (z + cshift ) )rMat  aMat bMat                             

        modN !j = mod j blockedN 
        next !x !y !z  =
                    do  -- prefetch the block after next
                        theKernel x y z             1 0 2 -- 1
                        theKernel x y (z + 1)       1 0 3-- 2 
                        theKernel (x+1) y (z+2)     2 1 4 -- 3
                        theKernel (x+1) y (z+3)     2 1 5 -- 4
                        theKernel (x+2) (y+1) (z+4) 3 1 6-- 5
                        theKernel (x+2) (y+1) (z+5) 3 1 7-- 6
                        theKernel (x+3) (y+1) (z+6) 4 2 8-- 7
                        theKernel (x+3) (y+1) (z+7) 4 2 9  -- 8       

--{-# INLINABLE nativeTwoByTwoMMultKernelFunc #-}
--nativeTwoByTwoMMultKernelFunc :: IOVectDouble ->IOVectDouble -> IO (Quad Double) 
--nativeTwoByTwoMMultKernelFunc leftMat rightMat =
--            do 
--                leftMatQ1 <- SM.unsafeIndex  leftMat 0
--                rightMatQ1 <- SM.unsafeIndex rightMat 0
--                leftMatQ2 <- SM.unsafeIndex leftMat 1 
--                rightMatQ2 <- S

{-# INLINE project2x2matrixMortonZ #-}  --- mortonZ in the 2x2 case is also row major
project2x2matrixMortonZ :: IOVectDouble -> IO (MortonZ (Quad Double))
project2x2matrixMortonZ mat =
        do 
            mq1 <- SM.unsafeRead mat 0
            mq2 <- SM.unsafeRead mat 1
            mq3 <- SM.unsafeRead mat 2 
            mq4 <- SM.unsafeRead mat 3
            return (MZ (QD mq1 mq2 mq3 mq4))

{-# INLINE project2x2matrixMortonFlipN #-}  --- mortonZ in the 2x2 case is also row major
project2x2matrixMortonFlipN :: IOVectDouble -> IO (MortonN (Quad Double))
project2x2matrixMortonFlipN mat =
        do 
            mq1 <- SM.unsafeRead mat 0
            mq2 <- SM.unsafeRead mat 2
            mq3 <- SM.unsafeRead mat 1             
            mq4 <- SM.unsafeRead mat 3
            return (MN (QD mq1 mq2 mq3 mq4))            
 
{-# INLINE project2x2MatrixMultMzMnMz #-}
project2x2MatrixMultMzMnMz :: (MortonZ (Quad Double)) -> (MortonN (Quad Double) ) -> MortonZ (Quad Double)
project2x2MatrixMultMzMnMz (MZ leftQMZ) (MN rightQMN) =
    MZ (QD (q1 leftQMZ * q1 rightQMN + q2 leftQMZ * q3 rightQMN ) 
            (q1 leftQMZ * q2 rightQMN + q2 leftQMZ * q4 rightQMN ) 
            (q3 leftQMZ * q1 rightQMN + q4 leftQMZ * q3 rightQMN )
            (q3 leftQMZ * q2 rightQMN + q4 leftQMZ * q4 rightQMN )  )

{-# INLINE  merge2x2QuadsMortonZ #-}
merge2x2QuadsMortonZ :: MortonZ(Quad Double) ->MortonZ (Quad Double) -> MortonZ (Quad Double)
merge2x2QuadsMortonZ (MZ !left) (MZ !right) =
  MZ $! QD (q1 left + q1 right) (q2 left + q2 right )
            (q3 left + q3 right )  (q4 left + q4 right)

write2x2QuadMZ :: IOVectDouble -> MortonZ (Quad Double) -> IO ()
write2x2QuadMZ = undefined

-- this way i can apply prepkernel64 multiple times! 
-- wh
{-# INLINE appKernel64  #-}
appKernel64::  (Int -> Int -> Int ->  b)  -> Int  -> Int -> Int -> b 
appKernel64 !kerf   = go 
    where 
        go !x !y !z  = kerf x y z  --- kerf ( mortonZ64 x y ) (mortonZ64 z y) (mortonFlipN64 x z )

type IOVectDouble = SM.IOVector Double 



{-# INLINE unsafeDiceMZ #-}
unsafeDiceMZ
  :: MortonZ (SM.IOVector Double) -> Quad (MortonZ (SM.IOVector Double))
unsafeDiceMZ  (MZ v) = 
        QD  (MZ $! SM.unsafeSlice q1Base len v )
            (MZ $! SM.unsafeSlice q2Base len v )
            (MZ $! SM.unsafeSlice q3Base len v )
            (MZ $! SM.unsafeSlice q4Base len  v )
    where
        !len = (parentLength ) >> 2 ---  divide by 4, should do with shifts
        !parentLength = (SM.length v )
        !q1Base = 0
        !q2Base = len 
        !q3Base = len  << 1     --- 2* len 
        !q4Base = parentLength - len   --- (4*len - len )
        

{-# INLINE unsafeDiceMFlipN #-}
unsafeDiceMFlipN
  ::   MortonN (SM.IOVector Double) -> Quad (MortonN (SM.IOVector Double))
unsafeDiceMFlipN (MN v) =  
        QD  (MN $! SM.unsafeSlice q1Base len v )
            (MN $! SM.unsafeSlice q2Base len  v )
            (MN $! SM.unsafeSlice q3Base len  v )
            (MN $! SM.unsafeSlice q4Base len  v )
    where
        !len = (parentLength ) >>2 ---  divide by 4
        !parentLength = (SM.length v )
        !q1Base = 0
        !q2Base = len << 1   --- MN same as MZ but with the base index for q2 and q3 swapped
        !q3Base = len    
        !q4Base = parentLength - len    --- (4*len - len )
        

-- i'm fed 3 2x2 matrices, i'll work on reading / writing them directly

--- INLINE , hopefully that + llvm bb vectorization work well
{-# INLINE unsafeDirect2x2MzMnMzStorable #-}
unsafeDirect2x2MzMnMzStorable ::  MortonZ (SM.IOVector Double)-> 
        MortonZ (SM.IOVector Double)->MortonN (SM.IOVector Double)-> IO ()
unsafeDirect2x2MzMnMzStorable (MZ resMat) (MZ leftReadMat) (MN rightReadMat) = 
        do 
            l0  <- SM.unsafeRead  leftReadMat  0
            l1 <- SM.unsafeRead  leftReadMat 1
            r0 <- SM.unsafeRead rightReadMat 0
            r1 <- SM.unsafeRead rightReadMat 1

            res0 <- SM.unsafeRead resMat 0
            SM.unsafeWrite resMat 0 $! (l0 * r0 + l1 * r1 + res0)

            r2 <- SM.unsafeRead rightReadMat 2 
            r3 <- SM.unsafeRead rightReadMat 3

            res1 <- SM.unsafeRead resMat 1

            SM.unsafeWrite resMat 1 $! (l0 * r2 + l1 * r3 + res1)            


            l2 <-SM.unsafeRead leftReadMat 2
            l3 <- SM.unsafeRead leftReadMat 3

            res2 <- SM.unsafeRead resMat 2
            res3 <- SM.unsafeRead resMat 3

            SM.unsafeWrite resMat 2 $! (l2 * r0 + l3 * r1 + res2)
            SM.unsafeWrite resMat 3 $! (l2 * r2 + l3 * r3 + res3)

            -- these are all register friendly right?
            return ()

unsafeDirect2x2MzMnMzUnbox ::  MortonZ (UM.IOVector Double)-> 
        MortonZ (UM.IOVector Double)->MortonN (UM.IOVector Double)-> IO ()
unsafeDirect2x2MzMnMzUnbox (MZ resMat) (MZ leftReadMat) (MN rightReadMat) = 
        do 
            l0  <- UM.unsafeRead  leftReadMat  0
            l1 <- UM.unsafeRead  leftReadMat 1
            r0 <- UM.unsafeRead rightReadMat 0
            r1 <- UM.unsafeRead rightReadMat 1

            res0 <- UM.unsafeRead resMat 0
            UM.unsafeWrite resMat 0 $! (l0 * r0 + l1 * r1 + res0)

            r2 <- UM.unsafeRead rightReadMat 2 
            r3 <- UM.unsafeRead rightReadMat 3

            res1 <- UM.unsafeRead resMat 1

            UM.unsafeWrite resMat 1 $! (l0 * r2 + l1 * r3 + res1)

            l2 <-UM.unsafeRead leftReadMat 2
            l3 <- UM.unsafeRead leftReadMat 3

            res2 <- UM.unsafeRead resMat 2
            res3 <- UM.unsafeRead resMat 3

            UM.unsafeWrite resMat 2 $! (l2 * r0 + l3 * r1 + res2)
            UM.unsafeWrite resMat 3 $! (l2 * r2 + l3 * r3 + res3)

            -- these are all register friendly right?
            -- need to try this unboxed version out shortly
            return ()

