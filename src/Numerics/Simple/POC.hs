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
import qualified  Data.Vector.Generic.Mutable as GM 
import qualified  Data.Vector.Unboxed.Mutable as UM 

import Control.Monad.Par.IO
import Control.Monad.Par.Class
import Control.Monad.IO.Class
import Numerics.Simple.Util

import Foreign.Ptr
import Foreign
import Foreign.C.Types

import Data.Foldable

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

{-
here we assume resMat and LeftMat are row maj and rightMat is column major
-}
dotMMultStorable :: SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> Int -> IO ()
dotMMultStorable !resMat !leftMat !rightMat !n = 
            forM_ [0 .. n-1 ] (\row -> 
                forM_ [0 ..n-1 ]( \col -> 
                          let 
                                !currentRowBase = n *row 
                                !currentColBase = n*col
                                fma !partialSum ix  = 
                                        do 
                                            leftOperand <- SM.read leftMat $! ( currentRowBase+ ix )
                                            rightOperand <- SM.read rightMat $! (currentColBase  + ix)
                                            return $! (partialSum + leftOperand * rightOperand)
                                        --- this is not the most numerical stable dot product, but whatever
                            in 
                                do 
                                    dotprod <- foldlM fma 0 [0 .. n -1 ]
                                    SM.write resMat (currentRowBase + col )  dotprod
                                    return ()
                    ))

{-# INLINE unsafeDiceMZ #-}
unsafeDiceMZ
  :: GM.MVector v a => MortonZ (v s a) -> Quad (MortonZ (v s a))
unsafeDiceMZ  (MZ v) = 
        QD  (MZ $! GM.unsafeSlice q1Base len v )
            (MZ $! GM.unsafeSlice q2Base len v )
            (MZ $! GM.unsafeSlice q3Base len v )
            (MZ $! GM.unsafeSlice q4Base len  v )
    where
        !len = (parentLength ) `div` 4 ---  divide by 4, should do with shifts
        !parentLength = (GM.length v )
        !q1Base = 0
        !q2Base = len 
        !q3Base = len  << 1     --- 2* len 
        !q4Base = parentLength - len   --- (4*len - len )
        

{-# INLINE unsafeDiceMFlipN #-}
unsafeDiceMFlipN
  :: GM.MVector v a => MortonN (v s a) -> Quad (MortonN (v s a))
unsafeDiceMFlipN (MN v) =  
        QD  (MN $!  GM.unsafeSlice q1Base len v )
            (MN $! GM.unsafeSlice q2Base len  v )
            (MN $! GM.unsafeSlice q3Base len  v )
            (MN $! GM.unsafeSlice q4Base len  v )
    where
        !len = (parentLength ) >>2 ---  divide by 4
        !parentLength = (GM.length v )
        !q1Base = 0
        !q2Base = len << 1   --- MN same as MZ but with the base index for q2 and q3 swapped
        !q3Base = len    
        !q4Base = parentLength - len    --- (4*len - len )
        


foreign import ccall unsafe "simplemat.c SimpleMatMult4x4" 
    c_SimpleMatMult4x4 :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

quadDirectSimpleC :: SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO ()
quadDirectSimpleC !res !leftM !rightM = 
    SM.unsafeWith res $! \a -> 
        SM.unsafeWith leftM $! \b ->
            SM.unsafeWith rightM $! \c ->  c_SimpleMatMult4x4  (castPtr a)  (castPtr b) (castPtr c)



--- 
dgemmBlockWrapped :: SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO () 
dgemmBlockWrapped a b c = runParIO $!  degmmBlockStorableRecurTOP (MZ a) (MZ b) (MN c)

degmmBlockStorableRecurTOP   res@(MZ resArr)  readL  readR  =
        do 
            unsafeBlockDiceRecurStorableTop  (unsafeDiceMZ res) 
                                        (unsafeDiceMZ readL) (unsafeDiceMFlipN readR)




degmmBlockStorableRecur   res@(MZ resArr)  readL  readR  -- | SM.length `sArr < 16 =  error  $! ("bad params"++ show (SM.length resArr))
                            | SM.length resArr == 16 =  
                                --quadDirectSimpleC (unMZ res) (unMZ readL) (unMN readR)
                                unsafeQuadDirectMzMn2MzMMultStorable (unsafeDiceMZ res) 
                                        (unsafeDiceMZ readL) (unsafeDiceMFlipN readR)
                         | otherwise = 
                            do 
                                unsafeBlockDiceRecurStorable (unsafeDiceMZ res) 
                                        (unsafeDiceMZ readL) (unsafeDiceMFlipN readR)


unsafeBlockDiceRecurStorableTop resQuad readLQuad readRQuad =
        do 
            a<- spawn $! liftIO ( 
                do  degmmBlockStorableRecur(q1 resQuad) (q1 readLQuad) (q1 readRQuad)
                    degmmBlockStorableRecur (q1 resQuad) (q2 readLQuad) (q2 readRQuad) )

            b <- spawn $! liftIO (
                do degmmBlockStorableRecur (q2 resQuad) (q1 readLQuad) (q3 readRQuad)
                   degmmBlockStorableRecur  (q2 resQuad) (q2 readLQuad) (q4 readRQuad))
            get a

            c <- spawn $! liftIO (
                do degmmBlockStorableRecur  (q3 resQuad) (q3 readLQuad) (q1 readRQuad)
                   degmmBlockStorableRecur  (q3 resQuad) (q4 readLQuad) (q2 readRQuad))
            get b
            d <- spawn $!  liftIO (
                do  (degmmBlockStorableRecur (q4 resQuad) (q3 readLQuad) (q3 readRQuad))
                    degmmBlockStorableRecur (q4 resQuad) (q4 readLQuad) (q4 readRQuad) )            
            --get a

            get c 
            get d 
            return () 

degmmBlockStorableRecurPrefetched a b c =
    do 
        --prefetchReadStorableM . unMZ $! b
        --prefetchReadStorableM . unMN $! c
        --prefetchWriteStorableM . unMZ $! a 
        degmmBlockStorableRecur a b c 


{-# INLINE unsafeBlockDiceRecurStorable #-}
unsafeBlockDiceRecurStorable resQuad readLQuad readRQuad =
            do 

            degmmBlockStorableRecurPrefetched(q1 resQuad) (q1 readLQuad) (q1 readRQuad)
            degmmBlockStorableRecurPrefetched (q1 resQuad) (q2 readLQuad) (q2 readRQuad)

            degmmBlockStorableRecurPrefetched (q2 resQuad) (q1 readLQuad) (q3 readRQuad)
            degmmBlockStorableRecurPrefetched  (q2 resQuad) (q2 readLQuad) (q4 readRQuad)


            degmmBlockStorableRecurPrefetched  (q3 resQuad) (q3 readLQuad) (q1 readRQuad)
            degmmBlockStorableRecurPrefetched  (q3 resQuad) (q4 readLQuad) (q2 readRQuad)

            degmmBlockStorableRecurPrefetched (q4 resQuad) (q3 readLQuad) (q3 readRQuad)
            degmmBlockStorableRecurPrefetched (q4 resQuad) (q4 readLQuad) (q4 readRQuad)



{-# INLINE unMZ #-}
unMZ (MZ a) = a

{-# INLINE unMN  #-}
unMN (MN v) = v 


-- i'm assuming you're giving me quadrants that are each 2x2 mats 
--- so  3 * 2 *2 * 4 = 3 * 16 = 48 = 3 * 2^4
{-# INLINE unsafeQuadDirectMzMn2MzMMultStorable #-}
unsafeQuadDirectMzMn2MzMMultStorable
  :: Quad (MortonZ (SM.IOVector Double))
     -> Quad (MortonZ (SM.IOVector Double))
     -> Quad (MortonN (SM.IOVector Double))
     -> IO ()
unsafeQuadDirectMzMn2MzMMultStorable  resQuad readLQuad  readRQuad= 
        do 

            prefetchReadStorableM . unMZ . q2 $! readLQuad
            prefetchReadStorableM . unMN . q2 $! readRQuad
            unsafeDirect2x2MzMnMzStorable (q1 resQuad) (q1 readLQuad) (q1 readRQuad)

            prefetchReadStorableM . unMN . q3 $! readRQuad
            prefetchWriteStorableM . unMZ . q2 $! resQuad
            unsafeDirect2x2MzMnMzStorable (q1 resQuad) (q2 readLQuad) (q2 readRQuad)

            ---------------
            prefetchReadStorableM . unMN . q4 $! readRQuad
            unsafeDirect2x2MzMnMzStorable (q2 resQuad) (q1 readLQuad) (q3 readRQuad)

            prefetchWriteStorableM . unMZ . q3 $! resQuad
            prefetchReadStorableM . unMZ . q3 $! readLQuad
            unsafeDirect2x2MzMnMzStorable (q2 resQuad) (q2 readLQuad) (q4 readRQuad)

            -----------
            prefetchReadStorableM . unMZ . q4 $! readLQuad
            unsafeDirect2x2MzMnMzStorable (q3 resQuad) (q3 readLQuad) (q1 readRQuad)

            prefetchWriteStorableM . unMZ . q4 $! resQuad            
            unsafeDirect2x2MzMnMzStorable (q3 resQuad) (q4 readLQuad) (q2 readRQuad)


            -------
            unsafeDirect2x2MzMnMzStorable (q4 resQuad) (q3 readLQuad) (q3 readRQuad)

            unsafeDirect2x2MzMnMzStorable (q4 resQuad) (q4 readLQuad) (q4 readRQuad)







-- i'm fed 3 2x2 matrices, i'll work on reading / writing them directly

--- INLINE , hopefully that + llvm bb vectorization work well
{-# INLINE unsafeDirect2x2MzMnMzStorable #-}
unsafeDirect2x2MzMnMzStorable ::  MortonZ (SM.IOVector Double)-> 
        MortonZ (SM.IOVector Double)->MortonN (SM.IOVector Double)-> IO ()
unsafeDirect2x2MzMnMzStorable (MZ resMat) (MZ leftReadMat) (MN rightReadMat) = 
        do 
            l0  <- SM.read  leftReadMat  0
            l1 <- SM.read  leftReadMat 1
            r0 <- SM.read rightReadMat 0
            r1 <- SM.read rightReadMat 1

            res0 <- SM.read resMat 0
            SM.write resMat 0 $! (l0 * r0 + l1 * r1 + res0)

            r2 <- SM.read rightReadMat 2 
            r3 <- SM.read rightReadMat 3

            res1 <- SM.read resMat 1

            SM.write resMat 1 $! (l0 * r2 + l1 * r3 + res1)            


            l2 <-SM.read leftReadMat 2
            l3 <- SM.read leftReadMat 3

            res2 <- SM.read resMat 2
            res3 <- SM.read resMat 3

            SM.write resMat 2 $! (l2 * r0 + l3 * r1 + res2)
            SM.write resMat 3 $! (l2 * r2 + l3 * r3 + res3)

            -- these are all register friendly right?
            return ()

unsafeDirect2x2MzMnMzUnbox ::  MortonZ (UM.IOVector Double)-> 
        MortonZ (UM.IOVector Double)->MortonN (UM.IOVector Double)-> IO ()
unsafeDirect2x2MzMnMzUnbox (MZ resMat) (MZ leftReadMat) (MN rightReadMat) = 
        do 
            l0  <- UM.read  leftReadMat  0
            l1 <- UM.read  leftReadMat 1
            r0 <- UM.read rightReadMat 0
            r1 <- UM.read rightReadMat 1

            res0 <- UM.read resMat 0
            UM.write resMat 0 $! (l0 * r0 + l1 * r1 + res0)

            r2 <- UM.read rightReadMat 2 
            r3 <- UM.read rightReadMat 3

            res1 <- UM.read resMat 1

            UM.write resMat 1 $! (l0 * r2 + l1 * r3 + res1)

            l2 <-UM.read leftReadMat 2
            l3 <- UM.read leftReadMat 3

            res2 <- UM.read resMat 2
            res3 <- UM.read resMat 3

            UM.write resMat 2 $! (l2 * r0 + l3 * r1 + res2)
            UM.write resMat 3 $! (l2 * r2 + l3 * r3 + res3)

            -- these are all register friendly right?
            -- need to try this unboxed version out shortly
            return ()




{-
the code i'm writing in this module 
will only work on morton z and flip n  that are square
and whose "elements" are 2x2 blocks

size as an array would be 2 * 2 * 2^k * 2^k 
ie 4* 2^(2k)

-}                        

