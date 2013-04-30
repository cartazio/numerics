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

import Control.Monad.Primitive (touch )

import Numerics.Simple.KernelPOC

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
                                            leftOperand <- SM.unsafeRead leftMat $! ( currentRowBase+ ix )
                                            rightOperand <- SM.unsafeRead rightMat $! (currentColBase  + ix)
                                            return $! (partialSum + leftOperand * rightOperand)
                                        --- this is not the most numerical stable dot product, but whatever
                            in 
                                do 
                                    dotprod <- foldlM fma 0 [0 .. n -1 ]
                                    SM.unsafeWrite resMat (currentRowBase + col )  dotprod
                                    return ()
                    ))





type KerFunType = SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO () 

---  
dgemmBlockWrapped :: SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO () 
dgemmBlockWrapped !a !b !c =  dgemmBlockWrappedGen  quadDirectSimpleC  a b c 

dgemmBlockNOOP !a !b !c = dgemmBlockWrappedGen  fancyNOOP a b c 

{-# NOINLINE fancyNOOP #-}
fancyNOOP !res !leftM !rightM = 
    SM.unsafeWith res $! \a -> 
        SM.unsafeWith leftM $! \b ->
            SM.unsafeWith rightM $! \c ->  return () 

{-# NOINLINE noopKern #-}
noopKern :: KerFunType 
noopKern a b c  = return () 

dgemmBlockWrappedGen :: KerFunType ->SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO () 
dgemmBlockWrappedGen !kerfun = \ !a !b  !c -> (runParIO $!  dgemmBlockStorableRecurTopGen kerfun (MZ a) (MZ b) (MN c))

{-# INLINE dgemmBlockStorableRecurTopGen #-}
--dgemmBlockStorableRecurTopGen :: KerFunType ->SM.IOVector Double -> SM.IOVector Double -> SM.IOVector Double -> IO () 
dgemmBlockStorableRecurTopGen  !kerfun !res@(MZ resArr)  !readL  !readR  =
        do 
            unsafeBlockDiceRecurStorableTopGen kerfun (unsafeDiceMZ res) 
                                        (unsafeDiceMZ readL) (unsafeDiceMFlipN readR)




degmmBlockStorableRecur   !res@(MZ resArr)  !readL  !readR   | SM.length resArr < 16  ||  SM.length (unMZ readL) < 16 || SM.length (unMN readR) < 16 =  error  $! ("bad params"++ show (SM.length resArr))
                            
                            | SM.length resArr == 16 =  
                                quadDirectSimpleC (unMZ res) (unMZ readL) (unMN readR)
                                --unsafeQuadDirectMzMn2MzMMultStorable (unsafeDiceMZ res) 
                                        --(unsafeDiceMZ readL) (unsafeDiceMFlipN readR)
                         | otherwise = 
                            do 
                                unsafeBlockDiceRecurStorable (unsafeDiceMZ res) 
                                        (unsafeDiceMZ readL) (unsafeDiceMFlipN readR)

{-# INLINE unsafeBlockDiceRecurStorableTopGen #-}
unsafeBlockDiceRecurStorableTopGen :: KerFunType 
        -> Quad  (MortonZ (SM.IOVector Double)) ->  Quad  (MortonZ( SM.IOVector Double)) 
            -> Quad  (MortonN (SM.IOVector Double)) -> ParIO () 
unsafeBlockDiceRecurStorableTopGen  !kernelFun resQuad !readLQuad !readRQuad = 

        do 
            a<- spawn $! liftIO ( 
                do  !r<- degmmBlockStorableRecur(q1 resQuad) (q1 readLQuad) (q1 readRQuad)
                    !r<- degmmBlockStorableRecur (q1 resQuad) (q2 readLQuad) (q2 readRQuad) 
                    return () ) 
            --get a 
            b <- spawn $! liftIO (
                do  !r<-  degmmBlockStorableRecur (q2 resQuad) (q1 readLQuad) (q3 readRQuad)
                    !r<-  degmmBlockStorableRecur  (q2 resQuad) (q2 readLQuad) (q4 readRQuad)
                    return () )
            --get b
            get a 
            get b 
            c <- spawn $! liftIO (
                do  !r<-  degmmBlockStorableRecur  (q3 resQuad) (q3 readLQuad) (q1 readRQuad)
                    !r<-  degmmBlockStorableRecur  (q3 resQuad) (q4 readLQuad) (q2 readRQuad)
                    return ()   )
            --get c 
            --get b
            ------ lets try sequentially for a wee bit
            d <- spawn $!  liftIO (
                do  (degmmBlockStorableRecur (q4 resQuad) (q3 readLQuad) (q3 readRQuad))
                    degmmBlockStorableRecur (q4 resQuad) (q4 readLQuad) (q4 readRQuad) )            
            --get a

            get c 
            get d 
            return () 
            

    where 
        {-# NOINLINE unsafeBlockDiceRecurStorableGen #-}
        unsafeBlockDiceRecurStorableGen !resQuad !readLQuad !readRQuad =
                do 

                    degmmBlockStorableRecurGen (q1 resQuad) (q1 readLQuad) (q1 readRQuad)
                    degmmBlockStorableRecurGen (q1 resQuad) (q2 readLQuad) (q2 readRQuad)

                    degmmBlockStorableRecurGen (q2 resQuad) (q1 readLQuad) (q3 readRQuad)
                    degmmBlockStorableRecurGen  (q2 resQuad) (q2 readLQuad) (q4 readRQuad)


                    degmmBlockStorableRecurGen  (q3 resQuad) (q3 readLQuad) (q1 readRQuad)
                    degmmBlockStorableRecurGen  (q3 resQuad) (q4 readLQuad) (q2 readRQuad)

                    degmmBlockStorableRecurGen (q4 resQuad) (q3 readLQuad) (q3 readRQuad)
                    degmmBlockStorableRecurGen (q4 resQuad) (q4 readLQuad) (q4 readRQuad)
                    return ()
        {-# INLINE degmmBlockStorableRecurGen #-}
        degmmBlockStorableRecurGen   !res@(MZ resArr)  !readL  !readR   
                | SM.length resArr < 16   ||  SM.length (unMZ readL) < 16  || SM.length (unMN readR) < 16 = 
                                 error  $! ("bad params"++ show (SM.length resArr))
                
                | SM.length resArr == 16 =  
                    -- the 4x4 * 3 kernel fun call
                    do  
                        kernelFun (unMZ res) (unMZ readL) (unMN readR)
                        return () 
                    --unsafeQuadDirectMzMn2MzMMultStorable (unsafeDiceMZ res) 
                            --(unsafeDiceMZ readL) (unsafeDiceMFlipN readR)
                | otherwise = 
                do 
                    unsafeBlockDiceRecurStorableGen (unsafeDiceMZ res)  (unsafeDiceMZ readL) (unsafeDiceMFlipN readR)
                    return ()


degmmBlockStorableRecurPrefetched !a !b !c =
    do 
        --prefetchReadStorableM . unMZ $! b
        --prefetchReadStorableM . unMN $! c
        --prefetchWriteStorableM . unMZ $! a 
        degmmBlockStorableRecur a b c 


{-# INLINE unsafeBlockDiceRecurStorable #-}
unsafeBlockDiceRecurStorable !resQuad !readLQuad !readRQuad =
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












{-
the code i'm writing in this module 
will only work on morton z and flip n  that are square
and whose "elements" are 2x2 blocks

size as an array would be 2 * 2 * 2^k * 2^k 
ie 4* 2^(2k)

-}                        

