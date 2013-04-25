{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}


module Numerics.Simple.Util where

import Data.Typeable
import Data.Data 
import Data.Word 

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Exts
import GHC.IO 
import GHC.ForeignPtr
import Control.Monad.Primitive

import qualified Data.Vector.Storable.Mutable as SM

--- | TupInt is just a proxy for using unboxed tuples, 
----  Should evaluate using those Tupint
data TupInt = TI {-#UNPACK#-} !Int {-#UNPACK#-} !Int
    deriving (Eq,Read,Show,Typeable,Data)


data TupWord = TW {-#UNPACK#-} !Word {-#UNPACK#-} !Word
    deriving (Eq,Read,Show,Typeable ,Data )
--- | quotRemStrong taks an int, the exponent of a number 2^k as the value k, 
---  returns (TI (a >> k)  (a .&. ((1 << k) -1 )  ) , the quotient and remainder


unsafePointerShiftWith   byteShift f ptr =  case f  $! pshifted of 
                                        !res -> res 
    where 
        !pshifted =  plusPtr ptr byteShift 


foreign import ccall unsafe "prefetch.c prefetchRead3" 
    c_prefetchRead :: Ptr a -> IO ()

prefetchReadStorableM v = SM.unsafeWith v c_prefetchRead  
prefetchReadStorableShiftedM  byteShift v = SM.unsafeWith v  $! unsafePointerShiftWith byteShift c_prefetchRead

-- probably useless
prefetchReadStorable v = unsafeDupablePerformIO $! prefetchReadStorableM v

foreign import ccall unsafe "prefetch.c prefetchWrite3" 
    c_prefetchWrite  ::  Ptr a -> IO ()

prefetchWriteStorableM   v = SM.unsafeWith v c_prefetchRead 
prefetchWriteStorableShiftedM byteShift v = SM.unsafeWith v  $! unsafePointerShiftWith byteShift c_prefetchWrite


-- probably useless
prefetchWriteStorable v = unsafeDupablePerformIO $! prefetchWriteStorableM v 



{-# INLINE mallocAlignedVectorAVX #-}
mallocAlignedVectorAVX :: Storable a => Int -> IO (ForeignPtr a)
mallocAlignedVectorAVX =
#if __GLASGOW_HASKELL__ >= 605
    doMalloc undefined
        where
          doMalloc :: Storable b => b -> Int -> IO (ForeignPtr b)
          doMalloc dummy size = mallocPlainForeignPtrBytesAlignedAVX (size * sizeOf dummy)
#else
    mallocForeignPtrArray
#endif


-- | This function is similar to 'mallocForeignPtrBytes', except that
-- the internally an optimised ForeignPtr representation with no
-- finalizer is used. Attempts to add a finalizer will cause an
-- exception to be thrown.
mallocPlainForeignPtrBytesAlignedAVX :: Int -> IO (ForeignPtr a)
mallocPlainForeignPtrBytesAlignedAVX !(I# size) = IO $ \s ->
        case newAlignedPinnedByteArray# size align s      of { (# s', mbarr# #) ->
           (# s', ForeignPtr (byteArrayContents# (unsafeCoerce# mbarr#))
                             (PlainPtr mbarr#) #)
         }
    where 
        !(I# align) =  32 -- 256/8 = 32

{-# INLINE basicUnsafeNewAVX #-}
basicUnsafeNewAVX n  = unsafePrimToPrim
    $ do
        fp <- mallocAlignedVectorAVX n
        return $ SM.MVector n fp

{-# INLINE replicateAlignedAVX #-}
replicateAlignedAVX ::(PrimMonad m, Storable a) => Int -> a -> m (SM.MVector (PrimState m) a)
replicateAlignedAVX size init =
        do  res <-  basicUnsafeNewAVX size
            SM.set res init
            return res 
