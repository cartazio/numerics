{-# LANGUAGE ConstraintKinds#-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE MagicHash, UnboxedTuples, DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns#-}
{-# LANGUAGE ScopedTypeVariables #-}


module Numerics.Simple.Util where

import Data.Typeable
import Data.Data 
import Data.Word 

import Foreign
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafeDupablePerformIO)

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

