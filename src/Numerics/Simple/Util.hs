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

--- | TupInt is just a proxy for using unboxed tuples, 
----  Should evaluate using those Tupint
data TupInt = TI {-#UNPACK#-} !Int {-#UNPACK#-} !Int
    deriving (Eq,Read,Show,Typeable,Data)


data TupWord = TW {-#UNPACK#-} !Word {-#UNPACK#-} !Word
    deriving (Eq,Read,Show,Typeable ,Data )
--- | quotRemStrong taks an int, the exponent of a number 2^k as the value k, 
---  returns (TI (a >> k)  (a .&. ((1 << k) -1 )  ) , the quotient and remainder