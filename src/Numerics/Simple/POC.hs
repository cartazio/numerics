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


{-
convention in this module
matrix M  =  decomposes as 
 
 A | B
 -----
 C | D 


-}

data Dice = Dice  {-#UNPACK#-} !Int {-# UNPACK #-} !Int 

{-  the max offset of the len element dice will be (base + len - 1) -}
diceLen (Dice len ix) = len
{-# INLINE diceLen #-}

{- the "base"  is the index 0 position within the slice-}
diceBase (Dice len ix)= ix
{-# INLINE diceBase #-}

data MortonZDecomp = MZDecomp { mzdOffsetA :: !Dice,
                        mzdOffsetB :: !Dice , mzdOffsetC :: !Int, mzdOffsetD:: !Int }



data MortonFlipNDecomp = MFND { mfnLen :: !Int , mfnOffsetA :: !Int,
                        mfnOffsetB :: !Int , mfnOffsetC :: !Int, mfnOffsetD:: !Int  }

{-
the code i'm writing in this module 
will only work on morton z and flip n  that are square
and whose "elements" are 2x2 blocks

size as an array would be 2 * 2 * 2^k * 2^k 
ie 4* 2^(2k)

-}                        

