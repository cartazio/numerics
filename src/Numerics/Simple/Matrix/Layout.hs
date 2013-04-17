{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE  TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators#-}
{-# LANGUAGE GADTs#-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}

{-#  LANGUAGE ScopedTypeVariables  #-}

module Numerics.Simple.Matrix.Layout where


import GHC.TypeLits     

{- NOTE: should uset Int64# things, punt for now -}

data Shape  lay  = Shp {-# UNPACK #-}  !Int {-# UNPACK #-}  !Int 

--data  a :& b  where
    --Sized :: forall (n:: Nat)  b . Sing n -> b ->  n :& b 

data SizedPow2 (n :: Nat) b   where 
    --SizeUnit :: Sing n ->  b -> Sized




class PrimMatrixLayout lay   where 
    tup2addr :: (# Int,Int #)-> Shape  lay ->Int
    addr2tup :: Int -> Shape lay  -> (# Int,Int #)


-- | For now lets only allow static matrices to be square,
-- also not sure if there needs to be different functions for the static case
class    StaticPrimMatrixLayout f lay  where
    staticTup2Addr ::   (# Int,Int #)-> Shape (f lay) ->Int
    staticAdd2Tup :: Int -> Shape (f lay)  -> (# Int,Int #)


toInt :: Integral a => a  -> Int 
toInt x = (fromIntegral x) :: Int 
{-# INLINE toInt #-}


-- Static* is an attempt to guarantee that statically known layouts
-- can be statically specialized! 
instance ( SingI n,PrimMatrixLayout lay) => StaticPrimMatrixLayout (SizedPow2  (n::Nat)) lay  where
    staticTup2Addr  tup  _ = case 2 ^ ( toInt $! fromSing (sing ::Sing n ) ) of
                        pow2 ->  tup2addr  tup ((Shp pow2 pow2 ) :: Shape lay )
    {-# INLINE staticTup2Addr #-}
    staticAdd2Tup ix _   = case 2 ^ ( toInt $! fromSing (sing:: Sing n ) ) of
                        pow2 ->  addr2tup ix ((Shp pow2 pow2 ) :: Shape lay )
    {-# INLINE staticAdd2Tup #-}    





{- nextAddr = (+1) , -}

{- note, lets punt on having the bounds checking at this layer for now -}


