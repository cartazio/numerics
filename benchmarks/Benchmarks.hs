module Main where

import Criterion.Main
--import Math.Indexors
import Data.Numerics.Simple.Bits
import Criterion.Config
import Data.Word 

import Data.Foldable 

whnfIter:: Int ->(a->a)-> a -> Pure 
whnfIter cnt f  arg = whnf (\v -> foldl' (\ a b -> f a ) v [0 .. cnt]  ) arg

main =  defaultMainWith defaultConfig (return ()) [
                bgroup "indexors" [
                {-bgroup "morton" $!-} bcompare [ bench "outerShuffle64B1000" $! whnfIter 1000 outerShuffle64B 7, bench "outerShuffle64A1000" $! whnfIter 1000 outerShuffle64A 7  , bench "addingNumbersIter1000" $! whnfIter 1000 ( (7 + ):: Word->Word)  9 ],
                bgroup "rowMaj" [],
                bgroup "colMaj" []
                ]]

