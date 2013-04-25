{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
--import Math.Indexors
import Numerics.Simple.Bits

import Criterion.Config
import Data.Word 

import Data.Foldable 
import AppleBlas

import Numerics.Simple.POC 


whnfIter:: Int ->(a->a)-> a -> Pure 
whnfIter cnt f  arg = whnf (\v -> foldl' (\ a b -> f a ) v [0 .. cnt]  ) arg

main =  defaultMainWith defaultConfig{cfgSamples=ljust 4} (return ()) [
    bgroup "Morton Z" [
    {-bgroup "morton" $!-} bcompare [ bench "outerShuffle64B 1000" $! whnfIter 1000 outerShuffle64B 7, bench "outerShuffle64A 1000" $! whnfIter 1000 outerShuffle64A 7  , bench "addingNumbersIter1000" $! whnfIter 1000 ( (7 + ):: Word->Word)  9 ,
        bench "outerUnShuffle64B 1000" $! whnfIter 1000 outerUnShuffle64B 7 ,
        bench "outerUnShuffle64A 1000" $! whnfIter 1000 outerUnShuffle64A 7 ,
        bench "outerUnShuffle64A dot outerShuffle64A 1000" $! whnfIter 1000 (\x ->outerUnShuffle64A $! outerShuffle64A x) 7 ,
        bench "outerUnShuffle64B dot outerShuffle64A 1000" $! whnfIter 1000 (\x ->outerUnShuffle64B $! outerShuffle64A x) 7 ,
        bench "id 1000" $! whnfIter 1000 id 7 
        ]],
    bgroup "rowMaj" [],
    bgroup "colMaj" [],

    bgroup "In L1, 8kb 2^12 each "  
       ( let    vTup=pureMkCAB powIx  
                localsize = 2^powIx
                powIx = 8
                in 
                     [bcompare [ bench "AppleBlas "  $! whnfIO (
                        do  (!cv,!av,!bv)<- return vTup 
                            blasMMult cv av bv (localsize))

                           ,
                           bench "POC Block"  $! whnfIO (
                               do  
                                    (!cv,!av,!bv)<- return vTup 
                                    dgemmBlockWrapped cv av bv 
                                    ),
                           bench "POC Naive Dot"  $! whnfIO (
                               do  
                                    (!cv,!av,!bv)<- return vTup 
                                    dotMMultStorable cv av bv  localsize
                                    ) ]] )
       ,bgroup "In L2,  2^16 each "  
               ( let    vTup=pureMkCAB powIx  
                        localsize = 2^powIx
                        powIx = 8
                        in 
                             [bcompare [ bench "AppleBlas "  $! whnfIO (
                                do  (!cv,!av,!bv)<- return vTup 
                                    blasMMult cv av bv (localsize))
                                   ,
                                   bench "POC Block"  $! whnfIO (
                                       do  
                                            (!cv,!av,!bv)<- return vTup 
                                            dgemmBlockWrapped cv av bv 
                                            ),
                                   bench "POC Naive Dot"  $! whnfIO (
                                       do  
                                            (!cv,!av,!bv)<- return vTup 
                                            dotMMultStorable cv av bv  localsize
                                            )
                               ]] )
        ,bgroup "In L3 2^18 each "  
           ( let    vTup=pureMkCAB powIx  
                    localsize = 2^powIx
                    powIx = 9
                    in 
                         [bcompare [ bench "AppleBlas "  $! whnfIO (
                            do  (!cv,!av,!bv)<- return vTup 
                                blasMMult cv av bv (localsize))
                               ,
                               bench "POC Block"  $! whnfIO (
                                   do  
                                        (!cv,!av,!bv)<- return vTup 
                                        dgemmBlockWrapped cv av bv 
                                        ),
                               bench "POC Naive Dot"  $! whnfIO (
                                   do  
                                        (!cv,!av,!bv)<- return vTup 
                                        dotMMultStorable cv av bv  localsize
                                        )]] )

{-
2^(23) * 3 bytes
-}               
            ,bgroup "beyond  L3 2^20 elms *8 bytes each "  
               ( let    vTup=pureMkCAB powIx  
                        localsize = 2^powIx
                        powIx = 10
                        in 
                             [bcompare [ bench "AppleBlas "  $! whnfIO (
                                do  (!cv,!av,!bv)<- return vTup 
                                    blasMMult cv av bv (localsize))
                                   ,
                                   bench "POC Block"  $! whnfIO (
                                       do  
                                            (!cv,!av,!bv)<- return vTup 
                                            dgemmBlockWrapped cv av bv 
                                            ) 
                                   --,
                                   --bench "POC Naive Dot"  $! whnfIO (
                                   --    do  
                                   --         (!cv,!av,!bv)<- return vTup 
                                   --         dotMMultStorable cv av bv  localsize
                                   --         )
                                   ]] )               

            --,bgroup "beyond l3,  2^24 each, > 50mb total "  
            --   ( let    vTup=pureMkCAB powIx  
            --            localsize = 2^powIx
            --            powIx = 12
            --            in 
            --                 [bcompare [ bench "AppleBlas "  $! whnfIO (
            --                    do  (!cv,!av,!bv)<- return vTup 
            --                        blasMMult cv av bv (localsize))
            --                       ],
            --                       bench "POC Block"  $! whnfIO (
            --                           do  
            --                                (!cv,!av,!bv)<- return vTup 
            --                                dgemmBlockWrapped cv av bv 
            --                                )] )            


        ] 
    
    
    

