
module AppleBlas(blasMMult, pureMkCAB) where



import Foreign
import Foreign.C.Types
import Unsafe.Coerce
import Prelude hiding (replicate)
--import Data.Storable
--import System.IO.Unsafe
import Data.Vector.Storable.Mutable  
import GHC.Ptr (castPtr)

foreign import ccall unsafe "simple_dgemm"
    dgemm :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

saphWrapper :: (Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ())-> ( Ptr Double -> Ptr Double -> Ptr Double -> Int -> IO ())
saphWrapper f = (\c a b  n -> f (castPtr c ) (castPtr a) (castPtr c )  (CInt  $ fromIntegral n ))
-- this is always safe/ok because CDouble is a newtyped Double, and CInt is a newtyped Int 

dgemm_wrapped :: Ptr Double -> Ptr Double -> Ptr Double -> Int -> IO ()
dgemm_wrapped = saphWrapper dgemm



blasMMult :: IOVector Double -> IOVector Double -> IOVector Double -> Int -> IO ()
blasMMult aVect bVect  cVect n =
    unsafeWith aVect $! \aPtr ->
               unsafeWith bVect $!  \bPtr -> 
                 unsafeWith cVect  $! \cPtr -> 
                    dgemm_wrapped aPtr bPtr cPtr n  

-- returns (2^n x 2^n matrices, so builds vectors of size 4^n)
mkABCIO :: Int -> IO (IOVector Double, IOVector Double, IOVector Double )
mkABCIO n = do 
        cVect <- replicate (4^n) 0.0 
        aVect <- replicate (4^n) 0.0
        bVect <- replicate (4^n) 0.0
        return (cVect, aVect, bVect)
  
--- need this to allow sanely write the matrix mult benchmarks :)
--- lets us guarantee that the allocations only happen on the first sample run :) 
pureMkCAB :: Int ->(IOVector Double, IOVector Double, IOVector Double ) 
pureMkCAB n = unsafePerformIO $! mkABCIO n 
{-# NOINLINE pureMkCAB #-}