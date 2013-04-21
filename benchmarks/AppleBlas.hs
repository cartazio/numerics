
module AppleBlas(blasMMult) where



import Foreign
import Foreign.C.Types
import Unsafe.Coerce
import Prelude hiding (replicate)
--import Data.Storable
import Data.Vector.Storable.Mutable  

foreign import ccall unsafe "simple_dgemm"
    dgemm :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()

dgemm_wrapped :: Ptr Double -> Ptr Double -> Ptr Double -> Int -> IO ()
dgemm_wrapped = unsafeCoerce dgemm
--- this is ok because CDouble is a newtyped Double, and CInt is a newtyped Int 

blasMMult :: IOVector Double -> IOVector Double -> IOVector Double -> Int -> IO ()
blasMMult aVect bVect  cVect n =
    unsafeWith aVect $! \aPtr ->
               unsafeWith bVect $!  \bPtr -> 
                 unsafeWith cVect  $! \cPtr -> 
                    dgemm_wrapped aPtr bPtr cPtr n  

mkABC :: Int -> IO (IOVector Double, IOVector Double, IOVector Double )
mkABC n = do 
        cVect <- replicate (2^n) 0.0 
        aVect <- replicate (2^n) 0.0
        bVect <- replicate (2^n) 0.0
        return (cVect, aVect, bVect)
  