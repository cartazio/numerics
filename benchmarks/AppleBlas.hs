
module AppleBlas(blasMMult) where

import Data.Vector.Storable

import Foreign
import Foreign.C.Types
--import Data.Storable
import qualified Data.Vector.Mutable as M 

foreign import ccall unsafe "simple_dgemm"
    dgemm :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> CInt -> IO ()


blasMMult :: M.IOVector Double -> M.IOVector Double -> M.IOVector Double -> Int -> IO ()
blasMMult aVect bVect  cVect n = undefined


