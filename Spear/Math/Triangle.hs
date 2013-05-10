module Spear.Math.Triangle
(
    Triangle(..)
)
where


import Spear.Math.Vector

import Foreign.C.Types
import Foreign.Storable


data Triangle = Triangle
    { p0 :: {-# UNPACK #-} !Vector3
    , p1 :: {-# UNPACK #-} !Vector3
    , p2 :: {-# UNPACK #-} !Vector3
    }


sizeVector3 = 3 * sizeOf (undefined :: CFloat)


instance Storable Triangle where
    
    sizeOf _    = 3 * sizeVector3
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        p0 <- peekByteOff ptr 0
        p1 <- peekByteOff ptr $ 1 * sizeVector3
        p2 <- peekByteOff ptr $ 2 * sizeVector3
        
        return $ Triangle p0 p1 p2
    
    
    poke ptr (Triangle p0 p1 p2) = do
        pokeByteOff ptr 0 p0
        pokeByteOff ptr (1*sizeVector3) p1
        pokeByteOff ptr (2*sizeVector3) p2
