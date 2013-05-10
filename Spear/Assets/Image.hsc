{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Spear.Assets.Image
(
    -- * Data types
    Image
    -- * Loading and unloading
,   loadImage
    -- * Accessors
,   width
,   height
,   bpp
,   pixels
)
where

import Spear.Game
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils as Foreign (with)
import Foreign.Marshal.Alloc (alloca)
import Data.List (splitAt, elemIndex)
import Data.Char (toLower)

#include "Image.h"
#include "BMP/BMP_load.h"

data ImageErrorCode
    = ImageSuccess
    | ImageReadError
    | ImageMemoryAllocationError
    | ImageFileNotFound
    | ImageInvalidFormat
    | ImageNoSuitableLoader
    deriving (Eq, Enum, Show)

data CImage = CImage
    { cwidth  :: CInt
    , cheight :: CInt
    , cbpp    :: CInt
    , cpixels :: Ptr CUChar
    }

instance Storable CImage where
    sizeOf _    = #{size Image}
    alignment _ = alignment (undefined :: CInt)
    
    peek ptr = do
        width  <- #{peek Image, width}  ptr
        height <- #{peek Image, height} ptr
        bpp    <- #{peek Image, bpp}    ptr
        pixels <- #{peek Image, pixels} ptr
        return $ CImage width height bpp pixels
    
    poke ptr (CImage width height bpp pixels) = do
        #{poke Image, width}  ptr width
        #{poke Image, height} ptr height
        #{poke Image, bpp}    ptr bpp
        #{poke Image, pixels} ptr pixels

-- | Represents an image 'Resource'.
data Image = Image
    { imageData :: CImage
    , rkey      :: Resource
    }

instance ResourceClass Image where
         getResource = rkey

foreign import ccall "Image.h image_free"
    image_free :: Ptr CImage -> IO ()

foreign import ccall "BMP_load.h BMP_load"
    bmp_load' :: Ptr CChar -> Ptr CImage -> IO Int

bmp_load :: Ptr CChar -> Ptr CImage -> IO ImageErrorCode
bmp_load file image = bmp_load' file image >>= \code -> return . toEnum $ code

-- | Load the image specified by the given file.
loadImage :: FilePath -> Game s Image
loadImage file = do
    dotPos <- case elemIndex '.' file of
            Nothing -> gameError $ "file name has no extension: " ++ file
            Just p  -> return p
    
    let ext = map toLower . tail . snd $ splitAt dotPos file
    
    result <- gameIO . alloca $ \ptr -> do
        status <- withCString file $ \fileCstr -> do
            case ext of
                "bmp" -> bmp_load fileCstr ptr
                _     -> return ImageNoSuitableLoader
        
        case status of
            ImageSuccess               -> peek ptr >>= return . Right
            ImageReadError             -> return . Left $ "read error"
            ImageMemoryAllocationError -> return . Left $ "memory allocation error"
            ImageFileNotFound          -> return . Left $ "file not found"
            ImageInvalidFormat         -> return . Left $ "invalid format"
            ImageNoSuitableLoader      -> return . Left $ "no suitable loader for extension " ++ ext
        
    case result of
        Right image -> register (freeImage image) >>= return . Image image
        Left  err   -> gameError $ "loadImage: " ++ err

-- | Free the given 'CImage'.
freeImage :: CImage -> IO ()
freeImage image = Foreign.with image image_free

-- | Return the given image's width.
width :: Image -> Int
width = fromIntegral . cwidth . imageData

-- | Return the given image's height.
height :: Image -> Int
height = fromIntegral . cheight . imageData

-- | Return the given image's bits per pixel.
bpp :: Image -> Int
bpp = fromIntegral . cbpp . imageData

-- | Return the given image's pixels.
pixels :: Image -> Ptr CUChar
pixels = cpixels . imageData
