module Spear.Render.Texture
(
    loadTextureImage
)
where


import Spear.Setup
import Spear.Assets.Image
import Spear.GLSL
import Data.StateVar (($=))
import Graphics.Rendering.OpenGL.Raw.Core31


-- | Load the 'Texture' specified by the given file.
loadTextureImage :: FilePath
                 -> GLenum          -- ^ Texture's min filter.
                 -> GLenum          -- ^ Texture's mag filter.
                 -> Setup Texture
loadTextureImage file minFilter magFilter = do
    image <- loadImage file
    tex   <- newTexture
    setupIO $ do
        let w    = width  image
            h    = height image
            pix  = pixels image
            rgb  = fromIntegral . fromEnum $ gl_RGB
        
        bindTexture tex
        loadTextureData gl_TEXTURE_2D 0 rgb w h 0 gl_RGB gl_UNSIGNED_BYTE pix
        texParami gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $= minFilter
        texParami gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $= magFilter
    
    return tex
