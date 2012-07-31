module Spear.Render.StaticModel
(
    StaticModelResource
,   StaticModelRenderer
,   staticModelResource
,   staticModelRenderer
,   Spear.Render.StaticModel.release
,   bind
,   render
)
where


import Spear.Assets.Model
import Spear.Render.Model
import Spear.GLSL
import Spear.Render.Material
import Spear.Render.Program
import Spear.Setup as Setup

import Graphics.Rendering.OpenGL.Raw.Core31
import Unsafe.Coerce (unsafeCoerce)


data StaticModelResource = StaticModelResource
    { vao       :: VAO
    , nVertices :: Int
    , material  :: Material
    , texture   :: Texture
    , rkey      :: Resource
    }


instance Eq StaticModelResource where
    m1 == m2 = vao m1 == vao m2


instance Ord StaticModelResource where
    m1 < m2 = vao m1 < vao m2


data StaticModelRenderer = StaticModelRenderer { model :: StaticModelResource }


instance Eq StaticModelRenderer where
    m1 == m2 = model m1 == model m2


instance Ord StaticModelRenderer where
    m1 < m2 = model m1 < model m2


-- | Create a 'StaticModelResource' from the given 'Model'.
staticModelResource :: StaticProgramChannels
                    -> Material
                    -> Texture
                    -> Model
                    -> Setup StaticModelResource

staticModelResource (StaticProgramChannels vertChan normChan texChan) material texture model = do
    RenderModel elements _ numVertices <- setupIO . renderModelFromModel $ model
    elementBuf <- newBuffer
    vao        <- newVAO
    
    setupIO $ do
        
        let elemSize  = 32
            elemSize' = fromIntegral elemSize
            n         = numVertices
        
        bindVAO vao
        
        bindBuffer elementBuf ArrayBuffer
        bufferData ArrayBuffer (fromIntegral $ elemSize*n) elements StaticDraw
    
        attribVAOPointer vertChan 3 gl_FLOAT False elemSize' 0
        attribVAOPointer normChan 3 gl_FLOAT False elemSize' 12
        attribVAOPointer texChan  2 gl_FLOAT False elemSize' 24
        
        enableVAOAttrib vertChan
        enableVAOAttrib normChan
        enableVAOAttrib texChan
    
    rkey <- register . runSetup_ $ do
        setupIO $ putStrLn "Releasing static model resource"
        releaseVAO vao
        releaseBuffer elementBuf
        --sequence_ . fmap releaseBuffer $ [elementBuf, indexBuf]
    
    return $ StaticModelResource vao (unsafeCoerce numVertices) material texture rkey


-- | Release the given 'StaticModelResource'.
release :: StaticModelResource -> Setup ()
release = Setup.release . rkey


-- | Create a 'StaticModelRenderer' from the given 'StaticModelResource'.
staticModelRenderer :: StaticModelResource -> StaticModelRenderer
staticModelRenderer = StaticModelRenderer


-- | Bind the given 'StaticModelRenderer' to prepare it for rendering.
bind :: StaticProgramUniforms -> StaticModelRenderer -> IO ()
bind (StaticProgramUniforms kaLoc kdLoc ksLoc shiLoc texLoc _ _ _) (StaticModelRenderer model) =
    let (Material _ ka kd ks shi) = material model
    in do
        bindVAO . vao $ model
        bindTexture $ texture model
        activeTexture $= gl_TEXTURE0
        glUniform1i texLoc 0


-- | Render the given 'StaticModelRenderer'.
render :: StaticProgramUniforms -> StaticModelRenderer -> IO ()
render uniforms (StaticModelRenderer model) =
    let (Material _ ka kd ks shi) = material model
    in do
        uniformVec4 (kaLoc uniforms) ka
        uniformVec4 (kdLoc uniforms) kd
        uniformVec4 (ksLoc uniforms) ks
        glUniform1f (shiLoc uniforms) $ unsafeCoerce shi
        drawArrays gl_TRIANGLES 0 $ nVertices model
