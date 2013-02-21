module Spear.Render.StaticModel
(
    -- * Data types
    StaticModelResource
,   StaticModelRenderer
    -- * Construction and destruction
,   staticModelResource
,   staticModelRenderer
,   Spear.Render.StaticModel.release
    -- * Manipulation
,   box
,   modelRes
    -- * Rendering
,   bind
,   render
    -- * Collision
,   mkColsFromStatic
)
where


import Spear.Assets.Model
import Spear.Collision
import Spear.GLSL
import Spear.Math.AABB
import Spear.Math.Matrix4 (Matrix4)
import Spear.Math.Vector
import Spear.Render.Material
import Spear.Render.Model
import Spear.Render.Program
import Spear.Setup as Setup

import qualified Data.Vector as V
import Graphics.Rendering.OpenGL.Raw.Core31
import Unsafe.Coerce (unsafeCoerce)


data StaticModelResource = StaticModelResource
    { vao       :: VAO
    , nVertices :: Int
    , material  :: Material
    , texture   :: Texture
    , boxes     :: V.Vector Box
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


-- | Create a model resource from the given model.
staticModelResource :: StaticProgramChannels
                    -> Material
                    -> Texture
                    -> Model
                    -> Setup StaticModelResource

staticModelResource (StaticProgramChannels vertChan normChan texChan) material texture model = do
    RenderModel elements _ numVertices <- setupIO . renderModelFromModel $ model
    elementBuf <- newBuffer
    vao        <- newVAO
    boxes      <- setupIO $ modelBoxes model
    
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
    
    return $ StaticModelResource
        vao (unsafeCoerce numVertices) material texture boxes rkey


-- | Release the given model resource.
release :: StaticModelResource -> Setup ()
release = Setup.release . rkey


-- | Create a renderer from the given model resource.
staticModelRenderer :: StaticModelResource -> StaticModelRenderer
staticModelRenderer = StaticModelRenderer


-- | Get the model's ith bounding box.
box :: Int -> StaticModelResource -> Box
box i model = boxes model V.! i


-- | Get the renderer's model resource.
modelRes :: StaticModelRenderer -> StaticModelResource
modelRes = model


-- | Bind the given renderer to prepare it for rendering.
bind :: StaticProgramUniforms -> StaticModelRenderer -> IO ()
bind (StaticProgramUniforms kaLoc kdLoc ksLoc shiLoc texLoc _ _ _) (StaticModelRenderer model) =
    let (Material _ ka kd ks shi) = material model
    in do
        bindVAO . vao $ model
        bindTexture $ texture model
        activeTexture $= gl_TEXTURE0
        glUniform1i texLoc 0


-- | Render the given renderer.
render :: StaticProgramUniforms -> StaticModelRenderer -> IO ()
render uniforms (StaticModelRenderer model) =
    let (Material _ ka kd ks shi) = material model
    in do
        uniformVec4 (kaLoc uniforms) ka
        uniformVec4 (kdLoc uniforms) kd
        uniformVec4 (ksLoc uniforms) ks
        glUniform1f (shiLoc uniforms) $ unsafeCoerce shi
        drawArrays gl_TRIANGLES 0 $ nVertices model


-- | Compute AABB collisioners in view space from the given model.
mkColsFromStatic
    :: Matrix4 -- ^ Modelview matrix
    -> StaticModelResource
    -> [Collisioner]
mkColsFromStatic modelview modelRes = mkCols modelview (box 0 modelRes)
