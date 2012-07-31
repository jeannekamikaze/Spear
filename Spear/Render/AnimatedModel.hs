module Spear.Render.AnimatedModel
(
    AnimatedModelResource
,   AnimatedModelRenderer
,   animatedModelResource
,   animatedModelRenderer
,   Spear.Render.AnimatedModel.release
,   setAnimation
,   currentAnimation
,   bind
,   render
)
where


import Spear.Assets.Model
import Spear.Render.Model
import Spear.GLSL
import Spear.Render.Material
import Spear.Render.Program
import Spear.Updatable
import Spear.Setup as Setup

import Control.Applicative ((<$>), (<*>))
import Graphics.Rendering.OpenGL.Raw.Core31
import Unsafe.Coerce (unsafeCoerce)


-- | An animated model resource.
--
-- Contains model data necessary to render an animated model.
data AnimatedModelResource = AnimatedModelResource
    { model     :: Model
    , vao       :: VAO
    , nFrames   :: Int
    , nVertices :: Int
    , material  :: Material
    , texture   :: Texture
    , rkey      :: Resource
    }


instance Eq AnimatedModelResource where
    m1 == m2 = vao m1 == vao m2


instance Ord AnimatedModelResource where
    m1 < m2 = vao m1 < vao m2


-- | An animated model renderer.
--
-- Holds animation data necessary to render an animated model and a reference
-- to an 'AnimatedModelResource'.
--
-- Model data is kept separate from animation data. This allows instances
-- of 'AnimatedModelRenderer' to share the underlying 'AnimatedModelResource',
-- minimising the amount of data in memory and allowing one to minimise OpenGL
-- state changes by sorting 'AnimatedModelRenderer's by their underlying
-- 'AnimatedModelResource' when rendering the scene.
data AnimatedModelRenderer = AnimatedModelRenderer
    { modelResource :: AnimatedModelResource
    , currentAnim   :: Int
    , frameStart    :: Int
    , frameEnd      :: Int
    , currentFrame  :: Int
    , frameProgress :: Float
    }


instance Eq AnimatedModelRenderer where
    m1 == m2 = modelResource m1 == modelResource m2


instance Ord AnimatedModelRenderer where
    m1 < m2 = modelResource m1 < modelResource m2


-- | Create an 'AnimatedModelResource' from the given 'Model'.
animatedModelResource :: AnimatedProgramChannels
                      -> Material
                      -> Texture
                      -> Model
                      -> Setup AnimatedModelResource

animatedModelResource
    (AnimatedProgramChannels vertChan1 vertChan2 normChan1 normChan2 texChan)
    material texture model = do
        RenderModel elements numFrames numVertices <- setupIO . renderModelFromModel $ model
        elementBuf <- newBuffer
        vao        <- newVAO
        
        setupIO $ do
        
            let elemSize  = 56
                elemSize' = fromIntegral elemSize
                n         = numVertices * numFrames            
            
            bindVAO vao
            
            bindBuffer elementBuf ArrayBuffer
            bufferData ArrayBuffer (unsafeCoerce $ elemSize * n) elements StaticDraw
            
            attribVAOPointer vertChan1 3 gl_FLOAT False elemSize' 0
            attribVAOPointer vertChan2 3 gl_FLOAT False elemSize' 12
            attribVAOPointer normChan1 3 gl_FLOAT False elemSize' 24
            attribVAOPointer normChan2 3 gl_FLOAT False elemSize' 36
            attribVAOPointer texChan   2 gl_FLOAT False elemSize' 48
            
            enableVAOAttrib vertChan1
            enableVAOAttrib vertChan2
            enableVAOAttrib normChan1
            enableVAOAttrib normChan2
            enableVAOAttrib texChan
            
        rkey <- register . runSetup_ $ do
            setupIO $ putStrLn "Releasing animated model resource"
            releaseVAO vao
            releaseBuffer elementBuf        
        
        return $ AnimatedModelResource
            model vao (unsafeCoerce numFrames) (unsafeCoerce numVertices) material texture rkey


-- | Release the given 'AnimatedModelResource'.
release :: AnimatedModelResource -> Setup ()
release = Setup.release . rkey


-- | Create an 'AnimatedModelRenderer' from the given 'AnimatedModelResource'.
animatedModelRenderer :: AnimatedModelResource -> AnimatedModelRenderer
animatedModelRenderer modelResource = AnimatedModelRenderer modelResource 0 0 0 0 0


instance Updatable AnimatedModelRenderer where
    
    update dt (AnimatedModelRenderer model curAnim startFrame endFrame curFrame fp) =
        AnimatedModelRenderer model curAnim startFrame endFrame curFrame' fp'
            where f = fp + dt
                  nextFrame = f >= 1.0
                  fp' = if nextFrame then f - 1.0 else f
                  curFrame' = if nextFrame
                              then let x = curFrame + 1 in if x > endFrame then startFrame else x
                              else curFrame


-- | Set the active animation to the given one.
setAnimation :: Enum a => a -> AnimatedModelRenderer -> AnimatedModelRenderer
setAnimation anim modelRend =
    let (Animation _ f1 f2) = animation (model . modelResource $ modelRend) anim'
        anim' = fromEnum anim
    in
        modelRend { currentAnim = anim', frameStart = f1, frameEnd = f2, currentFrame = f1 }


-- | Get the renderer's current animation.
currentAnimation :: Enum a => AnimatedModelRenderer -> a
currentAnimation = toEnum . currentAnim


-- | Bind the given 'AnimatedModelRenderer' to prepare it for rendering.
bind :: AnimatedProgramUniforms -> AnimatedModelRenderer -> IO ()
bind (AnimatedProgramUniforms kaLoc kdLoc ksLoc shiLoc texLoc _ _ _ _) modelRend =
    let model' = modelResource modelRend
    in do
        bindVAO . vao $ model'
        bindTexture $ texture model'
        activeTexture $= gl_TEXTURE0
        glUniform1i texLoc 0


-- | Render the model described by the given 'AnimatedModelRenderer'.
render :: AnimatedProgramUniforms -> AnimatedModelRenderer -> IO ()
render uniforms (AnimatedModelRenderer model _ _ _ curFrame fp) =
    let n = nVertices model
        (Material _ ka kd ks shi) = material model
    in do
        uniformVec4 (kaLoc uniforms) ka
        uniformVec4 (kdLoc uniforms) kd
        uniformVec4 (ksLoc uniforms) ks
        glUniform1f (shiLoc uniforms) $ unsafeCoerce shi
        glUniform1f (fpLoc uniforms) (unsafeCoerce fp)
        drawArrays gl_TRIANGLES (n*curFrame) n
