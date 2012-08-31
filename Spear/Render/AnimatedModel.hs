module Spear.Render.AnimatedModel
(
    -- * Data types
    AnimatedModelResource
,   AnimatedModelRenderer
,   AnimationSpeed
    -- * Construction and destruction
,   animatedModelResource
,   animatedModelRenderer
,   Spear.Render.AnimatedModel.release
    -- * Accessors
,   animationSpeed
,   box
,   currentAnimation
,   currentFrame
,   frameProgress
,   modelRes
,   nextFrame
    -- * Manipulation
,   update
,   setAnimation
,   setAnimationSpeed
    -- * Rendering
,   bind
,   render
)
where


import Spear.Assets.Model
import Spear.Render.Model
import Spear.GLSL
import Spear.Math.AABB
import Spear.Math.Vector2 (vec2)
import Spear.Render.Material
import Spear.Render.Program
import Spear.Setup as Setup

import Control.Applicative ((<$>), (<*>))
import qualified Data.Vector as V
import Graphics.Rendering.OpenGL.Raw.Core31
import Unsafe.Coerce (unsafeCoerce)


type AnimationSpeed = Float


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
    , boxes     :: V.Vector Box
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
    { modelResource  :: AnimatedModelResource
    , currentAnim    :: Int
    , frameStart     :: Int
    , frameEnd       :: Int
    , currentFrame   :: Int -- ^ Get the renderer's current frame.
    , frameProgress  :: Float -- ^ Get the renderer's frame progress.
    , animationSpeed :: Float -- ^ Get the renderer's animation speed.
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
        boxes      <- setupIO $ modelBoxes model
        
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
            model vao (unsafeCoerce numFrames) (unsafeCoerce numVertices)
            material texture boxes rkey


-- | Release the given 'AnimatedModelResource'.
release :: AnimatedModelResource -> Setup ()
release = Setup.release . rkey


-- | Create an 'AnimatedModelRenderer' from the given 'AnimatedModelResource'.
animatedModelRenderer :: AnimationSpeed -> AnimatedModelResource -> AnimatedModelRenderer
animatedModelRenderer animSpeed modelResource =
    AnimatedModelRenderer modelResource 0 0 0 0 0 animSpeed


-- | Update the 'AnimatedModelRenderer'.
update dt (AnimatedModelRenderer model curAnim startFrame endFrame curFrame fp s) =
    AnimatedModelRenderer model curAnim startFrame endFrame curFrame' fp' s
        where f = fp + dt * s
              nextFrame = f >= 1.0
              fp' = if nextFrame then f - 1.0 else f
              curFrame' = if nextFrame
                          then let x = curFrame + 1
                               in if x > endFrame then startFrame else x
                          else curFrame


-- | Get the model's ith bounding box.
box :: Int -> AnimatedModelResource -> Box
box i model = boxes model V.! i


-- | Get the renderer's current animation.
currentAnimation :: Enum a => AnimatedModelRenderer -> a
currentAnimation = toEnum . currentAnim


-- | Get the renderer's model resource.
modelRes :: AnimatedModelRenderer -> AnimatedModelResource
modelRes = modelResource


-- | Get the renderer's next frame.
nextFrame :: AnimatedModelRenderer -> Int
nextFrame rend =
    let curFrame = currentFrame rend
    in
        if curFrame == frameEnd rend
        then frameStart rend
        else curFrame + 1


-- | Set the active animation to the given one.
setAnimation :: Enum a => a -> AnimatedModelRenderer -> AnimatedModelRenderer
setAnimation anim modelRend =
    let (Animation _ f1 f2) = animation (model . modelResource $ modelRend) anim'
        anim' = fromEnum anim
    in
        modelRend { currentAnim = anim', frameStart = f1, frameEnd = f2, currentFrame = f1 }


-- | Set the renderer's animation speed.
setAnimationSpeed :: AnimationSpeed -> AnimatedModelRenderer -> AnimatedModelRenderer
setAnimationSpeed s r = r { animationSpeed = s }


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
render uniforms (AnimatedModelRenderer model _ _ _ curFrame fp _) =
    let n = nVertices model
        (Material _ ka kd ks shi) = material model
    in do
        uniformVec4 (kaLoc uniforms) ka
        uniformVec4 (kdLoc uniforms) kd
        uniformVec4 (ksLoc uniforms) ks
        glUniform1f (shiLoc uniforms) $ unsafeCoerce shi
        glUniform1f (fpLoc uniforms) (unsafeCoerce fp)
        drawArrays gl_TRIANGLES (n*curFrame) n
