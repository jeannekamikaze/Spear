module Spear.Render.StaticModel
  ( -- * Data types
    StaticModelResource,
    StaticModelRenderer,

    -- * Construction and destruction
    staticModelResource,
    staticModelRenderer,

    -- * Manipulation
    box,
    modelRes,

    -- * Rendering
    bind,
    render,

    -- * Collision
    mkColsFromStatic,
  )
where

import           Spear.Assets.Model
import           Spear.Game
import           Spear.GL
import           Spear.Math.AABB
import           Spear.Math.Algebra
import           Spear.Math.Collision
import           Spear.Math.Matrix4    (Matrix4)
import           Spear.Math.Vector
import           Spear.Render.Material
import           Spear.Render.Model
import           Spear.Render.Program

import qualified Data.Vector           as V
import           Foreign.C.Types
import           Prelude               hiding ((*))
import           Unsafe.Coerce         (unsafeCoerce)


data StaticModelResource = StaticModelResource
  { vao       :: VAO,
    nVertices :: Int,
    material  :: Material,
    texture   :: Texture,
    boxes     :: V.Vector Box,
    rkey      :: Resource
  }

instance Eq StaticModelResource where
  m1 == m2 = vao m1 == vao m2

instance Ord StaticModelResource where
  m1 < m2 = vao m1 < vao m2
  m1 <= m2 = vao m1 <= vao m2

instance ResourceClass StaticModelResource where
  getResource = rkey

data StaticModelRenderer = StaticModelRenderer {model :: StaticModelResource}

instance Eq StaticModelRenderer where
  m1 == m2 = model m1 == model m2

instance Ord StaticModelRenderer where
  m1 < m2 = model m1 < model m2
  m1 <= m2 = model m1 <= model m2

-- | Create a model resource from the given model.
staticModelResource ::
  StaticProgramChannels ->
  Material ->
  Texture ->
  Model ->
  Game s StaticModelResource
staticModelResource (StaticProgramChannels vertChan normChan texChan) material texture model = do
  RenderModel elements _ numVertices <- gameIO . renderModelFromModel $ model
  elementBuf <- newBuffer
  vao <- newVAO
  boxes <- gameIO $ modelBoxes model

  gameIO $ do
    let elemSize = 32::CUInt
        elemSize' = fromIntegral elemSize
        n = numVertices

    bindVAO vao

    bindBuffer ArrayBuffer elementBuf
    bufferData' ArrayBuffer (fromIntegral $ elemSize * n) elements StaticDraw

    attribVAOPointer vertChan 3 GL_FLOAT False elemSize' 0
    attribVAOPointer normChan 3 GL_FLOAT False elemSize' 12
    attribVAOPointer texChan 2 GL_FLOAT False elemSize' 24

    enableVAOAttrib vertChan
    enableVAOAttrib normChan
    enableVAOAttrib texChan

  rkey <- register $ do
    putStrLn "Releasing static model resource"
    clean vao
    clean elementBuf

  return $
    StaticModelResource
      vao
      (unsafeCoerce numVertices)
      material
      texture
      boxes
      rkey

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
        activeTexture $= GL_TEXTURE0
        glUniform1i texLoc 0

-- | Render the given renderer.
render :: StaticProgramUniforms -> StaticModelRenderer -> IO ()
render uniforms (StaticModelRenderer model) =
  let (Material _ ka kd ks shi) = material model
   in do
        uniform (kaLoc uniforms) ka
        uniform (kdLoc uniforms) kd
        uniform (ksLoc uniforms) ks
        glUniform1f (shiLoc uniforms) $ unsafeCoerce shi
        drawArrays GL_TRIANGLES 0 $ nVertices model

-- | Compute AABB collisioners in view space from the given model.
mkColsFromStatic ::
  -- | Modelview matrix
  Matrix4 ->
  StaticModelResource ->
  [Collisioner2]
mkColsFromStatic modelview modelRes = mkCols modelview (box 0 modelRes)
