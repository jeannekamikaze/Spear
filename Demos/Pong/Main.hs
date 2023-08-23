module Main where

import           Data.Maybe                              (mapMaybe)
import           Graphics.Rendering.OpenGL.GL            (($=))
import qualified Graphics.Rendering.OpenGL.GL            as GL
import           Graphics.Rendering.OpenGL.GL.VertexSpec (currentColor)
import           Pong
import           Spear.App
import           Spear.Game
import           Spear.Math.AABB
import           Spear.Math.Spatial2
import           Spear.Math.Vector
import           Spear.Window

data GameState = GameState
  { window :: Window,
    world  :: [GameObject]
  }

main =
  withWindow (900, 600) (2, 0) (Just "Pong") initGame $
    loop step

initGame :: Window -> Game () GameState
initGame window = return $ GameState window newWorld

step :: Elapsed -> Dt -> [InputEvent] -> Game GameState Bool
step elapsed dt inputEvents = do
  gs <- getGameState
  gameIO . process $ inputEvents
  let events = translate inputEvents
  modifyGameState $ \gs ->
    gs
      { world = stepWorld elapsed dt events (world gs)
      }
  getGameState >>= \gs -> gameIO . render $ world gs
  return (not $ exitRequested inputEvents)

render world = do
  -- Clear the background to a different colour than the playable area to make
  -- the latter distinguishable.
  GL.clearColor $= GL.Color4 0.2 0.2 0.2 0.0
  GL.clear [GL.ColorBuffer]
  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  renderBackground
  -- Draw objects.
  GL.currentColor $= GL.Color4 1.0 1.0 1.0 1.0
  mapM_ renderGO world

renderBackground :: IO ()
renderBackground =
  let pmin = 0 :: Float
      pmax = 1 :: Float
  in do
    GL.currentColor $= GL.Color4 0.7 0.5 0.7 1.0
    GL.renderPrimitive GL.TriangleStrip $ do
        GL.vertex (GL.Vertex2 pmin pmax)
        GL.vertex (GL.Vertex2 pmin pmin)
        GL.vertex (GL.Vertex2 pmax pmax)
        GL.vertex (GL.Vertex2 pmax pmin)

renderGO :: GameObject -> IO ()
renderGO go = do
  let (AABB2 (Vector2 xmin' ymin') (Vector2 xmax' ymax')) = aabb go
      (Vector2 xcenter ycenter) = pos go
      (xmin, ymin, xmax, ymax) = (f2d xmin', f2d ymin', f2d xmax', f2d ymax')
  GL.preservingMatrix $ do
    GL.translate (GL.Vector3 (f2d xcenter) (f2d ycenter) 0)
    GL.renderPrimitive GL.TriangleStrip $ do
      GL.vertex (GL.Vertex2 xmin ymax)
      GL.vertex (GL.Vertex2 xmin ymin)
      GL.vertex (GL.Vertex2 xmax ymax)
      GL.vertex (GL.Vertex2 xmax ymin)

process = mapM_ procEvent

procEvent (Resize w h) =
  let r = (fromIntegral w) / (fromIntegral h)
      pad    = if r > 1 then (r-1) / 2 else (1/r - 1) / 2
      left   = if r > 1 then -pad else 0
      right  = if r > 1 then 1 + pad else 1
      bottom = if r > 1 then 0 else -pad
      top    = if r > 1 then 1 else 1 + pad
  in do
    GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
    GL.matrixMode $= GL.Projection
    GL.loadIdentity
    GL.ortho left right bottom top (-1) 1
    GL.matrixMode $= GL.Modelview 0
procEvent _ = return ()

translate = mapMaybe translate'

translate' (KeyDown KEY_LEFT)  = Just MoveLeft
translate' (KeyDown KEY_RIGHT) = Just MoveRight
translate' (KeyUp KEY_LEFT)    = Just StopLeft
translate' (KeyUp KEY_RIGHT)   = Just StopRight
translate' _                   = Nothing

exitRequested = elem (KeyDown KEY_ESC)

f2d :: Float -> GL.GLdouble
f2d = realToFrac
