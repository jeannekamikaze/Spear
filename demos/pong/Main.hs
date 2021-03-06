module Main where

import Pong

import Spear.Math.AABB
import Spear.Math.Spatial2
import Spear.Math.Vector
import Spear.Game
import Spear.Window

import Data.Maybe (mapMaybe)
import qualified Graphics.Rendering.OpenGL.GL as GL
import Graphics.Rendering.OpenGL.GL (($=))

data GameState = GameState
     { wnd     :: Window
     , world   :: [GameObject]
     }

main = run
     $ withWindow (640,480) [] Window (2,0) (Just "Pong") initGame
     $ loop (Just 30) step

initGame wnd = do
         gameIO $ do
                GL.clearColor $= GL.Color4 0.7 0.5 0.7 1.0
                GL.matrixMode $= GL.Modelview 0
                GL.loadIdentity
         return $ GameState wnd newWorld

step :: Elapsed -> Dt -> Game GameState Bool
step elapsed dt = do
     gs <- getGameState
     evts <- events (wnd gs)
     gameIO . process $ evts
     let evts' = translate evts
     modifyGameState $ \ gs -> gs
                     { world = stepWorld elapsed dt evts' (world gs) }
     getGameState >>= \gs -> gameIO . render $ world gs
     return (not $ exitRequested evts)

render world = do
       GL.clear [GL.ColorBuffer]
       mapM_ renderGO world
       swapBuffers

renderGO :: GameObject -> IO ()
renderGO go = do
         let (AABB2 (Vector2 xmin' ymin') (Vector2 xmax' ymax')) = aabb go
             (Vector2 xcenter ycenter) = pos go
             (xmin,ymin,xmax,ymax) = (f2d xmin', f2d ymin', f2d xmax', f2d ymax')
         GL.preservingMatrix $ do
            GL.translate (GL.Vector3 (f2d xcenter) (f2d ycenter) 0)
            GL.renderPrimitive (GL.TriangleStrip) $ do
               GL.vertex (GL.Vertex2 xmin ymax)
               GL.vertex (GL.Vertex2 xmin ymin)
               GL.vertex (GL.Vertex2 xmax ymax)
               GL.vertex (GL.Vertex2 xmax ymin)

process = mapM_ procEvent
procEvent (Resize w h) = do
          GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
          GL.matrixMode $= GL.Projection
          GL.loadIdentity
          GL.ortho 0 1 0 1 (-1) 1
          GL.matrixMode $= GL.Modelview 0
procEvent _ = return ()

translate = mapMaybe translate'
translate' (KeyDown KEY_LEFT)  = Just MoveLeft
translate' (KeyDown KEY_RIGHT) = Just MoveRight
translate' (KeyUp   KEY_LEFT)  = Just StopLeft
translate' (KeyUp   KEY_RIGHT) = Just StopRight
translate' _ = Nothing

exitRequested = any (==(KeyDown KEY_ESC))

f2d :: Float -> GL.GLdouble
f2d = realToFrac
