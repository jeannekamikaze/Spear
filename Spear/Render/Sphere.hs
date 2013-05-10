module Spear.Render.Sphere
(
	render
)
where


import Spear.Math.Vector as Vector
import Spear.Math.Matrix
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.OpenGL.GL.Colors
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import Unsafe.Coerce


type Center = Vector R
type Radius = R
type Colour = Vector R


applyColour :: Colour -> IO ()
applyColour col =
	if Vector.length col == 4 then
		glColor4f (unsafeCoerce $ x col) (unsafeCoerce $ y col) (unsafeCoerce $ z col)
			  (unsafeCoerce $ w col)
	else
		glColor3f (unsafeCoerce $ x col) (unsafeCoerce $ y col) (unsafeCoerce $ z col)


-- | Renders a sphere.
-- Center is the sphere's center.
-- Radius is the sphere's radius.
-- Colour is a Vector representing the sphere's colour. Colour may hold an alpha channel.
render :: Center -> Radius -> Colour -> IO ()
render c radius col = do
	glPushMatrix
	glTranslatef (unsafeCoerce $ x c) (unsafeCoerce $ y c) (unsafeCoerce $ z c)
	applyColour col
	
	let r = unsafeCoerce $ (realToFrac radius :: Double)
	let style = GLU.QuadricStyle (Just Smooth) GLU.NoTextureCoordinates GLU.Outside GLU.FillStyle
	GLU.renderQuadric style $ GLU.Sphere r 16 16
	
	glPopMatrix
	