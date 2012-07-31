module Spear.Render.Box
(
	render
,	renderOutwards
,	renderInwards
,	renderEdges
)
where


import Spear.Math.Vector3
import Spear.Math.Matrix
import Graphics.Rendering.OpenGL.Raw
import Unsafe.Coerce
import Control.Monad.Instances

type Center = Vector3
type Colour = Vector4
type Length = Float
type Normals = [Vector3]
type GenerateTexCoords = Bool


applyColour :: Colour -> IO ()
--applyColour col = glColor4f (unsafeCoerce $ x col) (unsafeCoerce $ y col) (unsafeCoerce $ z col) (unsafeCoerce $ w col)
applyColour = do
    ax <- unsafeCoerce . x
    ay <- unsafeCoerce . y
    az <- unsafeCoerce . z
    aw <- unsafeCoerce . w
    glColor4f ax ay az aw


applyNormal :: Vector3 -> IO ()
--applyNormal v = glNormal3f (unsafeCoerce $ x v) (unsafeCoerce $ y v) (unsafeCoerce $ z v)
applyNormal = do
    nx <- unsafeCoerce . x
    ny <- unsafeCoerce . y
    nz <- unsafeCoerce . z
    glNormal3f nx ny nz


-- | Renders a box.
render :: Center  -- ^ The box's center.
       -> Length  -- ^ The perpendicular distance from the box's center to any of its sides.
       -> Colour  -- ^ The box's colour.
       -> Normals -- ^ The box's normals, of the form [front, back, right, left, top, bottom].
       -> IO ()
render c l col normals = do
	glPushMatrix
	glTranslatef (unsafeCoerce $ x c) (unsafeCoerce $ y c) (unsafeCoerce $ z c)
	applyColour col
	
	let d = unsafeCoerce l
	glBegin gl_QUADS
	
       --Front
	--glNormal3f 0 0 (-1)
	applyNormal $ normals !! 0
	glVertex3f	  d	(-d)	(-d)
	glVertex3f	  d	d	(-d)
	glVertex3f	(-d)	d	(-d)
	glVertex3f	(-d)	(-d)	(-d)
	
	--Back
	--glNormal3f 0 0 1
	applyNormal $ normals !! 1
	glVertex3f	(-d)	(-d)	d
	glVertex3f	(-d)	d	d
	glVertex3f	d	d	d
	glVertex3f	d	(-d)	d
	
	--Right
	--glNormal3f 1 0 0
	applyNormal $ normals !! 2
	glVertex3f	d	(-d)	(-d)
	glVertex3f	d	(-d)	d
	glVertex3f	d	d	d
	glVertex3f	d	d	(-d)
	
	--Left
	--glNormal3f (-1) 0 0
	applyNormal $ normals !! 3
	glVertex3f	(-d)	(-d)	(-d)
	glVertex3f	(-d)	d	(-d)
	glVertex3f	(-d)	d	d
	glVertex3f	(-d)	(-d)	d
	
	--Top
	--glNormal3f 0 1 0
	applyNormal $ normals !! 4
	glVertex3f	(-d)	d	(-d)
	glVertex3f	d	d	(-d)
	glVertex3f	d	d	d
	glVertex3f	(-d)	d	d
	
	--Bottom
	--glNormal3f 0 (-1) 0
	applyNormal $ normals !! 5
	glVertex3f	d	(-d)	d
	glVertex3f	d	(-d)	(-d)
	glVertex3f	(-d)	(-d)	(-d)
	glVertex3f	(-d)	(-d)	d
	
	glEnd
	
	glPopMatrix
	
	
normals = [vec3 0 0 (-1), vec3 0 0 1, vec3 1 0 0, vec3 (-1) 0 0, vec3 0 1 0, vec3 0 (-1) 0]


-- | Renders a box with normals facing outwards.
renderOutwards :: Center  -- ^ The box's center.
                -> Length -- ^ The perpendicular distance from the box's center to any of its sides.
                -> Colour -- ^ The box's colour.
                -> IO ()
renderOutwards c l col = render c l col normals
	
	
-- | Renders a box with normals facing inwards.
renderInwards :: Center  -- ^ The box's center.
              -> Length -- ^ The perpendicular distance from the box's center to any of its sides.
              -> Colour -- ^ The box's colour.
              -> IO ()
renderInwards c l col = do
	glFrontFace gl_CW
	render c l col $ Prelude.map neg normals
	glFrontFace gl_CCW
	
	
renderEdges :: Center -- ^ The box's center.
            -> Length -- ^ The perpendicular distance from the box's center to any of its sides.
            -> Colour -- ^ The box's colour.
            -> IO ()
renderEdges c l col = do
	glPushMatrix
	glTranslatef (unsafeCoerce $ x c) (unsafeCoerce $ y c) (unsafeCoerce $ z c)
	applyColour col
	
	let d = unsafeCoerce l
	
	--Front
	glBegin gl_LINE_STRIP
	glVertex3f	  d	(-d)	(-d)
	glVertex3f	  d	d	(-d)
	glVertex3f	(-d)	d	(-d)
	glVertex3f	(-d)	(-d)	(-d)
	glEnd
	
	--Back
	glBegin gl_LINE_STRIP
	glVertex3f	(-d)	(-d)	d
	glVertex3f	(-d)	d	d
	glVertex3f	d	d	d
	glVertex3f	d	(-d)	d
	glVertex3f	(-d)	(-d)	d
	glEnd
	
	--Right
	glBegin gl_LINE_STRIP
	glVertex3f	d	(-d)	(-d)
	glVertex3f	d	(-d)	d
	glVertex3f	d	d	d
	glVertex3f	d	d	(-d)
	glEnd
	
	--Left
	glBegin gl_LINE_STRIP
	glVertex3f	(-d)	(-d)	(-d)
	glVertex3f	(-d)	d	(-d)
	glVertex3f	(-d)	d	d
	glVertex3f	(-d)	(-d)	d
	glEnd
	
	--Top
	glBegin gl_LINE_STRIP
	glVertex3f	(-d)	d	(-d)
	glVertex3f	d	d	(-d)
	glVertex3f	d	d	d
	glVertex3f	(-d)	d	d
	glEnd
	
	--Bottom
	glBegin gl_LINE_STRIP
	glVertex3f	d	(-d)	d
	glVertex3f	d	(-d)	(-d)
	glVertex3f	(-d)	(-d)	(-d)
	glVertex3f	(-d)	(-d)	d
	glEnd
	
	glPopMatrix
	