{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Spear.Assets.Model
(
    -- * Data types
    Vec2(..)
,   Vec3(..)
,   TexCoord(..)
,   CTriangle(..)
,   Box(..)
,   Skin(..)
,   Animation(..)
,   Triangle(..)
,   Model(..)
    -- * Loading
,   loadModel
    -- * Accessors
,   animated
,   animation
,   animationByName
,   triangles'
    -- * Manipulation
,   transformVerts
,   transformNormals
,   toGround
,   modelBoxes
)
where


import Spear.Setup


import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (splitAt, elemIndex)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils as Foreign (with)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (allocaArray, copyArray, peekArray)
import Unsafe.Coerce (unsafeCoerce)


#include "Model.h"
#include "MD2/MD2_load.h"
#include "OBJ/OBJ_load.h"


data ModelErrorCode
    = ModelSuccess
    | ModelReadError
    | ModelMemoryAllocationError
    | ModelFileNotFound
    | ModelFileMismatch
    | ModelNoSuitableLoader
    deriving (Eq, Enum, Show)


sizeFloat = #{size float}
sizePtr   = #{size int*}


-- | A 2D vector.
data Vec2 = Vec2 {-# UNPACK #-} !Float {-# UNPACK #-} !Float


instance Storable Vec2 where
    sizeOf _ = 2*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        f0 <- peekByteOff ptr 0
        f1 <- peekByteOff ptr sizeFloat
        return $ Vec2 f0 f1
    
    poke ptr (Vec2 f0 f1) = do
        pokeByteOff ptr 0         f0
        pokeByteOff ptr sizeFloat f1


-- | A 3D vector.
data Vec3 = Vec3 {-# UNPACK #-} !Float {-# UNPACK #-} !Float {-# UNPACK #-} !Float


instance Storable Vec3 where
    sizeOf _ = 3*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        f0 <- peekByteOff ptr 0
        f1 <- peekByteOff ptr sizeFloat
        f2 <- peekByteOff ptr (2*sizeFloat)
        return $ Vec3 f0 f1 f2
    
    poke ptr (Vec3 f0 f1 f2) = do
        pokeByteOff ptr 0             f0
        pokeByteOff ptr sizeFloat     f1
        pokeByteOff ptr (2*sizeFloat) f2


-- | A 2D texture coordinate.
data TexCoord = TexCoord {-# UNPACK #-} !Float {-# UNPACK #-} !Float


instance Storable TexCoord where
    sizeOf _ = 2*sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        f0 <- peekByteOff ptr 0
        f1 <- peekByteOff ptr sizeFloat
        return $ TexCoord f0 f1
    
    poke ptr (TexCoord f0 f1) = do
        pokeByteOff ptr 0         f0
        pokeByteOff ptr sizeFloat f1


-- | A raw triangle holding vertex/normal and texture indices.
data CTriangle = CTriangle
    { vertexIndex0  :: {-# UNPACK #-} !CUShort
    , vertexIndex1  :: {-# UNPACK #-} !CUShort
    , vertexIndex2  :: {-# UNPACK #-} !CUShort
    , textureIndex1 :: {-# UNPACK #-} !CUShort
    , textureIndex2 :: {-# UNPACK #-} !CUShort
    , textureIndex3 :: {-# UNPACK #-} !CUShort
    }


instance Storable CTriangle where
    sizeOf _ = #{size triangle}
    alignment _ = alignment (undefined :: CUShort)
    
    peek ptr = do
        v0 <- #{peek triangle, vertexIndices[0]} ptr
        v1 <- #{peek triangle, vertexIndices[1]} ptr
        v2 <- #{peek triangle, vertexIndices[2]} ptr
        
        t0 <- #{peek triangle, textureIndices[0]} ptr
        t1 <- #{peek triangle, textureIndices[1]} ptr
        t2 <- #{peek triangle, textureIndices[2]} ptr
        
        return $ CTriangle v0 v1 v2 t0 t1 t2
    
    poke ptr (CTriangle v0 v1 v2 t0 t1 t2) = do
        #{poke triangle, vertexIndices[0]} ptr v0
        #{poke triangle, vertexIndices[1]} ptr v1
        #{poke triangle, vertexIndices[2]} ptr v2
        
        #{poke triangle, textureIndices[0]} ptr t0
        #{poke triangle, textureIndices[1]} ptr t1
        #{poke triangle, textureIndices[2]} ptr t2


-- | A 2D axis-aligned bounding box.
data Box = Box {-# UNPACK #-} !Vec2 {-# UNPACK #-} !Vec2


instance Storable Box where
    sizeOf _ = 4 * sizeFloat
    alignment _ = alignment (undefined :: CFloat)
    
    peek ptr = do
        f0 <- peekByteOff ptr 0
        f1 <- peekByteOff ptr sizeFloat
        f2 <- peekByteOff ptr $ 2*sizeFloat
        f3 <- peekByteOff ptr $ 3*sizeFloat
        return $ Box (Vec2 f0 f1) (Vec2 f2 f3)
    
    poke ptr (Box (Vec2 f0 f1) (Vec2 f2 f3)) = do
        pokeByteOff ptr 0             f0
        pokeByteOff ptr sizeFloat     f1
        pokeByteOff ptr (2*sizeFloat) f2
        pokeByteOff ptr (3*sizeFloat) f3


-- | A model skin.
newtype Skin = Skin { skinName :: B.ByteString }


instance Storable Skin where
    sizeOf (Skin s) = 64
    alignment _ = 1
    
    peek ptr = do
        s <- B.packCString $ unsafeCoerce ptr
        return $ Skin s
    
    poke ptr (Skin s) = do
        B.useAsCStringLen s $ \(sptr, len) -> copyArray (unsafeCoerce ptr) sptr len


-- | A model animation.
--
-- See also: 'animation', 'animationByName', 'numAnimations'.
data Animation = Animation
    { name  :: B.ByteString
    , start :: Int
    , end   :: Int
    }


instance Storable Animation where
    sizeOf _    = #{size animation}
    alignment _ = alignment (undefined :: CUInt)
    
    peek ptr = do
        name  <- B.packCString (unsafeCoerce ptr)
        start <- #{peek animation, start} ptr
        end   <- #{peek animation, end}   ptr
        return $ Animation name start end
    
    poke ptr (Animation name start end) = do
        B.useAsCStringLen name $ \(sptr, len) -> copyArray (unsafeCoerce ptr) sptr len
        #{poke animation, start} ptr start
        #{poke animation, end}   ptr end


-- | A 3D model.
data Model = Model
    { vertices      :: S.Vector Vec3       -- ^ Array of 'numFrames' * 'numVerts' vertices.
    , normals       :: S.Vector Vec3       -- ^ Array of 'numFrames' * 'numVerts' normals.
    , texCoords     :: S.Vector TexCoord   -- ^ Array of 'numTexCoords' texture coordinates.
    , triangles     :: S.Vector CTriangle  -- ^ Array of 'numTriangles' triangles.
    , skins         :: S.Vector Skin       -- ^ Array of 'numSkins' skins.
    , animations    :: S.Vector Animation  -- ^ Array of 'numAnimations' animations.
    , numFrames     :: Int                 -- ^ Number of frames.
    , numVerts      :: Int                 -- ^ Number of vertices (and normals) per frame.
    , numTriangles  :: Int                 -- ^ Number of triangles in one frame.
    , numTexCoords  :: Int                 -- ^ Number of texture coordinates in one frame.
    , numSkins      :: Int                 -- ^ Number of skins.
    , numAnimations :: Int                 -- ^ Number of animations.
    }


instance Storable Model where
    sizeOf _    = #{size Model}
    alignment _ = alignment (undefined :: CUInt)
    
    peek ptr = do
        numFrames     <- #{peek Model, numFrames}     ptr
        numVertices   <- #{peek Model, numVertices}   ptr
        numTriangles  <- #{peek Model, numTriangles}  ptr
        numTexCoords  <- #{peek Model, numTexCoords}  ptr
        numSkins      <- #{peek Model, numSkins}      ptr
        numAnimations <- #{peek Model, numAnimations} ptr
        pVerts        <- peek (unsafeCoerce ptr) 
        pNormals      <- peekByteOff ptr sizePtr
        pTexCoords    <- peekByteOff ptr (2*sizePtr)
        pTriangles    <- peekByteOff ptr (3*sizePtr)
        pSkins        <- peekByteOff ptr (4*sizePtr)
        pAnimations   <- peekByteOff ptr (5*sizePtr)
        vertices      <- fmap S.fromList $ peekArray (numVertices*numFrames) pVerts
        normals       <- fmap S.fromList $ peekArray (numVertices*numFrames) pNormals
        texCoords     <- fmap S.fromList $ peekArray numTexCoords            pTexCoords
        triangles     <- fmap S.fromList $ peekArray numTriangles            pTriangles
        skins         <- fmap S.fromList $ peekArray numSkins                pSkins
        animations    <- fmap S.fromList $ peekArray numAnimations           pAnimations
        return $
            Model vertices normals texCoords triangles skins animations
                  numFrames numVertices numTriangles numTexCoords numSkins numAnimations
    
    poke ptr
        (Model verts normals texCoords tris skins animations
         numFrames numVerts numTris numTex numSkins numAnimations) =
            S.unsafeWith verts $ \pVerts ->
            S.unsafeWith normals $ \pNormals ->
            S.unsafeWith texCoords $ \pTexCoords ->
            S.unsafeWith tris $ \pTris ->
            S.unsafeWith skins $ \pSkins ->
            S.unsafeWith animations $ \pAnimations -> do
                #{poke Model, vertices}      ptr pVerts
                #{poke Model, normals}       ptr pNormals
                #{poke Model, texCoords}     ptr pTexCoords
                #{poke Model, triangles}     ptr pTris
                #{poke Model, skins}         ptr pSkins
                #{poke Model, animations}    ptr pAnimations
                #{poke Model, numFrames}     ptr numFrames
                #{poke Model, numVertices}   ptr numVerts
                #{poke Model, numTriangles}  ptr numTris
                #{poke Model, numTexCoords}  ptr numTex
                #{poke Model, numSkins}      ptr numSkins
                #{poke Model, numAnimations} ptr numAnimations


-- | A model triangle.
--
-- See also: 'triangles''.
data Triangle = Triangle
    { v0 :: Vec3
    , v1 :: Vec3
    , v2 :: Vec3
    , n0 :: Vec3
    , n1 :: Vec3
    , n2 :: Vec3
    , t0 :: TexCoord
    , t1 :: TexCoord
    , t2 :: TexCoord
    }


instance Storable Triangle where
    sizeOf _ = #{size model_triangle}
    alignment _ = alignment (undefined :: Float)
    
    peek ptr = do
        v0 <- #{peek model_triangle, v0} ptr
        v1 <- #{peek model_triangle, v1} ptr
        v2 <- #{peek model_triangle, v2} ptr
        n0 <- #{peek model_triangle, n0} ptr
        n1 <- #{peek model_triangle, n1} ptr
        n2 <- #{peek model_triangle, n2} ptr
        t0 <- #{peek model_triangle, t0} ptr
        t1 <- #{peek model_triangle, t1} ptr
        t2 <- #{peek model_triangle, t2} ptr
        return $ Triangle v0 v1 v2 n0 n1 n2 t0 t1 t2
    
    poke ptr (Triangle v0 v1 v2 n0 n1 n2 t0 t1 t2) = do
        #{poke model_triangle, v0} ptr v0
        #{poke model_triangle, v1} ptr v1
        #{poke model_triangle, v2} ptr v2
        #{poke model_triangle, n0} ptr n0
        #{poke model_triangle, n1} ptr n1
        #{poke model_triangle, n2} ptr n2
        #{poke model_triangle, t0} ptr t0
        #{poke model_triangle, t1} ptr t1
        #{poke model_triangle, t2} ptr t2


foreign import ccall "Model.h model_free"
    model_free :: Ptr Model -> IO ()


foreign import ccall "MD2_load.h MD2_load"
    md2_load' :: Ptr CChar -> CChar -> CChar -> Ptr Model -> IO Int


foreign import ccall "OBJ_load.h OBJ_load"
    obj_load' :: Ptr CChar -> CChar -> CChar -> Ptr Model -> IO Int


md2_load :: Ptr CChar -> CChar -> CChar -> Ptr Model -> IO ModelErrorCode
md2_load file clockwise leftHanded model =
    md2_load' file clockwise leftHanded model >>= \code -> return . toEnum $ code


obj_load :: Ptr CChar -> CChar -> CChar -> Ptr Model -> IO ModelErrorCode
obj_load file clockwise leftHanded model =
    obj_load' file clockwise leftHanded model >>= \code -> return . toEnum $ code


-- | Load the model specified by the given file.
loadModel :: FilePath -> Setup Model
loadModel file = do
    dotPos <- case elemIndex '.' file of
            Nothing -> setupError $ "file name has no extension: " ++ file
            Just p  -> return p
    
    let ext = map toLower . tail . snd $ splitAt dotPos file
    
    result <- setupIO . alloca $ \ptr -> do
        status <- withCString file $ \fileCstr -> do
            case ext of
                "md2" -> md2_load fileCstr 0 0 ptr
                "obj" -> obj_load fileCstr 0 0 ptr
                _     -> return ModelNoSuitableLoader
        
        case status of
            ModelSuccess -> do
                model <- peek ptr
                model_free ptr
                return . Right $ model
            ModelReadError              -> return . Left $ "read error"
            ModelMemoryAllocationError  -> return . Left $ "memory allocation error"
            ModelFileNotFound           -> return . Left $ "file not found"
            ModelFileMismatch           -> return . Left $ "file mismatch"
            ModelNoSuitableLoader       -> return . Left $ "no suitable loader for extension " ++ ext
    
    case result of
        Right model -> return model
        Left err    -> setupError $ "loadModel: " ++ err


-- | Return 'True' if the model is animated, 'False' otherwise.
animated :: Model -> Bool
animated = (>1) . numFrames


-- | Return the model's ith animation.
animation :: Model -> Int -> Animation
animation model i = animations model S.! i


-- | Return the animation specified by the given string.
animationByName :: Model -> String -> Maybe Animation
animationByName model anim =
    let anim' = B.pack anim in S.find ((==) anim' . name) $ animations model


-- | Return a copy of the model's triangles.
triangles' :: Model -> IO [Triangle]
triangles' model =
    let n = numVerts model * numFrames model
    in  with model $ \modelPtr ->
        allocaArray n $ \arrayPtr -> do
            model_copy_triangles modelPtr arrayPtr
            tris <- peekArray n arrayPtr
            return tris


foreign import ccall "Model.h model_copy_triangles"
    model_copy_triangles :: Ptr Model -> Ptr Triangle -> IO ()


-- | Transform the model's vertices.
transformVerts :: Model -> (Vec3 -> Vec3) -> Model
transformVerts model f = model { vertices = vertices' }
    where
        n = numVerts model * numFrames model
        vertices' = S.generate n f'
        f' i = f $ vertices model S.! i 


-- | Transform the model's normals.
transformNormals :: Model -> (Vec3 -> Vec3) -> Model
transformNormals model f = model { normals = normals' }
    where
        n = numVerts model * numFrames model
        normals' = S.generate n f'
        f' i = f $ normals model S.! i 


-- | Translate the model such that its lowest point has y = 0.
toGround :: Model -> IO Model
toGround model =
    let model' = model { vertices = S.generate n $ \i -> vertices model S.! i }
        n = numVerts model * numFrames model
    in    
        with model' model_to_ground >> return model'


foreign import ccall "Model.h model_to_ground"
    model_to_ground :: Ptr Model -> IO ()


-- | Get the model's 2D bounding boxes.
modelBoxes :: Model -> IO (V.Vector Box)
modelBoxes model =
    with model $ \modelPtr ->
    allocaArray (numVerts model * numFrames model) $ \pointsPtr -> do
    model_compute_boxes modelPtr pointsPtr
    let n = numFrames model
        getBoxes = peekBoxes pointsPtr n 0 0 $ return []
        peekBoxes ptr n cur off l
            | cur == n = l
            | otherwise = do
                f0 <- peekByteOff ptr off
                f1 <- peekByteOff ptr $ off + sizeFloat
                f2 <- peekByteOff ptr $ off + 2*sizeFloat
                f3 <- peekByteOff ptr $ off + 3*sizeFloat
                peekBoxes ptr n (cur+1) (off + 4*sizeFloat) $
                    fmap ((f3:) . (f2:) . (f1:) . (f0:)) l
    fmap (V.fromList . reverse) getBoxes
    


foreign import ccall "Model.h model_compute_boxes"
    model_compute_boxes :: Ptr Model -> Ptr Vec2 -> IO ()
