{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Spear.Assets.Model
(
    -- * Data types
    ModelErrorCode
,   Vec3
,   TexCoord
,   CModel(..)
,   Animation(..)
,   Model
    -- * Loading and unloading
,   loadModel
,   releaseModel
    -- * Accessors
,   animated
,   vertices
,   normals
,   texCoords
,   triangles
,   skins
,   numFrames
,   numVertices
,   numTriangles
,   numTexCoords
,   numSkins
,   cmodel
,   animation
,   animationByName
,   numAnimations
    -- * Manipulation
,   transform
,   toGround
)
where


import Spear.Setup
import qualified Spear.Math.Matrix4 as M4
import qualified Spear.Math.Matrix3 as M3
import Spear.Math.MatrixUtils

import qualified Data.ByteString.Char8 as B
import Data.Char (toLower)
import Data.List (splitAt, elemIndex)
import qualified Data.Vector as V
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Utils as Foreign (with)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (copyArray, peekArray)
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


data Vec3 = Vec3 !CFloat !CFloat !CFloat

data TexCoord = TexCoord !CFloat !CFloat

data Triangle = Triangle !CUShort !CUShort !CUShort !CUShort !CUShort !CUShort

data Skin = Skin !(Ptr Char)

data CAnimation = CAnimation !B.ByteString !CUInt !CUInt


-- | The model's underlying representation.
data CModel = CModel
    { cVerts       :: Ptr Vec3       -- ^ Pointer to an array of 'cnFrames' * 'cnVerts' vertices.
    , cNormals     :: Ptr Vec3       -- ^ Pointer to an array of 'cnFrames' * cnVerts normals.
    , cTexCoords   :: Ptr TexCoord   -- ^ Pointer to an array of 'cnTris' texture coordinates.
    , cTris        :: Ptr Triangle   -- ^ Pointer to an array of 'cnTris' triangles.
    , cSkins       :: Ptr Skin       -- ^ Pointer to an array of 'cnSkins' skins.
    , cAnimations  :: Ptr CAnimation -- ^ Pointer to an array of 'cnAnimations' animations.
    , cnFrames     :: CUInt          -- ^ Number of frames.
    , cnVerts      :: CUInt          -- ^ Number of vertices per frame.
    , cnTris       :: CUInt          -- ^ Number of triangles in one frame.
    , cnTexCoords  :: CUInt          -- ^ Number of texture coordinates in one frame.
    , cnSkins      :: CUInt          -- ^ Number of skins.
    , cnAnimations :: CUInt          -- ^ Number of animations.
    }


instance Storable CModel where
    sizeOf _    = #{size Model}
    alignment _ = alignment (undefined :: CUInt)

    peek ptr = do
        vertices      <- #{peek Model, vertices}      ptr
        normals       <- #{peek Model, normals}       ptr
        texCoords     <- #{peek Model, texCoords}     ptr
        triangles     <- #{peek Model, triangles}     ptr
        skins         <- #{peek Model, skins}         ptr
        animations    <- #{peek Model, animations}    ptr
        numFrames     <- #{peek Model, numFrames}     ptr
        numVertices   <- #{peek Model, numVertices}   ptr
        numTriangles  <- #{peek Model, numTriangles}  ptr
        numTexCoords  <- #{peek Model, numTexCoords}  ptr
        numSkins      <- #{peek Model, numSkins}      ptr
        numAnimations <- #{peek Model, numAnimations} ptr
        return $
            CModel vertices normals texCoords triangles skins animations
                   numFrames numVertices numTriangles numTexCoords numSkins numAnimations

    poke ptr
        (CModel verts normals texCoords tris skins animations
         numFrames numVerts numTris numTex numSkins numAnimations) = do
            #{poke Model, vertices}      ptr verts
            #{poke Model, normals}       ptr normals
            #{poke Model, texCoords}     ptr texCoords
            #{poke Model, triangles}     ptr tris
            #{poke Model, skins}         ptr skins
            #{poke Model, animations}    ptr animations
            #{poke Model, numFrames}     ptr numFrames
            #{poke Model, numVertices}   ptr numVerts
            #{poke Model, numTriangles}  ptr numTris
            #{poke Model, numTexCoords}  ptr numTex
            #{poke Model, numSkins}      ptr numSkins
            #{poke Model, numAnimations} ptr numAnimations


-- data CAnimation = CAnimation !(Ptr CChar) !CUInt !CUInt
instance Storable CAnimation where
    sizeOf _    = #{size animation}
    alignment _ = alignment (undefined :: CUInt)

    peek ptr = do
        name  <- B.packCString (unsafeCoerce ptr)
        start <- #{peek animation, start} ptr
        end   <- #{peek animation, end}   ptr
        return $ CAnimation name start end

    poke ptr (CAnimation name start end) = do
        B.useAsCStringLen name $ \(sptr, len) -> copyArray (unsafeCoerce ptr) sptr len
        #{poke animation, start} ptr start
        #{poke animation, end}   ptr end


data Animation = Animation
    { name  :: String
    , start :: Int
    , end   :: Int
    }


-- | A model 'Resource'.
data Model = Model
    { modelData   :: CModel
    , mAnimations :: V.Vector Animation
    , rkey        :: Resource
    }


foreign import ccall "Model.h model_free"
    model_free :: Ptr CModel -> IO ()


foreign import ccall "MD2_load.h MD2_load"
    md2_load' :: Ptr CChar -> CChar -> CChar -> Ptr CModel -> IO Int


foreign import ccall "OBJ_load.h OBJ_load"
    obj_load' :: Ptr CChar -> CChar -> CChar -> Ptr CModel -> IO Int


md2_load :: Ptr CChar -> CChar -> CChar -> Ptr CModel -> IO ModelErrorCode
md2_load file clockwise leftHanded model =
    md2_load' file clockwise leftHanded model >>= \code -> return . toEnum $ code


obj_load :: Ptr CChar -> CChar -> CChar -> Ptr CModel -> IO ModelErrorCode
obj_load file clockwise leftHanded model =
    obj_load' file clockwise leftHanded model >>= \code -> return . toEnum $ code


-- | Load the model specified by the given 'FilePath'.
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
            ModelSuccess                -> peek ptr >>= return . Right
            ModelReadError              -> return . Left $ "read error"
            ModelMemoryAllocationError  -> return . Left $ "memory allocation error"
            ModelFileNotFound           -> return . Left $ "file not found"
            ModelFileMismatch           -> return . Left $ "file mismatch"
            ModelNoSuitableLoader       -> return . Left $ "no suitable loader for extension " ++ ext

    case result of
        Right model ->
            let numAnimations = fromIntegral $ cnAnimations model
            in register (freeModel model) >>=
                case numAnimations of
                    0 -> return . Model model V.empty
                    _ -> \key -> setupIO $ do
                        canims <- peekArray numAnimations $ cAnimations model
                        let animations = V.fromList $ fmap fromCAnimation canims
                        return $ Model model animations key

        Left  err   -> setupError $ "loadModel: " ++ err


fromCAnimation :: CAnimation -> Animation
fromCAnimation (CAnimation cname start end) =
    Animation (B.unpack cname) (fromIntegral start) (fromIntegral end)


-- | Release the given 'Model'.
releaseModel :: Model -> Setup ()
releaseModel = release . rkey


-- | Free the given 'CModel'.
freeModel :: CModel -> IO ()
freeModel model = Foreign.with model model_free


-- | Return 'True' if the given 'Model' is animated, 'False' otherwise.
animated :: Model -> Bool
animated = (>1) . numFrames


-- | Return the given 'Model's vertices.
vertices :: Model -> Ptr Vec3
vertices = cVerts . modelData


-- | Return the given 'Model's normals.
normals :: Model -> Ptr Vec3
normals = cNormals . modelData


-- | Return the given 'Model's texCoords.
texCoords :: Model -> Ptr TexCoord
texCoords = cTexCoords . modelData


-- | Return the given 'Model's triangles.
triangles :: Model -> Ptr Triangle
triangles = cTris . modelData


-- | Return the given 'Model's skins.
skins :: Model -> Ptr Skin
skins = cSkins . modelData


-- | Return the given 'Model's number of frames.
numFrames :: Model -> Int
numFrames = fromIntegral . cnFrames . modelData


-- | Return the given 'Model's number of vertices.
numVertices :: Model -> Int
numVertices = fromIntegral . cnVerts . modelData


-- | Return the given 'Model's number of triangles.
numTriangles :: Model -> Int
numTriangles = fromIntegral . cnTris . modelData


-- | Return the given 'Model's number of texture coordinates.
numTexCoords :: Model -> Int
numTexCoords = fromIntegral . cnTexCoords . modelData


-- | Return the given 'Model's number of skins.
numSkins :: Model -> Int
numSkins = fromIntegral . cnSkins . modelData


-- | Return the underlying 'CModel'.
cmodel :: Model -> CModel
cmodel = modelData


-- | Return the model's ith animation.
animation :: Model -> Int -> Animation
animation model i = mAnimations model V.! i


-- | Return the animation specified by the given string.
animationByName :: Model -> String -> Maybe Animation
animationByName model anim = V.find ((==) anim . name) $ mAnimations model


-- | Return the number of animations in the given 'Model'.
numAnimations :: Model -> Int
numAnimations = V.length . mAnimations


-- | Transform the given 'Model's vertices with the given matrix.
transform :: M4.Matrix4 -> Model -> IO ()
transform mat (Model model _ _) =
    allocaBytes (16*sizeFloat) $ \matPtr ->
    allocaBytes (9*sizeFloat)  $ \normalPtr ->
    with model $ \modelPtr -> do
        poke matPtr mat
        poke normalPtr $ fastNormalMatrix mat
        model_transform modelPtr matPtr normalPtr


foreign import ccall "Model.h model_transform"
    model_transform :: Ptr CModel -> Ptr M4.Matrix4 -> Ptr M3.Matrix3 -> IO ()


-- | Transform the given 'Model' such that its lowest point has y = 0.
toGround :: Model -> IO ()
toGround (Model model _ _) = with model model_to_ground


foreign import ccall "Model.h model_to_ground"
    model_to_ground :: Ptr CModel -> IO ()


sizeFloat = #{size float}
