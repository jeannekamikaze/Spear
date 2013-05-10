{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Spear.Render.Model
(
    RenderModel(..)
,   renderModelFromModel
)
where

import qualified Spear.Assets.Model as Assets
import Spear.Game

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils (with)
import Foreign.Storable

#include "RenderModel.h"

data Vec3 = Vec3 !CFloat !CFloat !CFloat

data TexCoord = TexCoord !CFloat !CFloat

data RenderModel = RenderModel
    { elements    :: Ptr CChar
    , numFrames   :: CUInt
    , numVertices :: CUInt -- ^ Number of vertices per frame.
    }

instance Storable RenderModel where
    sizeOf _    = #{size RenderModel}
    alignment _ = alignment (undefined :: CUInt)

    peek ptr = do
        elements    <- #{peek RenderModel, elements}    ptr
        numFrames   <- #{peek RenderModel, numFrames}   ptr
        numVertices <- #{peek RenderModel, numVertices} ptr
        return $ RenderModel elements numFrames numVertices

    poke ptr (RenderModel elements numFrames numVertices) = do
        #{poke RenderModel, elements}    ptr elements
        #{poke RenderModel, numFrames}   ptr numFrames
        #{poke RenderModel, numVertices} ptr numVertices

foreign import ccall "RenderModel.h render_model_from_model_asset"
    render_model_from_model_asset :: Ptr Assets.Model -> Ptr RenderModel -> IO Int

-- | Convert the given 'Model' to a 'ModelData' instance.
renderModelFromModel :: Assets.Model -> IO RenderModel
renderModelFromModel m = with m $ \mPtr -> alloca $ \mdPtr -> do
    render_model_from_model_asset mPtr mdPtr
    peek mdPtr
