module Spear.Scene.SceneResources
(
    -- * Data types
    SceneResources(..)
,   StaticProgram(..)
,   AnimatedProgram(..)
    -- * Construction
,   emptySceneResources
    -- * Accessors
,   getShader
,   getCustomProgram
,   getStaticProgram
,   getAnimatedProgram
,   getTexture
,   getStaticModel
,   getAnimatedModel
)
where

import Spear.Assets.Model as Model
import Spear.GL as GL
import Spear.Math.Vector
import Spear.Render.AnimatedModel
import Spear.Render.Material
import Spear.Render.Program
import Spear.Render.StaticModel
import Spear.Scene.Light

import Data.Map as M

data SceneResources = SceneResources
    { shaders          :: Map String GLSLShader
    , customPrograms   :: Map String GLSLProgram
    , staticPrograms   :: Map String StaticProgram
    , animatedPrograms :: Map String AnimatedProgram
    , textures         :: Map String Texture
    , staticModels     :: Map String StaticModelResource
    , animatedModels   :: Map String AnimatedModelResource
    , lights           :: [Light]
    }

-- | Build an empty instance of 'SceneResources'.
emptySceneResources =
    SceneResources M.empty M.empty M.empty M.empty M.empty M.empty M.empty []

-- | Get the shader specified by the given string.
getShader :: SceneResources -> String -> Maybe GLSLShader
getShader res key = M.lookup key $ shaders res

-- | Get the custom program specified by the given string.
getCustomProgram :: SceneResources -> String -> Maybe GLSLProgram
getCustomProgram res key = M.lookup key $ customPrograms res

-- | Get the static program specified by the given string.
getStaticProgram :: SceneResources -> String -> Maybe StaticProgram
getStaticProgram res key = M.lookup key $ staticPrograms res

-- | Get the animated program specified by the given string.
getAnimatedProgram :: SceneResources -> String -> Maybe AnimatedProgram
getAnimatedProgram res key = M.lookup key $ animatedPrograms res

-- | Get the texture specified by the given string.
getTexture :: SceneResources -> String -> Maybe Texture
getTexture res key = M.lookup key $ textures res

-- | Get the static model resource specified by the given string.
getStaticModel :: SceneResources -> String -> Maybe StaticModelResource
getStaticModel res key = M.lookup key $ staticModels res

-- | Get the animated model resource specified by the given string.
getAnimatedModel :: SceneResources -> String -> Maybe AnimatedModelResource
getAnimatedModel res key = M.lookup key $ animatedModels res
