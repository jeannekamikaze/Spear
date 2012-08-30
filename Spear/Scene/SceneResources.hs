module Spear.Scene.SceneResources
(
    SceneResources(..)
,   StaticProgram(..)
,   AnimatedProgram(..)
,   emptySceneResources
,   getShader
,   getStaticProgram
,   getAnimatedProgram
,   getTexture
,   getStaticModel
,   getAnimatedModel
)
where


import Spear.Assets.Model as Model
import Spear.GLSL as GLSL
import Spear.Math.Vector3
import Spear.Render.AnimatedModel
import Spear.Render.Material
import Spear.Render.Program
import Spear.Render.StaticModel
import Spear.Scene.Light

import Data.Map as M


data SceneResources = SceneResources
    { shaders          :: Map String GLSLShader
    , staticPrograms   :: Map String StaticProgram
    , animatedPrograms :: Map String AnimatedProgram
    , textures         :: Map String Texture
    , staticModels     :: Map String StaticModelResource
    , animatedModels   :: Map String AnimatedModelResource
    , lights           :: [Light]
    }


-- | Build an empty instance of 'SceneResources'.
emptySceneResources = SceneResources M.empty M.empty M.empty M.empty M.empty M.empty []


-- | Get the 'GLSLShader' specified by the given 'String' from the given 'SceneResources'.
getShader :: SceneResources -> String -> Maybe GLSLShader
getShader res key = M.lookup key $ shaders res


-- | Get the 'StaticProgram' specified by the given 'String' from the given 'SceneResources'.
getStaticProgram :: SceneResources -> String -> Maybe StaticProgram
getStaticProgram res key = M.lookup key $ staticPrograms res


-- | Get the 'AnimatedProgram' specified by the given 'String' from the given 'SceneResources'.
getAnimatedProgram :: SceneResources -> String -> Maybe AnimatedProgram
getAnimatedProgram res key = M.lookup key $ animatedPrograms res


-- | Get the 'Texture' specified by the given 'String' from the given 'SceneResources'.
getTexture :: SceneResources -> String -> Maybe Texture
getTexture res key = M.lookup key $ textures res


-- | Get the 'StaticModelResource' specified by the given 'String' from the given 'SceneResources'.
getStaticModel :: SceneResources -> String -> Maybe StaticModelResource
getStaticModel res key = M.lookup key $ staticModels res


-- | Get the 'AnimatedModelResource' specified by the given 'String' from the given 'SceneResources'.
getAnimatedModel :: SceneResources -> String -> Maybe AnimatedModelResource
getAnimatedModel res key = M.lookup key $ animatedModels res
