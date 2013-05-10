module Spear.Scene.Light
(
    Light(..)
)
where


import qualified Spear.Math.Matrix4 as M
import qualified Spear.Math.Spatial3 as S
import Spear.Math.Vector


data Light
    = PointLight
    { ambient   :: Vector3
    , diffuse   :: Vector3
    , specular  :: Vector3
    , transform :: M.Matrix4
    }
    | DirectionalLight
    { ambient   :: Vector3
    , diffuse   :: Vector3
    , specular  :: Vector3
    , direction :: Vector3
    }
    | SpotLight
    { ambient   :: Vector3
    , diffuse   :: Vector3
    , specular  :: Vector3
    , transform :: M.Matrix4
    }
