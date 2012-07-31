module Spear.Scene.Light
(
    Light(..)
)
where


import qualified Spear.Math.Matrix4 as M
import qualified Spear.Math.Spatial as S
import Spear.Math.Vector3
import qualified Spear.Math.Vector4 as V4


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


instance S.Spatial Light where
    move _ l@DirectionalLight {} = l
    move v l = l { transform = M.translv v * transform l}
    
    moveFwd _ l@DirectionalLight {} = l
    moveFwd f l = l { transform = M.translv (scale f $ S.fwd l) * transform l }
    
    moveBack _ l@DirectionalLight {} = l
    moveBack f l = l { transform = M.translv (scale (-f) $ S.fwd l) * transform l }
    
    strafeLeft _ l@DirectionalLight {} = l
    strafeLeft f l = l { transform = M.translv (scale (-f) $ S.right l) * transform l }
    
    strafeRight _ l@DirectionalLight {} = l
    strafeRight f l = l { transform = M.translv (scale f $ S.right l) * transform l }
    
    pitch _ l@DirectionalLight {} = l
    pitch a l = l { transform = transform l * M.axisAngle (S.right l) a }
    
    yaw _ l@DirectionalLight {} = l
    yaw a l = l { transform = transform l * M.axisAngle (S.up l) a }
    
    roll _ l@DirectionalLight {} = l
    roll a l = l { transform = transform l * M.axisAngle (S.fwd l) a }
    
    pos l@DirectionalLight {} = vec3 0 0 0
    pos l = M.position . transform $ l
    
    fwd (DirectionalLight _ _ _ f) = f
    fwd l = M.forward . transform $ l
    
    up l@DirectionalLight {} = vec3 0 1 0
    up l = M.up . transform $ l
    
    right l@DirectionalLight {} = vec3 1 0 0
    right l = M.right . transform $ l
    
    transform (PointLight _ _ _ transf) = transf
    transform (DirectionalLight _ _ _ fwd) =
        let up'   = vec3 0 1 0
            right = up `cross` fwd
            up    = fwd `cross` right
        in
            M.transform up right fwd (vec3 0 0 0)
    transform (SpotLight _ _ _ transf) = transf
    
    setTransform _ l@DirectionalLight {} = l
    setTransform t l = l { Spear.Scene.Light.transform = t }
