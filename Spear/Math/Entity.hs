module Spear.Math.Entity
(
    Entity(..)
)
where


import qualified Spear.Math.Matrix4 as M
import qualified Spear.Math.Spatial as S
import qualified Spear.Math.Vector3 as V


-- | An entity in 3D space.
newtype Entity = Entity { transform :: M.Matrix4 }


instance S.Spatial Entity where
    move        v ent = ent { transform = M.translv v * transform ent }
    moveFwd     f ent = ent { transform = M.translv (V.scale f $ S.fwd ent) * transform ent }
    moveBack    f ent = ent { transform = M.translv (V.scale (-f) $ S.fwd ent) * transform ent }
    strafeLeft  f ent = ent { transform = M.translv (V.scale (-f) $ S.right ent) * transform ent }
    strafeRight f ent = ent { transform = M.translv (V.scale f $ S.right ent) * transform ent }
    pitch       a ent = ent { transform = transform ent * M.axisAngle (S.right ent) a }
    yaw         a ent = ent { transform = transform ent * M.axisAngle (S.up ent) a }
    roll        a ent = ent { transform = transform ent * M.axisAngle (S.fwd ent) a }
    pos   = M.position . transform
    fwd   = M.forward  . transform
    up    = M.up       . transform
    right = M.right    . transform
    transform (Entity t) = t
    setTransform t (Entity _) = Entity t
