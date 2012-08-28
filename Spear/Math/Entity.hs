module Spear.Math.Entity
(
    Entity(..)
)
where


import qualified Spear.Math.Matrix3 as M
import qualified Spear.Math.Spatial2 as S
import qualified Spear.Math.Vector2 as V


-- | An entity in 2D space.
newtype Entity = Entity { transform :: M.Matrix3 }


instance S.Spatial2 Entity where
    move        v ent = ent { transform = M.translv v * transform ent }
    moveFwd     f ent = ent { transform = M.translv (V.scale f $ S.fwd ent) * transform ent }
    moveBack    f ent = ent { transform = M.translv (V.scale (-f) $ S.fwd ent) * transform ent }
    strafeLeft  f ent = ent { transform = M.translv (V.scale (-f) $ S.right ent) * transform ent }
    strafeRight f ent = ent { transform = M.translv (V.scale f $ S.right ent) * transform ent }
    rotate      a ent = ent { transform = transform ent * M.rot a }
    pos   = M.position . transform
    fwd   = M.forward  . transform
    right = M.right    . transform
    transform (Entity t) = t
    setTransform t (Entity _) = Entity t
    setPos pos (Entity t) = Entity $ M.transform (M.right t) (M.forward t) pos
