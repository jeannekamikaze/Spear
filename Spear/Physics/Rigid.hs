module Spear.Physics.Rigid
(
    module Spear.Physics.Types
,   RigidBody(..)
,   rigidBody
,   update
,   setVelocity
,   setAcceleration
)
where


import qualified Spear.Math.Matrix3 as M3
import Spear.Math.Spatial2
import Spear.Math.Vector2
import Spear.Physics.Types

import Data.List (foldl')
import Control.Monad.State


data RigidBody = RigidBody
    { mass         :: {-# UNPACK #-} !Float
    , position     :: {-# UNPACK #-} !Vector2
    , velocity     :: {-# UNPACK #-} !Vector2
    , acceleration :: {-# UNPACK #-} !Vector2
    }


instance Spatial2 RigidBody where
    
    move v body = body { position = v + position body }
    
    moveFwd     speed body = body { position = position body + scale speed unity }
    
    moveBack    speed body = body { position = position body + scale (-speed) unity }
    
    strafeLeft  speed body = body { position = position body + scale (-speed) unitx }
    
    strafeRight speed body = body { position = position body + scale speed unitx }
    
    rotate angle = id
    
    setRotation angle = id
    
    pos = position
    
    fwd _ = unity
    
    up _ = unity
    
    right _ = unitx
    
    transform body = M3.transform unitx unity $ position body
    
    setTransform transf body = body { position = M3.position transf }
    
    setPos p body = body { position = p }


-- | Build a 'RigidBody'.
rigidBody :: Mass -> Position -> RigidBody
rigidBody m x = RigidBody m x zero zero


-- | Update the given 'RigidBody'.
update :: [Force] -> Dt -> RigidBody -> RigidBody
update forces dt body =
    let netforce = foldl' (+) zero forces
        m  = mass body
        r1 = position body
        v1 = velocity body
        a1 = acceleration body
        r2 = r1 + scale dt v1 + scale (0.5*dt*dt) a1
        v' = v1 + scale (0.5*dt) a1
        a2 = a1 + scale (1/m) netforce
        v2 = v1 + scale (dt/2) (a2+a1) + scale (0.5*dt) a2
    in
        RigidBody m r2 v2 a2


-- | Set the body's velocity.
setVelocity :: Velocity -> RigidBody -> RigidBody
setVelocity v body = body { velocity = v }


-- | Set the body's acceleration.
setAcceleration :: Acceleration -> RigidBody -> RigidBody
setAcceleration a body = body { acceleration = a }


-- test
gravity = vec2 0 (-10)
b0 = rigidBody 50 $ vec2 0 1000


debug :: IO ()
debug = evalStateT debug' b0



debug' :: StateT RigidBody IO ()
debug' = do
    lift . putStrLn $ "Initial body:"
    lift . putStrLn . show' $ b0
    lift . putStrLn $ "Falling..."
    step $ update [gravity*50] 1
    step $ update [gravity*50] 1
    step $ update [gravity*50] 1
    lift . putStrLn $ "Jumping"
    step $ update [gravity*50, vec2 0 9000] 1
    lift . putStrLn $ "Falling..."
    step $ update [gravity*50] 1
    step $ update [gravity*50] 1
    step $ update [gravity*50] 1


step :: (RigidBody -> RigidBody) -> StateT RigidBody IO ()
step update = do
    modify update
    body <- get
    lift . putStrLn . show' $ body


show' body =
    "mass " ++ (show $ mass body) ++
    ", position " ++ (showVec $ position body) ++
    ", velocity " ++ (showVec $ velocity body) ++
    ", acceleration " ++ (showVec $ acceleration body)


showVec v = (show $ x v) ++ ", " ++ (show $ y v)
