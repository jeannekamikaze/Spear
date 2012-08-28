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


import qualified Spear.Math.Matrix4 as M4
import Spear.Math.Spatial
import Spear.Math.Vector3 as V3
import Spear.Physics.Types

import Data.List (foldl')
import Control.Monad.State


data RigidBody = RigidBody
    { mass         :: !Float
    , position     :: !Vector3
    , velocity     :: !Vector3
    , acceleration :: !Vector3
    }


instance Spatial RigidBody where
    
    move v body = body { position = v + position body }
    
    moveFwd     speed body = body { position = position body + scale (-speed) unitZ }
    
    moveBack    speed body = body { position = position body + scale speed unitZ }
    
    strafeLeft  speed body = body { position = position body + scale (-speed) unitX }
    
    strafeRight speed body = body { position = position body + scale speed unitX }
    
    pitch angle = id
    
    yaw angle = id
    
    roll angle = id
    
    pos = position
    
    fwd _ = unitZ
    
    up _ = unitY
    
    right _ = unitX
    
    transform body = M4.transform unitX unitY unitZ $ position body
    
    setTransform transf body = body { position = M4.position transf }
    
    setPos p body = body { position = p }


-- | Build a 'RigidBody'.
rigidBody :: Mass -> Position -> RigidBody
rigidBody m x = RigidBody m x V3.zero V3.zero


-- | Update the given 'RigidBody'.
update :: [Force] -> Dt -> RigidBody -> RigidBody
update forces dt body =
    let netforce = foldl' (+) V3.zero forces
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
gravity = vec3 0 (-10) 0
b0 = rigidBody 50 $ vec3 0 1000 0


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
    step $ update [gravity*50, vec3 0 9000 0] 1
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


showVec v = (show $ x v) ++ ", " ++ (show $ y v) ++ ", " ++ (show $ z v)
