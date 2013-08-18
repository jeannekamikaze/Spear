module Pong
(
    GameEvent(..)
,   GameObject
,   newWorld
,   stepWorld
,   aabb
)
where

import Spear.Math.AABB
import Spear.Math.Spatial2
import Spear.Math.Vector
import Spear.Step

import Data.Monoid (mconcat)
import GHC.Float (double2Float)

-- Game events

data GameEvent
     = MoveLeft
     | MoveRight
     | StopLeft
     | StopRight
     deriving (Eq, Ord)

-- Game objects

data GameObject = GameObject
     { aabb   :: AABB2
     , obj    :: Obj2
     , gostep :: Step [GameObject] [GameEvent] GameObject GameObject
     }

instance Spatial2 GameObject where
         getObj2 = obj
         setObj2 s o = s { obj = o }

stepWorld :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> [GameObject]
stepWorld elapsed dt evts gos = map (update elapsed dt evts gos) gos

update :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> GameObject -> GameObject
update elapsed dt evts gos go =
       let (go', s') = runStep (gostep go) elapsed dt gos evts go
       in go' { gostep = s' }

ballBox :: AABB2
ballBox = AABB2 (vec2 (-s) (-s)) (vec2 s s) where s = 0.01

padSize = vec2 0.05 0.02

padBox = AABB2 (-padSize) padSize

obj2 x y = obj2FromVectors unitx2 unity2 (vec2 x y)

ballVelocity = Vector2 0.3 0.3

newWorld =
         [ GameObject ballBox (obj2 0.5 0.5) $ stepBall ballVelocity
         , GameObject padBox  (obj2 0.5 0.9) stepEnemy
         , GameObject padBox  (obj2 0.5 0.1) stepPlayer
         ]

-- Ball steppers

stepBall vel = collideBall vel .> moveBall

collideBall :: Vector2 -> Step [GameObject] e GameObject (Vector2, GameObject)
collideBall vel = step $ \_ _ gos _ ball ->
            let (AABB2 pmin pmax) = aabb ball `aabbAdd` pos ball
                collideCol = x pmin < 0 || x pmax > 1
                collideRow = y pmin < 0 || y pmax > 1
                           || any (collide ball) (tail gos)
                negx v@(Vector2 x y) = if collideCol then vec2 (-x) y else v
                negy v@(Vector2 x y) = if collideRow then vec2 x (-y) else v
                vel' = negx . negy $ vel
            in ((vel', ball), collideBall vel')

collide go1 go2 =
        let (AABB2 (Vector2 xmin1 ymin1) (Vector2 xmax1 ymax1))
                   = aabb go1 `aabbAdd` pos go1
            (AABB2 (Vector2 xmin2 ymin2) (Vector2 xmax2 ymax2))
                   = aabb go2 `aabbAdd` pos go2
        in not $  xmax1 < xmin2 || xmin1 > xmax2
               || ymax1 < ymin2 || ymin1 > ymax2

aabbAdd (AABB2 pmin pmax) p = AABB2 (p+pmin) (p+pmax)

moveBall :: Step s e (Vector2, GameObject) GameObject
moveBall = step $ \_ dt _ _ (vel,ball) -> (move (scale dt vel) ball, moveBall)

-- Enemy stepper

stepEnemy = movePad

movePad :: Step s e GameObject GameObject
movePad = step $ \elapsed _ _ _ pad ->
        let p  = vec2 px 0.9
            px = double2Float (sin elapsed * 0.5 + 0.5)
               * (1 - 2 * x padSize)
               + x padSize
        in (setPos p pad, movePad)

-- Player stepper

stepPlayer = sfold moveGO .> clamp

moveGO = mconcat
       [ switch StopLeft  sid MoveLeft  (moveGO' $ vec2 (-1) 0)
       , switch StopRight sid MoveRight (moveGO' $ vec2 1 0)
       ]

moveGO' :: Vector2 -> Step s e GameObject GameObject
moveGO' dir = step $ \_ dt _ _ go -> (move (scale dt dir) go, moveGO' dir)

clamp :: Step s e GameObject GameObject
clamp = spure $ \go ->
      let p' = vec2 (clamp' x s (1 - s)) y
          (Vector2 x y) = pos go
          clamp' x a b = if x < a then a else if x > b then b else x
          (Vector2 s _) = padSize
      in setPos p' go

toDir True MoveLeft  = vec2 (-1) 0
toDir True MoveRight = vec2 1 0
toDir _ _ = vec2 0 0