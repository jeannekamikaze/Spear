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

import Data.List (foldl')
import Data.Monoid
import GHC.Float (double2Float)

-- Game events

data GameEvent
     = MoveLeft
     | MoveRight
     | StopLeft
     | StopRight
     deriving Eq

-- Game objects

data GameObject = GameObject
     { aabb   :: AABB2
     , obj    :: Obj2
     , gostep :: Step ([GameEvent], [GameObject], GameObject) GameObject
     }

instance Spatial2 GameObject where
         getObj2 = obj
         setObj2 s o = s { obj = o }

stepWorld :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> [GameObject]
stepWorld elapsed dt evts gos = map (update elapsed dt evts gos) gos

update :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> GameObject -> GameObject
update elapsed dt evts gos go =
       let (go', s') = step (gostep go) elapsed dt (evts, gos, go)
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

-- Generic steppers

ignore :: Step ([GameEvent], [GameObject], GameObject) GameObject
ignore = spure $ \(_,_,go) -> go

ignoreEvts :: Step ([GameEvent], [GameObject], GameObject) ([GameObject], GameObject)
ignoreEvts = spure $ \(_, world, go) -> (world, go)

ignoreGOs :: Step ([GameEvent], [GameObject], GameObject) ([GameEvent], GameObject)
ignoreGOs = spure $ \(evts, _, go) -> (evts, go)

-- Ball steppers

stepBall vel = ignoreEvts .> collideBall vel .> moveBall

collideBall :: Vector2 -> Step ([GameObject], GameObject) (Vector2, GameObject)
collideBall vel = Step $ \_ _ (gos, ball) ->
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

moveBall :: Step (Vector2, GameObject) GameObject
moveBall = Step $ \_ dt (vel,ball) -> (move (scale dt vel) ball, moveBall)

-- Enemy stepper

stepEnemy = ignore .> movePad

movePad :: Step GameObject GameObject
movePad = Step $ \elapsed _ pad ->
        let p  = vec2 px 0.9
            px = double2Float (sin elapsed * 0.5 + 0.5)
               * (1 - 2 * x padSize)
               + x padSize
        in (setPos p pad, movePad)

-- Player stepper

stepPlayer = ignoreGOs
           .> moveGO False MoveLeft StopLeft
           .> moveGO False MoveRight StopRight
           .> ssnd
           .> clamp

moveGO :: Bool -> GameEvent -> GameEvent
       -> Step ([GameEvent], GameObject) ([GameEvent], GameObject)
moveGO moving start stop = Step $ \_ dt (evts, go) ->
       let moving' = (moving || any (==start) evts) && not (any (==stop) evts)
           dir = scale dt $ toDir moving' start
       in ((evts, move dir go), moveGO moving' start stop)

clamp :: Step GameObject GameObject
clamp = spure $ \go ->
      let p' = vec2 (clamp' x s (1 - s)) y
          (Vector2 x y) = pos go
          clamp' x a b = if x < a then a else if x > b then b else x
          (Vector2 s _) = padSize
      in setPos p' go

toDir True MoveLeft  = vec2 (-1) 0
toDir True MoveRight = vec2 1 0
toDir _ _ = vec2 0 0