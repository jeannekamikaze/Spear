module Pong
  ( GameEvent (..),
    GameObject,
    newWorld,
    stepWorld,
    aabb,
  )
where

import           Data.Monoid         (mconcat)
import           GHC.Float           (double2Float)
import           Spear.Math.AABB
import           Spear.Math.Spatial2
import           Spear.Math.Vector
import           Spear.Step

-- Configuration

padSize = vec2 0.05 0.02

ballSize = 0.01

ballVelocity = vec2 0.3 0.3

playerSpeed = 0.7

initialEnemyPos = vec2 0.5 0.9

initialPlayerPos = vec2 0.5 0.1

initialBallPos = vec2 0.5 0.5

-- Game events

data GameEvent
  = MoveLeft
  | MoveRight
  | StopLeft
  | StopRight
  deriving (Eq, Ord)

-- Game objects

data GameObject = GameObject
  { aabb   :: AABB2,
    obj    :: Obj2,
    gostep :: Step [GameObject] [GameEvent] GameObject GameObject
  }

instance Spatial2 GameObject where
  getObj2 = obj
  setObj2 s o = s {obj = o}

stepWorld :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> [GameObject]
stepWorld elapsed dt evts gos = map (update elapsed dt evts gos) gos

update :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> GameObject -> GameObject
update elapsed dt evts gos go =
  let (go', s') = runStep (gostep go) elapsed dt gos evts go
   in go' {gostep = s'}

ballBox, padBox :: AABB2
ballBox = AABB2 (vec2 (-s) (-s)) (vec2 s s) where s = ballSize
padBox = AABB2 (-padSize) padSize

obj2 = obj2FromVectors unitx2 unity2

newWorld =
  [ GameObject ballBox (obj2 initialBallPos) $ stepBall ballVelocity,
    GameObject padBox (obj2 initialEnemyPos) stepEnemy,
    GameObject padBox (obj2 initialPlayerPos) stepPlayer
  ]

-- Ball steppers

stepBall vel = collideBall vel .> moveBall

collideBall :: Vector2 -> Step [GameObject] e GameObject (Vector2, GameObject)
collideBall vel = step $ \_ dt gos _ ball ->
  let (AABB2 pmin pmax) = aabb ball `aabbAdd` pos ball
      collideCol = x pmin < 0 || x pmax > 1
      collideRow = y pmin < 0 || y pmax > 1 || any (collide ball) (tail gos)
      negx v@(Vector2 x y) = if collideCol then vec2 (-x) y else v
      negy v@(Vector2 x y) = if collideRow then vec2 x (-y) else v
      vel' = negx . negy $ vel
      delta = dt -- A small delta to apply when collision occurs.
      adjustX = if collideCol then scale delta (vec2 (x vel) 0) else vec2 0 0
      adjustY = if collideRow then scale delta (vec2 0 (y vel)) else vec2 0 0
   in ((vel' + adjustX + adjustY, ball), collideBall vel')

collide go1 go2 =
  let (AABB2 (Vector2 xmin1 ymin1) (Vector2 xmax1 ymax1)) =
        aabb go1 `aabbAdd` pos go1
      (AABB2 (Vector2 xmin2 ymin2) (Vector2 xmax2 ymax2)) =
        aabb go2 `aabbAdd` pos go2
   in not $
        xmax1 < xmin2
          || xmin1 > xmax2
          || ymax1 < ymin2
          || ymin1 > ymax2

aabbAdd (AABB2 pmin pmax) p = AABB2 (p + pmin) (p + pmax)

moveBall :: Step s e (Vector2, GameObject) GameObject
moveBall = step $ \_ dt _ _ (vel, ball) -> (move (scale dt vel) ball, moveBall)

-- Enemy stepper

stepEnemy = movePad

movePad :: Step s e GameObject GameObject
movePad = step $ \elapsed _ _ _ pad ->
  let p = vec2 px 0.9
      px =
        double2Float (sin elapsed * 0.5 + 0.5)
          * (1 - 2 * x padSize)
          + x padSize
   in (setPos p pad, movePad)

-- Player stepper

stepPlayer = sfold moveGO .> clamp

moveGO =
  mconcat
    [ switch StopLeft sid MoveLeft (moveGO' $ vec2 (-playerSpeed) 0),
      switch StopRight sid MoveRight (moveGO' $ vec2 playerSpeed 0)
    ]

moveGO' :: Vector2 -> Step s e GameObject GameObject
moveGO' dir = step $ \_ dt _ _ go -> (move (scale dt dir) go, moveGO' dir)

clamp :: Step s e GameObject GameObject
clamp = spure $ \go ->
  let p' = vec2 (clamp' x s (1 - s)) y
      (Vector2 x y) = pos go
      clamp' x a b
        | x < a = a
        | x > b = b
        | otherwise = x
      (Vector2 s _) = padSize
   in setPos p' go
