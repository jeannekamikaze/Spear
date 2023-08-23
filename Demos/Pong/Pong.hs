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

padSize = vec2 0.07 0.02
ballSize = 0.012
ballSpeed = 0.6
initialBallVelocity = vec2 1 1
maxBounceAngle = 65 * pi/180
playerSpeed = 1.0
enemySpeed = 1.5
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
  [ GameObject ballBox (obj2 initialBallPos) $ stepBall initialBallVelocity,
    GameObject padBox (obj2 initialEnemyPos) stepEnemy,
    GameObject padBox (obj2 initialPlayerPos) stepPlayer
  ]

-- Ball steppers

stepBall vel = collideBall vel .> moveBall

-- TODO: in collideBall and paddleBounce, we should an apply an offset to the
-- ball when collision is detected.
collideBall :: Vector2 -> Step [GameObject] e GameObject (Vector2, GameObject)
collideBall vel = step $ \_ dt gos _ ball ->
  let (AABB2 pmin pmax) = aabb ball `aabbAdd` pos ball
      collideSide = x pmin < 0 || x pmax > 1
      collideBack = y pmin < 0 || y pmax > 1
      collidePaddle = any (collide ball) (tail gos)
      flipX v@(Vector2 x y) = if collideSide then vec2 (-x) y else v
      flipY v@(Vector2 x y) = if collideBack then vec2 x (-y) else v
      vel' = normalise . (\v -> foldl (paddleBounce ball) v (tail gos)) . flipX . flipY $ vel
      -- A small delta to apply when collision occurs.
      delta = 1 + if collideSide || collideBack || collidePaddle then 2*dt else 0
   in ((scale ballSpeed (scale delta vel'), ball), collideBall vel')

paddleBounce :: GameObject -> Vector2 -> GameObject -> Vector2
paddleBounce ball v paddle =
  if collide ball paddle
  then
    let (AABB2 pmin pmax) = aabb paddle `aabbAdd` pos paddle
        center = (x pmin + x pmax) / 2
        -- Normalized offset of the ball from the paddle's center, [-1, +1].
        -- It's outside the [-1, +1] range if there is no collision.
        offset = (x (pos ball) - center) / ((x pmax - x pmin) / 2)
        angle  = offset * maxBounceAngle
        -- When it bounces off of a paddle, y vel is flipped.
        ysign = -(signum (y v))
    in vec2 (sin angle) (ysign * cos angle)
  else v

collide :: GameObject -> GameObject -> Bool
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
        double2Float (sin (elapsed * enemySpeed) * 0.5 + 0.5)
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
