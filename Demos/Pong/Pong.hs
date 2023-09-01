{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Pong
  ( GameEvent (..),
    GameObject,
    newWorld,
    stepWorld,
    aabb,
  )
where

import           Spear.Math.AABB
import           Spear.Math.Algebra
import           Spear.Math.Spatial
import           Spear.Math.Spatial2
import           Spear.Math.Vector
import           Spear.Prelude
import           Spear.Step

import           Data.Monoid         (mconcat)


-- Configuration

padSize             = vec2 0.07 0.02
ballSize            = 0.012 :: Float
ballSpeed           = 0.6 :: Float
initialBallVelocity = vec2 1 1
maxBounceAngle      = (65::Float) * (pi::Float)/(180::Float)
playerSpeed         = 1.0 :: Float
enemySpeed          = 3.0 :: Float
initialEnemyPos     = vec2 0.5 0.9
initialPlayerPos    = vec2 0.5 0.1
initialBallPos      = vec2 0.5 0.5

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
    basis  :: Transform2,
    gostep :: Step [GameObject] [GameEvent] GameObject GameObject
  }


instance Has2dTransform GameObject where
  set2dTransform transform object = object { basis = transform }
  transform2 = basis


instance Positional GameObject Vector2 where
  setPosition p = with2dTransform (setPosition p)
  position = position . basis
  translate v = with2dTransform (translate v)


instance Rotational GameObject Vector2 Angle where
  setRotation r = with2dTransform (setRotation r)
  rotation = rotation . basis
  rotate angle = with2dTransform (rotate angle)
  right = right . basis
  up = up . basis
  forward = forward . basis
  setForward v = with2dTransform (setForward v)


instance Spatial GameObject Vector2 Angle Transform2 where
  setTransform t obj = obj { basis = t }
  transform = basis


stepWorld :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> [GameObject]
stepWorld elapsed dt evts gos = map (update elapsed dt evts gos) gos

update :: Elapsed -> Dt -> [GameEvent] -> [GameObject] -> GameObject -> GameObject
update elapsed dt evts gos go =
  let (go', s') = runStep (gostep go) elapsed dt gos evts go
   in go' {gostep = s'}

ballBox, padBox :: AABB2
ballBox = AABB2 (vec2 (-s) (-s)) (vec2 s s) where s = ballSize
padBox = AABB2 (-padSize) padSize

newWorld =
  [ GameObject ballBox (makeAt initialBallPos) $ stepBall initialBallVelocity,
    GameObject padBox  (makeAt initialEnemyPos)  stepEnemy,
    GameObject padBox  (makeAt initialPlayerPos) stepPlayer
  ]
  where makeAt = newTransform2 unitx2 unity2

-- Ball steppers

stepBall vel = collideBall vel .> moveBall

-- TODO: in collideBall and paddleBounce, we should an apply an offset to the
-- ball when collision is detected.
collideBall :: Vector2 -> Step [GameObject] e GameObject (Vector2, GameObject)
collideBall vel = step $ \_ dt gos _ ball ->
  let (AABB2 pmin pmax) = translate (position ball) (aabb ball)
      collideSide = x pmin < 0 || x pmax > 1
      collideBack = y pmin < 0 || y pmax > 1
      collidePaddle = any (collide ball) (tail gos)
      flipX v@(Vector2 x y) = if collideSide then vec2 (-x) y else v
      flipY v@(Vector2 x y) = if collideBack then vec2 x (-y) else v
      vel' = normalise . (\v -> foldl (paddleBounce ball) v (tail gos)) . flipX . flipY $ vel
      -- A small delta to apply when collision occurs.
      delta = (1::Float) + if collideSide || collideBack || collidePaddle then (2::Float)*dt else (0::Float)
   in ((ballSpeed * delta * vel', ball), collideBall vel')

paddleBounce :: GameObject -> Vector2 -> GameObject -> Vector2
paddleBounce ball v paddle =
  if collide ball paddle
  then
    let (AABB2 pmin pmax) = translate (position paddle) (aabb paddle)
        center = (x pmin + x pmax) / (2::Float)
        -- Normalized offset of the ball from the paddle's center, [-1, +1].
        -- It's outside the [-1, +1] range if there is no collision.
        offset = (x (position ball) - center) / ((x pmax - x pmin) / (2::Float))
        angle  = offset * maxBounceAngle
        -- When it bounces off of a paddle, y vel is flipped.
        ysign = -(signum (y v))
    in vec2 (sin angle) (ysign * cos angle)
  else v

collide :: GameObject -> GameObject -> Bool
collide go1 go2 =
  let (AABB2 (Vector2 xmin1 ymin1) (Vector2 xmax1 ymax1)) =
        translate (position go1) (aabb go1)
      (AABB2 (Vector2 xmin2 ymin2) (Vector2 xmax2 ymax2)) =
        translate (position go2) (aabb go2)
   in not $
        xmax1 < xmin2
          || xmin1 > xmax2
          || ymax1 < ymin2
          || ymin1 > ymax2

moveBall :: Step s e (Vector2, GameObject) GameObject
moveBall = step $ \_ dt _ _ (vel, ball) -> (translate (vel * dt) ball, moveBall)

-- Enemy stepper

stepEnemy = movePad

movePad :: Step s e GameObject GameObject
movePad = step $ \elapsed _ _ _ pad ->
  let enemyY = 0.9
      p = vec2 px enemyY
      px =
        (sin (enemySpeed * elapsed) * (0.5::Float) + (0.5::Float))
          * ((1::Float) - (2::Float) * x padSize)
          + x padSize
   in (setPosition p pad, movePad)

-- Player stepper

stepPlayer = sfold moveGO .> clamp

moveGO =
  mconcat
    [ switch StopLeft  sid MoveLeft  (moveGO' $ vec2 (-playerSpeed) 0),
      switch StopRight sid MoveRight (moveGO' $ vec2   playerSpeed  0)
    ]

moveGO' :: Vector2 -> Step s e GameObject GameObject
moveGO' dir = step $ \_ dt _ _ go -> (translate (dir * dt) go, moveGO' dir)

clamp :: Step s e GameObject GameObject
clamp = spure $ \go ->
  let p' = vec2 (clamp' x s (1 - s)) y
      (Vector2 x y) = position go
      clamp' x a b
        | x < a = a
        | x > b = b
        | otherwise = x
      (Vector2 s _) = padSize
   in setPosition p' go
