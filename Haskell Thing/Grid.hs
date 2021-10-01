module Grid where
import Icos

gridDelta = 1/30

data Direction = PosZ | PosX | NegZ | NegX deriving (Eq, Enum);

data Location = Location {
  locationX :: Int,
  locationY :: Int,
  locationFacing :: Direction
}

move i (Location x y facing) = case facing of
  PosX -> Location (x+i)  y    facing
  NegX -> Location (x-i)  y    facing
  PosZ -> Location  x    (y+i) facing
  NegZ -> Location  x    (y-i) facing

face i (Location x y facing) = Location x y $ toEnum $ (fromEnum facing) + i

data Animator = Animator {
  animCurrent :: Int,
  animWhile :: (CameraPoint -> CameraPoint),
  animOnDone :: (Location -> Location)
}

tickAnimator (Animator curr while update) camera location
  | curr >= 0 = (Animator (curr - 1) while update, while camera, location)
  | otherwise = (Animator 0 id id, camera, update location)

doTurnLeft     = Animator 30 (flip yawCamera $ gridDelta/2) (face $ 1)
doTurnRight    = Animator 30 (flip yawCamera $ -gridDelta/2) (face $ -1)
doMoveForward  = Animator 30 (flip moveCameraInto $ gridDelta*5) (move $ 1)
doMoveBackward = Animator 30 (flip moveCameraInto $ -gridDelta*5) (move $ -1)

defaultLocation = Location 0 1 NegZ
defaultAnim = Animator 0 id id
