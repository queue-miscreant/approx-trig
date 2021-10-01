{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Main where
import Icos
import Grid

import qualified SDL
import SDL.Event
import Control.Monad

type GameState = (Animator, CameraPoint, Location)
defaultGameState = (defaultAnim, defaultCamera, defaultLocation) :: GameState

tryAction action state@(anim, camera, loc)
  | animCurrent anim > 0 = state
  | otherwise = (action, camera, loc)

ploop renderer !state@(gameState, frame) = do 
  let (animator, camera, loc) = gameState
  let getCamera = getTransformer camera
  let SDL.V3 x 0 y = camPosition camera

  SDL.clear renderer

  Just (SDL.Rectangle _ (SDL.V2 width height)) <- 
    SDL.get $ SDL.rendererViewport renderer 
  let whVector = fmap (/2) $ SDL.V2 (fromIntegral width) (fromIntegral height)

  drawObject renderer getCamera icosahedron
  drawObject renderer getCamera icosahedron2

  --centered point showing camera location
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 maxBound
  SDL.drawPoint renderer $ SDL.P $ fmap floor $ (+whVector) $ SDL.V2 (x*10) (y*10)
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 maxBound

  SDL.present renderer

  let newState = seq state (tickAnimator animator camera loc, frame+1)

  evts <- SDL.pollEvents
  handle (ploop renderer) (map eventPayload evts) (newState, False)

handle next []         (s,q) = SDL.delay 2 >> (unless q $ next s)
handle next (evt:evts) state = do
  let newState = maybe state id $ (Just state) >>= (\((gameState, frame),q) -> case evt of {
    KeyboardEvent a -> case SDL.keysymKeycode $ keyboardEventKeysym a of {
      SDL.KeycodeW -> Just ((tryAction doMoveForward  gameState, frame), q);
      SDL.KeycodeS -> Just ((tryAction doMoveBackward gameState, frame), q);
      SDL.KeycodeD -> Just ((tryAction doTurnLeft     gameState, frame), q);
      SDL.KeycodeA -> Just ((tryAction doTurnRight    gameState, frame), q);
      otherwise    -> Nothing
    };
    QuitEvent      -> Just ((gameState, frame), True);
    otherwise      -> Nothing
  }) 

  handle next evts newState

main = do 
  SDL.initializeAll
  window <- SDL.createWindow "" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  ploop renderer (defaultGameState, 0)
  SDL.quit
