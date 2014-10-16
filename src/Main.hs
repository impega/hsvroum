module Main where

import Graphics.UI.GLUT
import Data.IORef
import Control.Applicative
import Control.Monad
import System.Exit

import Logic
import Display

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
  initialWindowSize  $= Size 1024 768
  _window <- createWindow "VROUUUUM!!"
  -- references
  ship  <- newIORef defaultShip
  traps <- newIORef []
  time  <- newIORef =<< get elapsedTime
  -- callback functions
  displayCallback       $= display ship traps
  reshapeCallback       $= Just reshape
  idleCallback          $= Just (idle _window time ship traps)
  keyboardMouseCallback $= Just (keyboardMouse ship)
  -- btw, we do care about depth
  depthFunc $= Just Less
  mainLoop

idle :: Window -> IORef Int -> IORef Ship -> IORef [Trap] -> IdleCallback
idle _window time shipRef trapsRef = do
  -- compute the time step since last update
  ptime <- get time
  ntime <- get elapsedTime
  let dt = ntime - ptime
  time $= ntime
  -- update the ship
  shipRef $~! moveShip dt
  -- check the outcome
  ship  <- get shipRef
  traps <- get trapsRef
  case outcomes ship traps of
    Dead     -> print "Argh!" >> destroyWindow _window >> exit >> exitSuccess
    Alive ts -> do
      trapsRef $= ts
      -- spaw a trap if we want to
      spawnTrap (ship_pos ship) trapsRef
      -- redraw the scene
      postRedisplay Nothing

keyboardMouse :: IORef Ship -> KeyboardMouseCallback
keyboardMouse shipRef key _ _ _ = case key of
  (SpecialKey KeyLeft ) -> shipRef $~! \ ship -> ship { angle = angle ship + 0.05 }
  (SpecialKey KeyRight) -> shipRef $~! \ ship -> ship { angle = angle ship - 0.05 }
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()

display :: IORef Ship -> IORef [Trap] -> DisplayCallback
display shipRef trapsRef = do
  -- let's not forget to clear the depth buffer!
  clear [ColorBuffer, DepthBuffer]
  loadIdentity
  perspective 90 (4/3) 1 500
  -- we are watching the scene from above
  traps <- get trapsRef
  ship  <- get shipRef
  let pos = ship_pos ship
  let alp = angle ship
  let eye_pos = Vertex3 (x_pos pos) (y_pos pos + 40) (z_pos pos - 40)
  let ctr_pos = Vertex3 (x_pos pos) (y_pos pos + 1)  (z_pos pos + 100)
  let up_vect = Vector3 (-sin (angle ship / 2)) (cos (angle ship / 2)) 0
  lookAt (realToFrac <$> eye_pos) (realToFrac <$> ctr_pos) (realToFrac <$> up_vect)
  -- draw the plane supporting the race
  preservingMatrix $ displayPlane ship 0
  -- draw the traps TODO: dump the ones not visible anymore
  mapM_ displayTrap traps
  -- draw the ship
  preservingMatrix $ displayShip ship
  swapBuffers

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing
