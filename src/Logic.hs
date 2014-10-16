module Logic where

import Graphics.UI.GLUT
import System.Random
import Data.IORef
import Data.Monoid

data Point3D =
  Point3D { x_pos :: GLfloat
          , y_pos :: GLfloat
          , z_pos :: GLfloat }

toVertex3 :: Point3D -> Vertex3 GLfloat
toVertex3 pt = Vertex3 (x_pos pt) (y_pos pt) (z_pos pt)

toVector3 :: Point3D -> Vector3 GLfloat
toVector3 pt = Vector3 (x_pos pt) (y_pos pt) (z_pos pt)

data Ship = Ship { ship_pos :: Point3D
                 , velocity :: GLfloat
                 , angle    :: GLfloat
                 , strength :: GLdouble }

defaultPoint3D :: Point3D
defaultPoint3D = Point3D { x_pos = 0 , y_pos = 0 , z_pos = 0 }

defaultShip :: Ship
defaultShip =
  Ship { ship_pos = defaultPoint3D
       , velocity = 0.1
       , angle    = 0
       , strength = 5 }

moveShip :: Int -> Ship -> Ship
moveShip dt ship =
  let pt  = ship_pos ship
      dt' = fromIntegral dt
      alp = realToFrac $ angle ship
      vel = realToFrac $ velocity ship
  in ship { ship_pos = pt { x_pos = x_pos pt + alp * (dt' / 3)
                          , z_pos = z_pos pt + vel * dt' }}

data Trap = Trap { trap_pos :: Point3D
                 , trap_col :: Color3 GLdouble
                 , size     :: GLfloat }

data Outcome = Dead | Alive [Trap]

instance Monoid Outcome where
  mempty = Alive []
  mappend Dead       _          = Dead
  mappend _          Dead       = Dead
  mappend (Alive xs) (Alive ys) = Alive $ xs ++ ys

outcome :: Ship -> Trap -> Outcome
outcome ship trap =
  let spos = ship_pos ship
      tpos = trap_pos trap in
  if z_pos tpos + 2 * size trap < z_pos spos
  then Alive []
  else
    let dx = x_pos tpos - x_pos spos
        dz = z_pos tpos - z_pos spos
    in if sqrt ((dx + 3) * (dx + 3) + dz * dz) < 5
       || sqrt ((dx - 3) * (dx - 3) + dz * dz) < 5
       then Dead
       else Alive [trap]

outcomes :: Ship -> [Trap] -> Outcome
outcomes ship = foldl (\ o -> (o <>) . outcome ship) mempty

randPoint3D :: Point3D -> IO Point3D
randPoint3D pt = do
  x <- randomRIO (x_pos pt - 500, x_pos pt + 500)
  y <- randomRIO (2, 2.1)
  return $ Point3D { x_pos = x, y_pos = y, z_pos = z_pos pt + 500 }

randTrap :: Point3D -> IO Trap
randTrap pos_ship = do
  p <- randPoint3D pos_ship
  r <- randomRIO (0.2, 1)
  g <- randomRIO (0.2, 0.7)
  b <- randomRIO (0.1, 0.4)
  s1 <- randomRIO (0, 3)
  s2 <- randomRIO (0, 3)
  s3 <- randomRIO (1, 4)
  return $ Trap p (Color3 r g b) (s1 + s2 + s3)

spawnTrap :: Point3D -> IORef [Trap] -> IO ()
spawnTrap pt trapsRef = do
  trap <- randTrap pt
  trapsRef $~! (trap :)
