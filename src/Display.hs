module Display where

import Control.Monad
import Graphics.UI.GLUT
import Logic

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

vertex3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
vertex3f x y z = vertex $ Vertex3 x y z

vertex3fs :: [GLfloat] -> IO ()
vertex3fs (x : y : z : fs) = vertex3f x y z >> vertex3fs fs
vertex3fs _                = return ()

assetFromFile :: FilePath -> IO ()
assetFromFile fp = do
  vals <- readFile fp
  vertex3fs $ fmap read $ join $ fmap words $ lines vals

displayPlane :: Ship -> GLfloat -> IO ()
displayPlane ship y = do
  let pos_ship = ship_pos ship
  translate $ Vector3 (x_pos pos_ship) (y_pos pos_ship) (z_pos pos_ship)
  color3f 0.5 0.5 0.6
  let inf = 1000
  renderPrimitive Quads $ vertex3fs $
   [ (-inf), y, (-inf)
   , (-inf), y,  inf
   , inf, y,  inf
   , inf, y, (-inf) ]

displayTokens :: IO ()
displayTokens = do
  color3f 0.1 0.1 1
  forM_ [(-300),(-290)..300] $ \ x ->
    forM_ [(-300),(-290)..300] $ \ z ->
      displayCube (Point3D x 0 z) 0.1

darken :: Color3 GLdouble -> Color3 GLdouble
darken (Color3 r g b) = Color3 (0.9 * r) (0.9 * g) (0.9 * b)

displayTrap :: Trap -> IO ()
displayTrap trap = do
  color $ trap_col trap
  displayCube (trap_pos trap) (size trap)
  color $ darken $ trap_col trap
  displayCubeFrame (trap_pos trap) (size trap)

displayCubeFrame :: Point3D -> GLfloat -> IO ()
displayCubeFrame pt w = preservingMatrix $ do
  translate $ toVector3 pt
  renderPrimitive Lines $ vertex3fs
    [ w,-w, w,  w, w, w,   w, w, w, -w, w, w,
     -w, w, w, -w,-w, w,  -w,-w, w,  w,-w, w,
      w,-w, w,  w,-w,-w,   w, w, w,  w, w,-w,
     -w, w, w, -w, w,-w,  -w,-w, w, -w,-w,-w,
      w,-w,-w,  w, w,-w,   w, w,-w, -w, w,-w,
     -w, w,-w, -w,-w,-w,  -w,-w,-w,  w,-w,-w ]

displayCube :: Point3D -> GLfloat -> IO ()
displayCube pt w = preservingMatrix $ do
  translate $ toVector3 pt
  renderPrimitive Quads $ vertex3fs
     [ w, w, w,  w, w,-w,  w,-w,-w,  w,-w, w,
       w, w, w,  w, w,-w, -w, w,-w, -w, w, w,
       w, w, w,  w,-w, w, -w,-w, w, -w, w, w,
      -w, w, w, -w, w,-w, -w,-w,-w, -w,-w, w,
       w,-w, w,  w,-w,-w, -w,-w,-w, -w,-w, w,
       w, w,-w,  w,-w,-w, -w,-w,-w, -w, w,-w ]


displayShip :: Ship -> IO ()
displayShip ship = do
  translate $ toVector3 $ ship_pos ship
  rotate (-90 :: GLfloat) $ Vector3 0 1 0
  translate $ Vector3 0 1 (0 :: GLfloat)
  rotate (-90 :: GLfloat) $ Vector3 1 0 0
  rotate (180 * angle ship / 3.14159) $ Vector3 0 0 1
  color3f 0.7 0.5 0.6
  renderPrimitive Quads $ assetFromFile "../assets/body.raw"
  color3f 0.7 0.2 0.2
  renderPrimitive Quads $ assetFromFile "../assets/wings.raw"
  color3f 0.7 0.5 0.6
  renderPrimitive Triangles $ assetFromFile "../assets/nose.raw"

