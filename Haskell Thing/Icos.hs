module Icos where

import SDL 
import Foreign.C (CInt)
import Data.Array
import Data.List as List
import Data.Maybe
import Control.Monad (unless)

type V3D = V3 Double
data Camera = Camera Double (V3D -> V3D)
data CameraPoint = CameraPoint {focusDepth :: Double, camPosition :: V3D, 
  camYaw :: Double,
  camPitch :: Double}
  --camDirection :: Quaternion Double}

--data about a vertex
data VertexProjection = VP {worldCoords :: (V3D), --(nonrotated w.r.t. camera) position in 3D
  localCoords :: (V2 CInt), --projected position on screen
  cameraDistance :: Double, --distance to camera
  cameraDot :: Double       --dot of position with projection plane
}
--an array of vertices and associated tris
data ObjectMesh = ObjectMesh {
  objectVertices :: (Array Int V3D),
  objectTopo :: [(Int, Int, Int)]
}

--point on sphere
sphereParam s t = fmap (/(1 + s^2 + t^2)) $ V3 (2*s) (2*t) (1 - s^2 - t^2)

--half-turns, with interpolation which normalizes nicely when doubled
halfTurnToCircle = circleParam . interPoly where 
  circleParam t = fmap (/(1 + t^2)) $ V2 (1 - t^2) (2*t)
  interPoly t = (a*t*t + (1-a))*t
  a = 4 - 8*(sqrt 2)/3

--approximate turns on a unit circle (interval -1 to 1)
turnToCircle = doubleTurn . halfTurnToCircle where 
  doubleTurn (V2 c s) = V2 (c^2 - s^2) (2*s*c)

--camera at (x,z) = (0, 5) facing the negative x axis
defaultCamera = CameraPoint 5 (V3 0 0 5) 0 0

--yaw the camera (i.e., rotate it parallel to the xz plane)
yawCamera (CameraPoint p pos yaw pitch) t = CameraPoint p pos yaw' pitch where 
  newyaw = yaw + t
  yaw' | newyaw >  1  = newyaw - 2
       | newyaw < -1  = newyaw + 2
       | otherwise    = newyaw

--move the camera toward the direction it faces along the distance d
moveCameraInto (CameraPoint p pos yaw pitch) d = CameraPoint p pos' yaw pitch where 
  pos' = pos + fmap (d*) rot
  rot = qpq_1 (V3 1 0 0) pitch $ qpq_1 (V3 0 1 0) (-yaw) $ V3 0 0 (-1)

getTransformer (CameraPoint p pos yaw pitch)
  = Camera p (rotate . translate) where 
    translate    = (+(-pos))
    rotate = qpq_1 (V3 1 0 0) pitch . qpq_1 (V3 0 1 0) yaw

qpq_1 v t p          = b where
  SDL.Quaternion a b = q*(SDL.Quaternion 0 p)*q_1
  (q, q_1)           = let quat = SDL.Quaternion c . (*(SDL.V3 s s s))
                       in (quat v, quat (-v))
  SDL.V2 c s         = halfTurnToCircle t --rotation "angle"

--(Maybe) project a vertex onto a camera. 
--If the projection plane lies past the focus, return Nothing
vertexProjection (Camera p rotate) vertex 
  | quadToFocus > quadToPlane = Just $ VP vertex local quadToFocus z
  | otherwise                 = Nothing where
    V3 x y z    = rotate vertex 
    --points in projection plane
    x' = x*projCoeff
    y' = y*projCoeff
    --quadrances to in projection plane
    quadToFocus = x^2 + y^2 + (z - p)^2
    quadToPlane = (x - x')^2 + (y - y')^2 + z^2

    projCoeff   = p/(p - z)
    local       = (+V2 200 100) . fmap (floor . (*200) . (+1)) $ V2 x' y'

drawObject renderer camera (ObjectMesh vertices triIndex) 
  = flip mapM_ sortedTris (\(center, tri) -> do 
    let dot = cameraDot center / dotNormalizer
    --rendererDrawColor renderer $= V4 (floor $ 255*dot) 0 0 maxBound
    --unless (dot < 0) $ fillTri renderer $ map localCoords tri
    rendererDrawColor renderer $= V4 255 0 0 maxBound
    fillTri renderer $ map localCoords tri
    rendererDrawColor renderer $= V4 0 0 0 maxBound
    wireframeTri renderer $ map localCoords tri
  )
  where vertices'      = fmap (vertexProjection camera) vertices
        getTri (x,y,z) = do x' <- vertices'!x
                            y' <- vertices'!y
                            z' <- vertices'!z
                            let ret = [x', y', z']
                            let triCenter = sum $ map worldCoords ret
                            tri' <- vertexProjection camera triCenter
                            return (tri', ret)
        dotNormalizer  = maximum $ map (cameraDot . fst) sortedTris
        sortedTris     = sortOn (negate . cameraDistance . fst) $ map (maybe undefined id) $ filter (isJust) $ map getTri triIndex
        

--TODO: make the local coordinates more general 
--Assumsing the viewpane has extent in all axes, (-1,1):(-1:1)
wireframeTri renderer tri = mapM_ (uncurry $ drawLine renderer) cycleTri
  where cycleTri  = take 4 $ (zip <*> tail) $ cycle $ map P tri

fillTri renderer [a,b,c] = flip mapM_ [0,0.005..1] $ (\i ->
    let t = V2 i i in
    drawLine renderer (P a) (P $ fmap floor $ (1 - t)*b' + t*c')
  ) where [b', c'] = map (fmap fromIntegral) [b,c]

phi = (/2) $ 1 + sqrt 5

icos = concat [
  map (\[x,y] -> V3 0       x       (y*phi)) pms,
  map (\[x,y] -> V3 x       (y*phi) 0      ) pms,
  map (\[x,y] -> V3 (y*phi) 0       x      ) pms ] where
  pms = sequence (replicate 2 [1,-1])

dist = (quadrance .) . (-)
shortest = minimum [head icos `dist` i | i <- tail icos]
isEdge a b = (<= 1e-5) $ abs $ (dist a b) - shortest

--tris in an icosahedron
icosahedron = ObjectMesh icos_array tris where 
  icos_ind = zip icos [0..]
  tris     = [(i1, i2, i3) | (v1,i1) <- icos_ind, 
                             (v2,i2) <- icos_ind, isEdge v1 v2, 
                             (v3,i3) <- icos_ind, isEdge v2 v3, isEdge v1 v3]
  icos_array = listArray (0, length icos) icos 

icosahedron2 = ObjectMesh (fmap (+V3 10 0 10) vert) topo where
  ObjectMesh vert topo = icosahedron
