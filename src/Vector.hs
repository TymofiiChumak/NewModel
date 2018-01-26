-- | functions wich connected to vector calculations
module Vector(
             getColor,
             drawOne,
             rotatePointXZ,
             rotatePointYZ,
             scalePoint,
             convertPoint  
             )where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Data.Array 

type Triangle = (Int,Int,Int)
type Point3D = (Float,Float,Float)

-- | Draw on triangle. If triangle is in the back of figure it will not be painted.
-- Color of figure depends on position of triangle in relation to light vetor
drawOne :: Array Int Point3D -> Point3D -> Int -> Triangle -> Picture
drawOne points light distance (a, b, c) = 
    if (scalarProduct norm (0.0, 0.0, 1.0)) > 0.0 then Blank
    else Color (getColor (p1, p2, p3) light) (Polygon [ 
            convertPoint distance p1,
            convertPoint distance p2,
            convertPoint distance p3])
    where
        p1 =  points ! a
        p2 =  points ! b
        p3 =  points ! c
        norm = normalToTriangle (p1, p2, p3)

-- | choice of color according to relation to light vetor
getColor :: (Point3D, Point3D, Point3D) -> Point3D -> Color
getColor (a, b, c) ligth = makeColor proportion proportion proportion 1.0
    where
        norm = normalToTriangle (a, b, c)
        proportion = ((scalarProduct norm ligth) / ((lengthOfVector norm) * (lengthOfVector ligth))) * 0.5 +0.25 

-- | get a norm to triangle's plane        
normalToTriangle :: (Point3D, Point3D, Point3D) -> Vector3D
normalToTriangle (a, b, c) = crossProduct (lineToVector a b) (lineToVector a c)

-- | vector at three dimensions Cartesian coordinates
type Vector3D = Point3D        
lineToVector :: Point3D -> Point3D -> Vector3D
lineToVector (x1, y1, z1) (x2, y2, z2) = 
    (x2 - x1, y2 - y1, z2 - z1)

-- | cross product from two vctors
crossProduct :: Vector3D -> Vector3D -> Vector3D
crossProduct (x1, y1, z1) (x2, y2, z2) = 
    (y1 * z2 - y2 * z1,
     z1 * x2 - z2 * x1,
     x1 * y2 - x2 * y1)

-- | scalar product fom twoo vectors     
scalarProduct :: Vector3D -> Vector3D -> Float
scalarProduct (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | length of vector
lengthOfVector :: Vector3D -> Float
lengthOfVector (x, y, z) = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- | rotate point around Y0
rotatePointXZ ::  Float -> Point3D -> Point3D
rotatePointXZ angle (x,y,z) = (x', y, z')
    where
        x' = x * cos angle - z * sin angle
        z' = x * sin angle + z * cos angle

-- | rotate point around X0
rotatePointYZ ::  Float -> Point3D -> Point3D
rotatePointYZ angle (x,y,z) = (x, y', z')
    where
        y' = y * cos angle - z * sin angle
        z' = y * sin angle + z * cos angle
        
-- | increase lenth of vector to point
scalePoint :: Float -> Point3D -> Point3D
scalePoint d (x, y, z) = (x * d, y * d , z * d)

-- | convert point to coordinate of display
convertPoint :: Int -> Point3D -> (Float,Float)
convertPoint r0  (x, y, z) = (x , y )
