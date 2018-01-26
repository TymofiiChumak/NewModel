-- | parsing contest of .obj files to arrays of popints and triangles. Norm and texture are skipped
module Parser (
    Point3D,
    Triangle,
    parser'
    )where

import System.Environment
import System.IO.Error
import Control.Exception
import Test.HUnit
import Test.QuickCheck

-- | Point witn Cartesian coordinates at three dimensions
type Point3D =  (Float, Float, Float)
-- | Triangle described as three number of points
type Triangle = (Int, Int, Int)

-- | Main parsing function. 'v' - Vector 'f' - triangle
parser' :: [String] -> ([Point3D], [Triangle])
parser' contest = loop contest [] []
    where 
        loop [] points triangles = (points, triangles) 
        loop (x:xs) points triangles = case x of
            ('v' : ys) -> 
                if (head ys) == ' ' 
                    then loop xs ((parsePoint ys):points) triangles
                    else loop xs points triangles
            ('f' : ys) -> loop xs points ((parseTriangle ys): triangles)
            otherwice -> loop xs points triangles
     

-- | Parse line wich consists three nonintegral numbers to one Point3D
parsePoint :: String -> Point3D
parsePoint xs = (x, y, z)
    where
        (x:y:z:ys) = map read (words xs)



-- | Parse line with consists three integral numbers to Triangle. Norm and texture are skipped
parseTriangle :: String -> Triangle
parseTriangle xs = (x, y, z)
    where
        (x:y:z:ys) = map read (words xs)


-- | Skip all exept number of point
convert :: String -> String
convert (x:xs) = 
    if x == '/' || x ==' '
        then []
    else [x] ++  (convert xs)

