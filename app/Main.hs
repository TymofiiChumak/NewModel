module Main where
import System.Environment
import Graphics.Gloss.Interface.Pure.Game
import Parser
import Vector
import Data.Array 
import Data.List



displaySize :: Int
displaySize = 350

-- | description of frame
window :: Display
window = InWindow "New window" (2 * displaySize, 2 * displaySize) (10, 10)


-- | collor of background
background :: Color
background = white

-- | description of models shape 
data FiguresDescription = FiguresDescription {
                              points :: [Point3D],
                              triangles :: [Triangle],
                              lightVec :: Point3D,
                              distanceToDisplay :: Int}

-- | description of models movement                               
data MotionCharacteristic = MotionCharacteristic {
                                   isRotatedXZ :: Float,
                                   isRotatedYZ :: Float,
                                   isScaled :: Int,
                                   mousePosition :: (Bool,(Float,Float),(Float,Float))}
-- | model consists of figure and movemet
data Model = Model FiguresDescription MotionCharacteristic                 
                    
-- | draw a icture from model
render :: Model -> Picture
render (Model (FiguresDescription {points=points,
                       triangles=triangles, 
                       lightVec=light,
                       distanceToDisplay=distance}) _) = pictures (map drawPoint triangles)
                       where
                            drawPoint = (drawOne  (listArray (1,length points)  points) light distance)




-- | update Model: set new value of rtation at XZ plane
changeRotationXZ ::  Float -> Model -> Model  
changeRotationXZ newValue
    (Model figuresDescription (MotionCharacteristic {
                                                isRotatedXZ = angle1, 
                                                isRotatedYZ = angle2, 
                                                isScaled = prop,
                                                mousePosition = position})) = 
        (Model figuresDescription (MotionCharacteristic {
                                                isRotatedXZ = newValue, 
                                                isRotatedYZ = angle2, 
                                                isScaled = prop,
                                                mousePosition = position}))

-- | update Model: set new value of rtation at YZ plane                                                
changeRotationYZ ::  Float -> Model -> Model  
changeRotationYZ newValue
    (Model figuresDescription (MotionCharacteristic {
                                                isRotatedXZ = angle1,
                                                isRotatedYZ = angle2, 
                                                isScaled = prop,
                                                mousePosition = position})) = 
        (Model figuresDescription (MotionCharacteristic {
                                                isRotatedXZ = angle1, 
                                                isRotatedYZ = newValue, 
                                                isScaled = prop,
                                                mousePosition = position}))

-- | update Model: set new value of scaling proportion                                               
changeScale :: Int -> Model -> Model
changeScale newValue
    (Model figuresDescription (MotionCharacteristic {
                                                isRotatedXZ = angle1, 
                                                isRotatedYZ = angle2, 
                                                isScaled = prop,
                                                mousePosition = position})) = 
        (Model figuresDescription (MotionCharacteristic {
                                                isRotatedXZ = angle1, 
                                                isRotatedYZ = angle2, 
                                                isScaled = newValue,
                                                mousePosition = position}))

-- | change distance to veiw point                                                
changeDistance :: Int -> Model -> Model
changeDistance value (Model 
                (FiguresDescription {
                        points=points, 
                        triangles=triangles, 
                        lightVec=light, 
                        distanceToDisplay = distance}) 
                motionCharacteristic) =
        Model (FiguresDescription {
                        points=newPoints, 
                        triangles=triangles, 
                        lightVec=light, 
                        distanceToDisplay = distance}) motionCharacteristic
                                where
                                        newPoints =  case value of
                                                1 ->  map (scalePoint (1.1)) points
                                                0 -> points
                                                -1 -> map (scalePoint (0.9)) points

-- | set mouse position when left button of mouse is pressed                        
initMousePosition :: Bool -> (Float,Float) -> Model -> Model
initMousePosition state (x,y) (Model figuresDescription (MotionCharacteristic {
        isRotatedXZ = angle1, 
        isRotatedYZ = angle2, 
        isScaled = prop,
        mousePosition = _ })) = 
                Model figuresDescription (MotionCharacteristic {
                                        isRotatedXZ = angle1, 
                                        isRotatedYZ = angle2, 
                                        isScaled = prop,
                                        mousePosition = (state,(x,y),(x,y))})

-- | change position of mouse when it wos moved
changeMousePosition :: (Float, Float) -> Model -> Model
changeMousePosition (x,y) (Model figuresDescription (MotionCharacteristic {
                        isRotatedXZ = angle1, 
                        isRotatedYZ = angle2, 
                        isScaled = prop,
                        mousePosition = (state,prevPos,(prevX,prevY)) })) = 
                                (Model figuresDescription (MotionCharacteristic {
                                        isRotatedXZ = angle1, 
                                        isRotatedYZ = angle2, 
                                        isScaled = prop,
                                        mousePosition = (state,p1,p2)}))
                                where
                                        p1 = if state == True then (prevX,prevY) 
                                                else prevPos
                                        p2 = if state == True then (x,y)
                                                else (prevX,prevY)
                                                           

-- | rotate model according to mouse
moveByMouse :: Bool -> (Float,Float) -> (Float,Float) -> Point3D-> Point3D
moveByMouse state (prevX,prevY) (newX,newY) point = if state == True then (rotatePointXZ deltaX).(rotatePointYZ deltaY) $ point
                                                        else point
                                        where 
                                                deltaX = -(pi/360)*(prevX - newX)
                                                deltaY = -(pi/360)*(prevY - newY)

-- | handles pushed keys, movement of mous and mouses wheel
handleKeys :: Event -> Model -> Model
handleKeys (EventKey (SpecialKey KeyUp) (x) _ _) model = changeRotationYZ value model
        where  value = if x == Up then 0.0  else 1.0
handleKeys (EventKey (SpecialKey KeyDown) (x) _ _) model = changeRotationYZ value model        
        where  value = if x == Up then 0.0  else -1.0
handleKeys (EventKey (SpecialKey (KeyRight)) (x) _ _) model = changeRotationXZ value model
        where  value = if x == Up then 0.0  else 1.0
handleKeys (EventKey (SpecialKey (KeyLeft)) (x) _ _) model = changeRotationXZ value model
        where  value = if x == Up then 0.0  else -1.0
handleKeys (EventKey (Char 'w') (x) _ _) model = changeScale value model
        where  value = if x == Up then 0  else 1
handleKeys (EventKey (Char 's') (x) _ _) model = changeScale value model
        where  value = if x == Up then 0  else -1
handleKeys (EventKey (MouseButton LeftButton) d _ (x, y)) model = initMousePosition state (x,y) model
        where state = if d == Down then True else False
handleKeys (EventMotion (x,y)) model = changeMousePosition (x,y) model
handleKeys (EventKey (MouseButton WheelUp) x _ _) model = changeDistance 1 model
handleKeys (EventKey (MouseButton WheelDown) x _ _) model = changeDistance (-1) model
handleKeys _ world = world

-- | updates points according to current motion caracteristic
update :: Float -> Model -> Model
update _  (Model (FiguresDescription 
                    {points=points, 
                     triangles=triangles,
                     lightVec=light,
                     distanceToDisplay = distance})
                 (MotionCharacteristic 
                    {isRotatedXZ = angle1, 
                    isRotatedYZ = angle2, 
                    isScaled = prop,
                    mousePosition = (state,prev,new)})) = 
                        Model (FiguresDescription 
                                {points=newPoints4, 
                                triangles=triangles,
                                lightVec=light,
                                distanceToDisplay = distance})
                            (MotionCharacteristic 
                                {isRotatedXZ = angle1, 
                                isRotatedYZ = angle2, 
                                isScaled = prop,
                                mousePosition = (state,prev,new)})
                        where
                            newPoints1 = map (rotatePointXZ ((pi/36) * angle1)) points
                            newPoints2 = map (rotatePointYZ ((pi/36) * angle2)) newPoints1
                            newPoints3 = case prop of
                                1 ->  map (scalePoint (1.1)) newPoints2
                                0 -> newPoints2
                                -1 -> map (scalePoint (0.9)) newPoints2
                            newPoints4 = map (moveByMouse state prev new ) newPoints3

-- | move figure to centr of coordinates
toCenter :: [Point3D] -> [Point3D]
toCenter xs = map (\(x,y,z) -> (x - avX,y - avY,z - avZ)) xs
        where
                len =fromIntegral (length xs)
                (sumX,sumY,sumZ) = foldr (\(x,y,z) (sx,sy,sz) -> (sx + x,sy + y, sz + z)) (0,0,0) xs
                (avX,avY,avZ) = (sumX/len,sumY/len,sumZ/len)






main = do 
        (fileName:_) <- getArgs
        contest <- readFile fileName  
        points_triangles <- return (parser' (lines  contest))
        figuresDescription <- return (FiguresDescription {
                points = toCenter (reverse (fst points_triangles)),
                triangles = snd points_triangles,
                lightVec = (0.25,0,-1.0),
                distanceToDisplay = 500
                })
        motionCharacteristic <- return (MotionCharacteristic{
                isRotatedXZ = 0.0,
                isRotatedYZ = 0.0,        
                isScaled = 0,
                mousePosition = (False,(0,0),(0,0))
                })
        play window background 60 (Model figuresDescription motionCharacteristic) render  handleKeys update


