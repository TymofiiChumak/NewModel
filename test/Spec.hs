import Test.QuickCheck
import Test.HUnit
import Parser 

{-}  test main
testToCentr1 = TestCase (assertEqual "only one " [(1.615,8.731,-7.231)] (toCenter [(1.615,8.731,-7.231)]))
testToCentr2 = TestCase (assertEqual "three " [(2.0,-7.0,3.0),
                                               (4.0,5.0,2.0),
                                               (-6.0,2.0,-5.0)] 
                                        (toCenter [(1.0,-5.0,7.0),
                                                   (3.0,7.0,6.0),
                                                   (-7.0,4.0,-1.0)]))
testsTC = TestList [TestLabel "test1" testToCentr1,
                  TestLabel "test2" testToCentr2]

prop_toCentr :: [(Float,Float,Float)] -> Bool;
prop_toCentr xs = sumX == 0 && sumY == 0 && sumZ == 0
        where
                (sumX,sumY,sumZ) = foldr (\(x,y,z) (sx,sy,sz) -> (sx + x,sy + y, sz + z)) (0,0,0) xs                  

-- test psrser
--HUnit

testConvert1 = TestCase (assertEqual "/ - 2" "1845" (convert "1845/6515/6516" ))
testConvert2 = TestCase (assertEqual "/ - 1" "152" (convert "152/645 "))
testConvert3 = TestCase (assertEqual "/ - 0" "541" (convert "541 "))
testConvert4 = TestCase (assertEqual "nothing" "" (convert "/645 "))
testConvert5 = TestCase (assertEqual "nothing" "" (convert " "))
    
testsC = TestList [
        TestLabel "test1" testConvert1,
        TestLabel "test2" testConvert2,
        TestLabel "test3" testConvert3,
        TestLabel "test4" testConvert4,
        TestLabel "test5" testConvert5]

testParseTriangle1  = TestCase (assertEqual "1/2/3" (6516,651,65) (parseTriangle "6516/165/9864 951/491/651 65/6196/465 "))        
testParseTriangle2  = TestCase (assertEqual "1/2" (984,941,635) (parseTriangle "984/6351 941/615 635/61 "))
testParseTriangle3  = TestCase (assertEqual "1/3" (981,1289,984) (parseTriangle "981//3324 1289//374 984//1891 "))
testParseTriangle4  = TestCase (assertEqual "1" (6198,9498,984) (parseTriangle "6198 9498 984 "))

testsPT = TestList [
    TestLabel "test1" testParseTriangle1,
    TestLabel "test2" testParseTriangle2,
    TestLabel "test3" testParseTriangle3,
    TestLabel "test4" testParseTriangle4]



testParsePoint1 = TestCase(assertEqual "1" (16.691, 165.68, 71.18) (parsePoint "16.691 165.68 71.18 "))
testParsePoint2 = TestCase(assertEqual "2" (269.2, 894.6, 98.656) (parsePoint "-269.2 -894.6 -98.656 "))
testParsePoint3 = TestCase(assertEqual "3" (0.652, 0.981, 0.651) (parsePoint "0.652 0.981 0.651 "))


prop_convert1 :: Int -> Int -> Int -> Bool
prop_convert1 x y z = (show x) == (convert line)
        where
            line = (show x) ++ "/" ++ (show y) ++ "/" ++ (show z)

testsPP = TestList [
    TestLabel "test1" testParsePoint1,
    TestLabel "test2" testParsePoint2,
    TestLabel "test3" testParsePoint3]
-}
testParser1 = TestCase (assertEqual "points" ([
            ( 16.686701, -16.409100, -138.173096),
            (10.875000, -17.360001, -140.454590),
            (-17.189699, -25.368601, -135.428802),
            (-5.544940, -22.904800, -84.208015)],[])
                                 (parser' ["v 16.686701 -16.409100 -138.173096 ",
                                           "v 10.875000 -17.360001 -140.454590 ",
                                           "v -17.189699 -25.368601 -135.428802 ",
                                           "v -5.544940 -22.904800 -84.208015 "]))

testParser2 = TestCase (assertEqual "triangles" ([],[
            (212, 457, 351),
            (401, 307, 308),
            (235, 234, 363),
            (384, 478, 477)])
                                 (parser' ["f 212/192/212 457/447/457 351/335/351 ",
                                           "f 401/387/401 307/291/307 308/290/308 ",
                                           "f 235/215/235 234/214/234 363/347/363 ",
                                           "f 384/370/384 478/468/478 477/469/477 "]))

testParser3 = TestCase (assertEqual "nothing" ([],[])
                                         (parser' ["vt  0.545 0.436 0.000 ",
                                                   "vn  0.083 -0.881 0.465"]))
                                                   
testsP = TestList [
    TestLabel "test1" testParser1,
    TestLabel "test1" testParser2,
    TestLabel "test1" testParser3]                                            


prop_parseV :: Float -> Float -> Float -> Bool
prop_parseV x y z = ([(x,y,z)],[]) == parser' [line]
        where
            line = "v " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " "

prop_parseT :: Int -> Int -> Int -> Bool
prop_parseT x y z = ([],[(x,y,z)]) == parser' [line]
        where
            line = "f " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " "  

main :: IO ()            
main = do
    runTestTT testsP
    quickCheck prop_parseV
    quickCheck prop_parseT

