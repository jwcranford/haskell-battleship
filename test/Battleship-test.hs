-- Battleship tests

import Battleship

-- hunit
import Test.HUnit

testCreateBoardWithCollisions =  
    let ship1 = Ship "A" 3 [(2,1), (2,2), (2,3)]
        ship2 = Ship "B" 3 [(1,2), (2,2), (3,2)]
        board = createBoard ((1,1),(3,3)) [ship1, ship2]
        colls = collisions board
    in assertEqual "collisions wrong" colls [(Collision ship1 ship2)]
    
testCreateBoardNoCollisions =
    let ship1 = Ship "A" 3 [(2,1), (2,2), (2,3)]
        ship2 = Ship "B" 3 [(1,1), (1,2), (1,3)]
        board = createBoard ((1,1),(3,3)) [ship1, ship2]
        colls = collisions board
    in assertBool "expected no collisions" (null colls)


-- variant of createBoard for testing
createSunkBoard :: (BoardIx,BoardIx) -> [Ship] -> Board
createSunkBoard = createBoard' True
  
testSunk =
    let ship1 = Ship "A" 3 [(2,1), (2,2), (2,3)]
        ship2 = Ship "B" 3 [(1,1), (1,2), (1,3)]
        b = createSunkBoard ((1,1),(3,3)) [ship1, ship2]
        sunkResults = map (\s -> sunk s $ board b) $ ships b
    in assertBool "both ships should be sunk" (and sunkResults)

testShootSinks =
    let ship1 = Ship "A" 3 [(2,1), (2,2), (2,3)]
        ship2 = Ship "B" 3 [(1,1), (1,2), (1,3)]
        b = createBoard ((1,1),(3,3)) [ship1, ship2]
        (True,b2) = shoot (1,1) b
        (True,b3) = shoot (1,2) b2
        (True, b4) = shoot (1,3) b3
  in assertBool ("should be sunk: " ++ show b4) $ sunk ship2 $ board b4
  

tests = test [ "createBoardWithCollisions" ~: testCreateBoardWithCollisions
               , "noCollisions" ~: testCreateBoardNoCollisions
               , "sunk" ~: testSunk
               , "shoot" ~: testShootSinks
             ]

main = runTestTT tests >> return ()

