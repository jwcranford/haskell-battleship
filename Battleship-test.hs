-- Battleship tests

import Battleship
import Test.HUnit

testCreateBoardWithCollisions =  
    let ship1 = Ship 3 [(2,1), (2,2), (2,3)]
        ship2 = Ship 3 [(1,2), (2,2), (3,2)]
        board = createBoard ((1,1),(3,3)) [ship1, ship2]
        colls = collisions board
    in assertBool "expected collisions" (not (null colls))
    
testCreateBoardNoCollisions =
    let ship1 = Ship 3 [(2,1), (2,2), (2,3)]
        ship2 = Ship 3 [(1,1), (1,2), (1,3)]
        board = createBoard ((1,1),(3,3)) [ship1, ship2]
        colls = collisions board
    in assertBool "expected no collisions" (null colls)
    
tests = test [ "createBoardWithCollisions" ~: testCreateBoardWithCollisions,
                "noCollisions" ~: testCreateBoardNoCollisions]
