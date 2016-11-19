module Examples where


import LoebSheet

import Data.Char (isLower)
import Control.Compose ((:.)(..), unO)


-- | Prints:
-- [Just 0.3, Just 1000.0, Just 1.3, Just 300.0]
sheet1 :: [Maybe Float]
sheet1 = eval
    [ Item 0.3
    , Item 1000
    , At 0 + 1.0
    , At 0 * At 1
    ]


-- | Prints:
-- [Just "Hello", Just "Hello World!", Just "elloorld", Nothing, Just "World!"]
sheet2 :: [Maybe String]
sheet2 = eval
    [ Item "Hello"
    , Fn2 (\x y -> x ++ " " ++ y) (At 0) (At 4)
    , Fn1 (filter isLower) $ At 1
    , At 1000
    , Item "World!"
    ]


-- | Prints (with take 10):
-- [Just 0,Just 1,Just 1,Just 2,Just 3,Just 5,Just 8,Just 13,Just 21,Just 34]
fibSheet :: [Maybe Int]
fibSheet = eval $ 0 : 1 : recsheet 0
    where recsheet x = At x + At (x+1) : recsheet (x+1)


-- | Prints (with take 5 . fmap (take 5) and some sensible printing):
-- 1 1 1 1 1
-- 1 2 3 4 5
-- 1 3 6 10 15
-- 1 4 10 20 35
-- 1 5 15 35 70
pythagorasTriangle :: [[Maybe Int]]
pythagorasTriangle = unO . eval . O $ repeat (Item 1) : recColumns 1
    where addElems (x,y) = At (x-1,y) + At (x,y-1) : addElems (x,y+1)
          recColumns x = (1 : addElems (x,1)) : recColumns (x+1)


-- | Printing function to be used for pythagorasTringle to make it nicer looking
printPythagorasTriangle = putStrLn . unlines . fmap (concat . intersperse " " . fmap (show . fromJust))


-- | Prints after running it through runsheetIO:
-- HiHi
sheetIO1 :: [Maybe (IO ())]
sheetIO1 = eval
    [ Item $ putChar 'H'
    , Item $ putChar 'i'
    , Fn2 (>>) (At 0) (At 1)
    , Item $ putChar '\n'
    ]


-- | Runs the IOs inside [Maybe (IO a)]
runSheetIO :: [Maybe (IO a)] -> IO ()
runSheetIO sheet = sequence_ $ fmap sequence_ sheet
