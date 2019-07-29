#!/usr/bin/env stack
-- stack --resolver lts-13.7 script
data Tree = Leaf | Node Int Tree Tree deriving Show

checkSort :: Tree -> Bool
checkSort tree = checkSort' tree Nothing Nothing
    where checkSort' Leaf _ _ = True
          checkSort' (Node value leftTree rightTree) minVal maxVal =
            let leftSorted = checkSort' leftTree minVal (Just value)
                rightSorted = checkSort' rightTree (Just value) maxVal
            in (case minVal of Nothing -> True; Just x -> value >= x) &&
              (case maxVal of Nothing -> True; Just x -> value < x) &&
              leftSorted && rightSorted

main :: IO ()
main = do
    print $ checkSort (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf))
