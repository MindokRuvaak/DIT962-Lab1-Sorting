{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- getContents
  -- print $ length $ words contents -- debug print

  -- split the data into words and build an AA tree
  let tree = foldl (flip insert) emptyTree (words contents)
  -- print tree -- debug print
  
  -- calculate and print statistics
  let optimalHeight = ceiling (logBase 2 (fromIntegral (length (words contents) + 1))) - 1
  putStrLn $ "Size: " ++ show (size tree) 
    ++ "\nHeight: " ++ show (height tree) 
    ++ "\nOptimal Height: " ++ show optimalHeight
    ++ "\nHeight / Optimum height: " ++ show (fromIntegral (height tree) / fromIntegral optimalHeight)
    ++ "\ncheckTree: " ++ show (checkTree tree)
    ++ "\nFirst 20 words: " ++ show (take 20 (inorder tree))


--------------------------------------------------------------------------------
