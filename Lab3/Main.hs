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
  print contents
  -- split the data into words and build an AA tree
  -- use foldl
  let tree = foldl (flip insert) emptyTree (words contents)
  print tree
  -- calculate and print statistics
  -- use fromIntegral/ceiling/logBase
  undefined

--------------------------------------------------------------------------------

