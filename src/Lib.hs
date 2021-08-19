module Lib
  ( someFunc,
  )
where

import Regex
import Text.Parsec

someFunc :: IO ()
someFunc = putStrLn "someFunc"

test :: IO ()
test = do
  print "something"