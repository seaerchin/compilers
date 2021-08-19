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
  let parsed = parse expr "" "something"
  print parsed