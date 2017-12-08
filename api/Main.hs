module Main where

import Control.Monad (msum)
import Happstack.Server (nullConf, simpleHTTP, ok, dir)

main :: IO ()
main = simpleHTTP nullConf $ msum [dir "hello" $ ok ("Hello, World!" :: String)]
