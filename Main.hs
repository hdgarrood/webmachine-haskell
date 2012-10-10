module Main where

import Happstack.Server
import App.Routes (routes)

main :: IO ()
main = simpleHTTP nullConf routes
