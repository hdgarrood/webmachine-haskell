module Main where

import Happstack.Server
import App.Routes (routes)
import App.Config (conf)

main :: IO ()
main = simpleHTTP conf routes
