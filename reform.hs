--{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
--    ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
--{-# OPTIONS_GHC -F 

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import qualified Data.Map as M

import Happstack.Server
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import DefaultLayout (defaultLayout)

testLayout :: ServerPart Response
testLayout = ok $ toResponse $ defaultLayout $ M.fromList
    [
        ("body", H.h1 "Hello World!!")
    ]

main :: IO ()
main = simpleHTTP nullConf $ testLayout
