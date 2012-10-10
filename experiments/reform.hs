--{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
--    ScopedTypeVariables, TypeFamilies, TypeSynonymInstances #-}
--{-# OPTIONS_GHC -F 

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text
import qualified Data.Map as M

import Happstack.Server
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
--import qualified Text.Reform.Blaze.Common as R
import qualified Text.Reform.Blaze.Text as R

import DefaultLayout (defaultLayout)

type SimpleForm = 

data Message = Message { name :: Text,
                         title :: Text,
                         contents :: Text }

simpleForm :: SimpleForm Message
simpleForm = R.form
    (A.enctype "multipart/form-data" ! A.method "POST")
    [("","")]
    (R.inputText "hello")

testLayout :: ServerPart Response
testLayout = ok $ toResponse $ defaultLayout $ M.fromList
    [
        ("body", simpleForm)
    ]

main :: IO ()
main = simpleHTTP nullConf $ testLayout
