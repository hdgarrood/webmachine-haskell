{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (head, id)
import Web.Scotty hiding (body, text)
import Network.Wai.Middleware.RequestLogger  (logStdoutDev)
import Network.Wai.Middleware.MethodOverridePost (methodOverridePost)
import Data.Monoid ((<>))

main = scotty 3030 $ do
    middleware methodOverridePost
    middleware logStdoutDev
    home

home = do
  get "/" $ html $ homepage $ []
  put "/" $ param "nickname" >>= html . homepage

homepage [ n ] = str n
homepage   _   = str ""

title = "Network.Wai.Middleware.MethodOverride Test"

str s = "<html><head><title>" <> title
      <> "</title></head><body><h1>" <> title
      <> "</h1><form method='post' action='/'><input type='hidden' name='_method' value='PUT' />"
      <> "<input type='text' name='nickname' value='" <> s
      <> "' /><input type='submit' value='Submit Test' /></form></body></html>"
