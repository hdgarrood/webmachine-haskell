{-# LANGUAGE OverloadedStrings #-}

module App.Routes where

import Data.Text
import Control.Monad
import Happstack.Server

routes :: ServerPart Response
--routes = ok $ toResponse ("hello world!\n" :: String)

routes = msum $
    [
    ok $ toResponse $ ("You did a request.\n" :: Text)
--    , do methodM POST
--        ok "You did a POST request.\n"
--    , dir "foo" do
--        ok "You did a request on /foo.\n"
    ]
