{-# LANGUAGE OverloadedStrings #-}

module App.Routes where

import Data.Text (Text)
import Control.Monad
import Happstack.Server

routes :: ServerPart Response
routes = resources

resources :: ServerPart Response
resources = msum
    [ nullDir >> index
    , dir "new" $ new
    ]

simpleResponse :: Text -> ServerPart Response
simpleResponse text = ok $ toResponse text

new :: ServerPart Response
new = simpleResponse "Form to make a new thingy with\n"

index :: ServerPart Response
index = simpleResponse "A list of thingies\n"

aroutes :: ServerPart Response
aroutes = method [GET, HEAD] >> subRoutes

subRoutes :: ServerPart Response
subRoutes = msum 
       [ do methodM GET
            ok $ toResponse ("You did a GET request.\n" :: Text)
         , do methodM POST
              ok $ toResponse ("You did a POST request.\n" :: Text)
         , dir "foo" $ do methodM GET
                          ok $ toResponse ("You did a GET request on /foo\n" :: Text)
       ]

