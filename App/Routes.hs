{-# LANGUAGE OverloadedStrings #-}

module App.Routes where

import Data.Text (Text, append)
import Control.Monad
import Happstack.Server

routes :: ServerPart Response
routes = resources

-- like the resources method in Rails; should provide 7 RESTful actions
-- for a resource.
resources :: ServerPart Response
resources = msum
    [ nullDir >> msum
        [ method GET >> indexR
        , method POST >> createR
        ]
    , dir "new" $ nullDir >> newR
    , path $ \id -> msum
        [ nullDir >> showR id
        ]
    ]

simpleResponse :: Text -> ServerPart Response
simpleResponse text = ok $ toResponse text

indexR :: ServerPart Response
indexR = simpleResponse "A list of thingies\n"

newR :: ServerPart Response
newR = simpleResponse "Form to make a new thingy with\n"

createR :: ServerPart Response
createR = simpleResponse "Created a thingy\n"

showR :: Text -> ServerPart Response
showR id_ = simpleResponse (append (append "Here's a thingy: " id_) "\n")

editR :: ServerPart Response
editR = simpleResponse "Form to edit a thingy\n"

updateR :: ServerPart Response
updateR = simpleResponse "Updated a thingy\n"

destroyR :: ServerPart Response
destroyR = simpleResponse "Deleted a thingy\n"
