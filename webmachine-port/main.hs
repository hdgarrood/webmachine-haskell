{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (msum)
import Safe (readMay)
import qualified Data.Text as T

import qualified Happstack.Server as HS
import Happstack.Server.Types (Method(..))

class Resource a where
    serviceAvailable :: a -> HS.ServerPart Bool
    serviceAvailable = const (return True) 

    knownMethods :: a -> HS.ServerPart [HS.Method]
    knownMethods _ = return [GET, HEAD, POST, PUT, DELETE, OPTIONS, TRACE]

    allowedMethods :: a -> HS.ServerPart [HS.Method]
    allowedMethods a = knownMethods a

    toHtml :: a -> HS.ServerPart T.Text
    toText :: a -> HS.ServerPart T.Text

data PostsCollection = PostsCollection

newtype PostId = PostId Int deriving (Show, Read, Eq, Ord)
data Post = Post PostId

instance HS.FromReqURI PostId where
    fromReqURI = readMay

instance Resource PostsCollection where
    toText _ = return "PostsCollection: Here are all the posts."

instance Resource Post where
    toText (Post postId) = return $ "This is the page for post" `T.append` (T.pack . show $ postId)

-- the main entry point. Takes a resource and returns a response
handle :: (Resource a) => a -> HS.ServerPart HS.Response
handle res = do
    available <- serviceAvailable res
    if available
        then do
            knownMeths <- knownMethods res
            HS.Request {HS.rqMethod = rqMeth} <- HS.askRq
            if HS.matchMethod knownMeths rqMeth
                then do
                    allowedMeths <- allowedMethods res
                    if HS.matchMethod allowedMeths rqMeth
                        then do
                            text <- toText res
                            return $ HS.toResponse text
                        -- TODO: list of allowed methods in Accept header
                        else methodNotAllowed 
                else notImplemented
        else serviceUnavailable

methodNotAllowed :: HS.ServerPart HS.Response
methodNotAllowed = HS.resp 405 (HS.toResponse T.empty)

notImplemented :: HS.ServerPart HS.Response
notImplemented = HS.resp 501 (HS.toResponse T.empty)

serviceUnavailable :: HS.ServerPart HS.Response
serviceUnavailable = HS.resp 503 (HS.toResponse T.empty)

main :: IO ()
main = HS.simpleHTTP HS.nullConf $ msum
    [ HS.dir "posts" $ HS.nullDir >> handle PostsCollection
    , HS.dir "posts" $ HS.path (\postId -> handle (Post postId))
    ]
