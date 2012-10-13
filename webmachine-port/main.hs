{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Reader
import Safe (readMay)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8 (pack) 

import qualified Network.Wai as W
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp (run)

-- combines ability to read the request with doing IO actions
type ServerMonad = ReaderT W.Request IO

class Resource a where
    serviceAvailable :: a -> ServerMonad Bool
    serviceAvailable = const $ return True

    knownMethods :: a -> ServerMonad [H.Method]
    knownMethods = const $ return 
        [ "GET"
        , "HEAD"
        , "POST"
        , "PUT"
        , "DELETE"
        , "OPTIONS"
        , "TRACE"
        ]

    allowedMethods :: a -> ServerMonad [H.Method]
    allowedMethods = knownMethods

    toHtml :: a -> ServerMonad BS.ByteString
    toText :: a -> ServerMonad BS.ByteString

data PostsCollection = PostsCollection

newtype PostId = PostId Int deriving (Show, Read, Eq, Ord)
data Post = Post PostId

instance Resource PostsCollection where
    toText _ = return "PostsCollection: Here are all the posts.\n"

instance Resource Post where
    toText (Post postId) = return $
        "This is the page for post"
        `BS.append` (BS8.pack $ show $ postId)
        `BS.append` ".\n"

-- the main entry point. Takes a resource and returns a response
handle :: (Resource a) => a -> ServerMonad W.Response
handle res = do
    available <- serviceAvailable res
    if available
        then do
            knownMeths <- knownMethods res
            W.Request {W.requestMethod = rqMeth} <- ask
            if rqMeth `elem` knownMeths
                then do
                    allowedMeths <- allowedMethods res
                    if rqMeth `elem` allowedMeths
                        then do
                            text <- toText res
                            return $ W.responseLBS H.ok200 [] text
                        -- TODO: list of allowed methods in Accept header
                        else methodNotAllowed 
                else notImplemented
        else serviceUnavailable

methodNotAllowed :: ServerMonad W.Response
methodNotAllowed = return $ W.responseLBS H.status405 [] ""

notImplemented :: ServerMonad W.Response
notImplemented = return $ W.responseLBS H.status501 [] ""

serviceUnavailable :: ServerMonad W.Response
serviceUnavailable = return $ W.responseLBS H.status503 [] ""

app :: W.Application
app req = case W.rawPathInfo req of
            "/posts"    -> runReaderT (handle PostsCollection) req
            "/posts/1"  -> runReaderT (handle Post (PostId 1)) req
            _           -> return $ H.status404 [] "LULZ"

main :: IO ()
main = run 3000 app
--main :: IO ()
--main = HS.simpleHTTP HS.nullConf $ msum
--    [ HS.dir "posts" $ HS.nullDir >> handle PostsCollection
--    , HS.dir "posts" $ HS.path (\postId -> handle (Post postId))
--    ]
