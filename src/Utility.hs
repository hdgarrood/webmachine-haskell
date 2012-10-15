module Utility where

import qualified Data.ByteString.Lazy as BS

fromStrict bs = BS.fromChunks [bs]
