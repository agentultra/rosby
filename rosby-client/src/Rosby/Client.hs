module Rosby.Client where

import Data.ByteString (ByteString)
import Network.Socket

import Rosby.Protocol.Command
import Rosby.Protocol.Serial
import qualified Control.Exception as E

setKey :: ByteString -> ByteString -> Command
setKey key value = Set (Key key) value

getKey :: ByteString -> Command
getKey key = Get (Key key)

deleteKey :: ByteString -> Command
deleteKey key = Delete (Key key)

data RosbyClientConfig
  = RosbyClientConfig
  { _rosbyServerHost :: String
  , _rosbyServerPort :: String
  }
  deriving (Eq, Show)

defaultConfig :: RosbyClientConfig
defaultConfig = RosbyClientConfig "localhost" "1993"

runRosby :: RosbyClientConfig -> (Socket -> IO a) -> IO a
runRosby (RosbyClientConfig host port) client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock
