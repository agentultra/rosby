module Rosby.Server where

import Conferer
import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Text (Text())
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Socket
import qualified Network.Socket.ByteString as S
import System.IO

import Rosby.Config
import Rosby.Protocol.Serial
import Rosby.Protocol.Command
import Rosby.Protocol.Response

newtype Context
  = Context
  { _contextConfig :: Config
  }

newtype Handler a = Handler { runHandler :: ReaderT Context (LoggingT IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadReader Context
    , MonadIO
    )

newtype SocketDispatcher a = SocketDispatcher { runDispatcher :: LoggingT IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadIO
    )

handler :: Socket -> Handler ()
handler socket = do
  (Context configs) <- ask
  $(logDebug) "We has connection"
  input <- liftIO $ S.recv socket 1024
  unless (B.null input) $ do
    let cmd = runParser input
    let response = Response Ok
    liftIO $ S.sendAll socket $ serializeResponse response
    handler socket

dispatcher :: Chan LogLine -> Context -> Socket -> SocketDispatcher ()
dispatcher logChan ctx socket = do
  liftIO $ forever $ runStdoutLoggingT $ do
    $(logDebug) "ROSBY!!!!"
    (conn, _peer) <- liftIO $ accept socket
    liftIO $ void $ forkFinally (doHandle conn logChan ctx) (const $ gracefulClose conn 5000)
  where
    doHandle conn logChan = runChanLoggingT logChan . runReaderT (runHandler $ handler conn)

type Port = String

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve host port = do
  let hints
        = defaultHints
        { addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
  head <$> getAddrInfo (Just hints) (Just host) (Just port)

start :: IO ()
start = withSocketsDo $ runStdoutLoggingT $ do
  $(logDebug) "Rosby, reporting for duty!"
  config <- liftIO $ defaultConfig "rosby"
  logChan <- liftIO newChan
  rosbyConfigs :: RosbyConfig <- liftIO $ getFromRootConfig config
  let ctx = Context config
  let h = serverHost rosbyConfigs
  let p = serverPort rosbyConfigs
  addr <- liftIO $ resolve (T.unpack h) (show p)
  liftIO $ bracket (open addr) close (runStdoutLoggingT . runDispatcher . dispatcher logChan ctx)

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  listen sock 1024
  return sock
