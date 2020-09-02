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

data Context
  = Context
  { _contextConfig  :: Config
  , _contextLogChan :: Chan LogLine
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

newtype SocketDispatcher a = SocketDispatcher { runDispatcher :: ReaderT Context (LoggingT IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadIO
    , MonadReader Context
    )

handler :: Socket -> Handler ()
handler socket = do
  (Context configs _) <- ask
  $(logDebug) "We has connection"
  input <- liftIO $ S.recv socket 1024
  unless (B.null input) $ do
    let cmd = runParser input
    -- TODO (james): wire this up
    let response = Response Ok
    liftIO $ S.sendAll socket $ serializeResponse response
    handler socket

dispatcher :: Socket -> SocketDispatcher ()
dispatcher socket = do
  ctx@(Context _ logChan) <- ask
  $(logDebug) "Rosby!!!"
  liftIO $ forever $ do
    (conn, _peer) <- liftIO $ accept socket
    liftIO
      $ void
      $ forkFinally
      (runChanLoggingT logChan
       . flip runReaderT ctx
       . runHandler $ handler conn) (const $ gracefulClose conn 5000)

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
  config <- liftIO $ defaultConfig "rosby"
  logChan <- liftIO newChan
  rosbyConfigs :: RosbyConfig <- liftIO $ getFromRootConfig config
  let ctx = Context config logChan
  let h = serverHost rosbyConfigs
  let p = serverPort rosbyConfigs
  addr <- liftIO $ resolve (T.unpack h) (show p)
  $(logDebug) "Server starting up..."
  liftIO $ forkIO $ logger logChan
  liftIO $ bracket (open addr) close (doDispatcher logChan ctx . dispatcher)
  where
    doDispatcher logChan ctx =
      runChanLoggingT logChan
      . flip runReaderT ctx
      . runDispatcher

logger :: Chan LogLine -> IO ()
logger logChan = runStdoutLoggingT $ do
  unChanLoggingT logChan

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  listen sock 1024
  return sock
