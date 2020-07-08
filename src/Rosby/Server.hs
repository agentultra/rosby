module Rosby.Server where

import Conferer
import Control.Concurrent
import Control.Exception hiding (Handler)
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text (Text())
import qualified Data.Text as T
import Network.Socket
import qualified Network.Socket.ByteString as S
import System.IO

import Rosby.Config

data Context
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

handler :: Socket -> Handler ()
handler socket = do
  (Context configs) <- ask
  $(logDebug) "We has connection"

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
  logChan <- liftIO $ newChan
  rosbyConfigs :: RosbyConfig <- liftIO $ getFromRootConfig config
  let ctx = Context config
  let h = serverHost rosbyConfigs
  let p = serverPort rosbyConfigs
  addr <- liftIO $ resolve (T.unpack h) (show p)
  liftIO $ bracket (open addr) close (loop logChan ctx)
  where
    loop logChan ctx s = forever $ do
      (conn, _peer) <- accept s
      void $ forkFinally (doHandle conn logChan ctx) (const $ gracefulClose conn 5000)
    doHandle conn logChan ctx = runChanLoggingT logChan $ (runReaderT (runHandler $ handler conn) ctx)

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock $ setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  listen sock 1024
  return sock
