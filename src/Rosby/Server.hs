module Rosby.Server where

import Conferer
import Control.Exception
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

newtype Server a = Server { runServer :: ReaderT Context (LoggingT IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadReader Context
    , MonadIO
    )

server :: Socket -> Server ()
server socket = do
  (Context configs) <- ask
  $(logDebug) "Rosby, reporting for duty"
  liftIO $ hFlush stdout
  -- TODO (james): replace this with an actual loop
  liftIO $ loop
  where
    loop = do
      cmd <- getLine
      unless (cmd == "quit") $ loop

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
start = do
  config <- defaultConfig "rosby"
  rosbyConfigs :: RosbyConfig <- liftIO $ getFromRootConfig config
  let ctx = Context config
  let h = serverHost rosbyConfigs
  let p = serverPort rosbyConfigs
  addr <- resolve (T.unpack h) (show p)
  bracket (open addr) close $ \s -> do
    runStderrLoggingT $ runReaderT (runServer $ server s) ctx

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  setSocketOption sock ReuseAddr 1
  withFdSocket sock $ setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  listen sock 1024
  return sock
