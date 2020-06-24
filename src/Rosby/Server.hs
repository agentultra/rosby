{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Rosby.Server where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import System.IO

data Context
  = Context
  { _contextHost :: String
  , _contextPort :: Int
  }
  deriving (Eq, Show)

newtype Server a = Server { runServer :: ReaderT Context (LoggingT IO) a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadReader Context
    , MonadIO
    )

server :: Server ()
server = do
  (Context host post) <- ask
  $(logDebug) "Rosby, reporting for duty"
  liftIO $ hFlush stdout
  liftIO $ loop
  where
    loop = do
      cmd <- getLine
      unless (cmd == "quit") $ loop

start :: IO ()
start = do
  let ctx = Context "localhost" 8989
  runStderrLoggingT $ runReaderT (runServer server) ctx
