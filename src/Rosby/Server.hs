{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Rosby.Server where

import Conferer
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Text (Text())
import qualified Data.Text as T
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

server :: Server ()
server = do
  (Context configs) <- ask
  $(logDebug) "Rosby, reporting for duty"
  rosbyConfigs :: RosbyConfig <- liftIO $ getFromRootConfig configs
  $(logDebug) ("Host " <> (_contextHost rosbyConfigs))
  liftIO $ hFlush stdout
  -- TODO (james): replace this with an actual loop
  liftIO $ loop
  where
    loop = do
      cmd <- getLine
      unless (cmd == "quit") $ loop

start :: IO ()
start = do
  config <- defaultConfig "rosby"
  let ctx = Context config
  runStderrLoggingT $ runReaderT (runServer server) ctx
