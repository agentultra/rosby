{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Rosby.Server where

import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans
import System.IO

newtype Server a = Server { runServer :: LoggingT IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadLogger
    , MonadIO
    )

server :: Server ()
server = do
  $(logDebug) "Rosby, reporting for duty"
  liftIO $ hFlush stdout
  liftIO $ loop
  where
    loop = do
      cmd <- getLine
      unless (cmd == "quit") $ loop

start :: IO ()
start = do
  runStderrLoggingT (runServer server)
