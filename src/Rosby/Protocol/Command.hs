module Rosby.Protocol.Command where

import Data.ByteString (ByteString)
import Rosby.Protocol.Serial

newtype Key = Key { unKey :: ByteString }
  deriving (Eq, Show)

data Command
  = Set Key ByteString
  | Get Key
  | Delete Key
  deriving (Eq, Show)

fromPrim :: Primitive -> Either String Command
fromPrim (Array _ []) = Left "Command must not be an empty array"
fromPrim (Array _ ((Str len bytes):ps)) =
  case bytes of
    "SET" -> parseSet ps
    "GET" -> parseGet ps
    "DEL" -> parseDelete ps
    _     -> Left "Unrecognized command"
fromPrim (Array _ _) = Left "Unrecognized command"
fromPrim _ = Left "Command must be in an array format"

parseSet :: [Primitive] -> Either String Command
parseSet [] = Left "Way too few"
parseSet ((Str _ y):(Str _ z):_) = Right $ Set (Key y) z
parseSet _ = Left "Hooha"

parseGet :: [Primitive] -> Either String Command
parseGet [] = Left "Way too few"
parseGet ((Str _ y):_) = Right $ Get (Key y)
parseGet _ = Left "Hooha"

parseDelete :: [Primitive] -> Either String Command
parseDelete [] = Left "Way too few"
parseDelete ((Str _ y):_) = Right $ Delete (Key y)
parseDelete _ = Left "Hooha"
