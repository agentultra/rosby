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
fromPrim prim =
  case prim of
    Array _ ((Str len bytes):ps) ->
      case bytes of
        "SET" -> parseSet ps
        "GET" -> parseGet ps
        "DEL" -> parseDelete ps
        _     -> Left "Unrecognized command"
    _ -> Left "Invalid command"

parseSet :: [Primitive] -> Either String Command
parseSet ((Str _ y):(Str _ z):_) = Right $ Set (Key y) z
parseSet _ = Left "Invalid set command"

parseGet :: [Primitive] -> Either String Command
parseGet ((Str _ y):_) = Right $ Get (Key y)
parseGet _ = Left "Invalid get command"

parseDelete :: [Primitive] -> Either String Command
parseDelete ((Str _ y):_) = Right $ Delete (Key y)
parseDelete _ = Left "Invalid delete command"
