module Rosby.Protocol.Command where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

toPrim :: Command -> Primitive
toPrim (Set (Key key) value) =
  let keyLen = BS.length key
      valLen = BS.length value
  in Array 3 [(Str 3 "SET"), (Str keyLen key), (Str valLen value)]
toPrim (Get (Key key)) =
  let keyLen = BS.length key
  in Array 2 [(Str 3 "GET"), (Str keyLen key)]
toPrim (Delete (Key key)) =
  let keyLen = BS.length key
  in Array 2 [(Str 3 "DEL"), (Str keyLen key)]

serializeCommand :: Command -> ByteString
serializeCommand = serialize . toPrim
