module Rosby.Protocol.Response where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Rosby.Protocol.Serial

data Result
  = Ok
  | Fail ByteString
  deriving (Eq, Show)

data Response
  = Response
  { result :: Result
  }
  deriving (Eq, Show)

toPrim :: Response -> Primitive
toPrim (Response result) =
  case result of
    Ok -> Str 2 "Ok"
    Fail msg ->
      Array 2
      [ Str 4 "Fail"
      , Str (B.length msg) msg]

serializeResponse :: Response -> ByteString
serializeResponse = serialize . toPrim
