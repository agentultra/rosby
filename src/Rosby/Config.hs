module Rosby.Config where

import Conferer
import Data.Text (Text())
import GHC.Generics (Generic)

data RosbyConfig
  = RosbyConfig
  { _contextHost   :: Text
  , _contextPort   :: Int
  }
  deriving (Generic)

instance FromConfig RosbyConfig


instance DefaultConfig RosbyConfig where
  configDef =
    RosbyConfig
    { _contextHost = "localhost"
    , _contextPort = 1993
    }
