module Rosby.Config where

import Conferer
import Data.Text (Text())
import GHC.Generics (Generic)

-- We break from our record naming convention because of Conferer
data RosbyConfig
  = RosbyConfig
  { serverHost :: Text
  , serverPort :: Int
  }
  deriving (Generic)

instance FromConfig RosbyConfig


instance DefaultConfig RosbyConfig where
  configDef =
    RosbyConfig
    { serverHost = "localhost"
    , serverPort = 1993
    }
