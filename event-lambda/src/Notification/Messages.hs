module Notification.Messages
    ( NotificationRecord(..),
    ) where

import Data.Aeson 
import GHC.Generics
-- import Data.UUID (UUID, fromString)
import Data.String
import Data.Time
import Verification.Types (CorrelationID(..))

import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string

data NotificationRecord = NotificationRecord {
  accountId :: String
, messageId :: String
, body :: ByteString
} deriving (Generic, Show)  

instance FromJSON NotificationRecord where
  parseJSON = withObject "dynamodb" $ \v -> do
      k <- v .: "Keys"
      ka <- k .: "accountId"
      accountId <- ka .: "S"
      km <- k .: "messageId"
      messageId <- km .: "S"     
      o <- v .: "OldImage"
      mS <- o .: "message"
      b <- mS .: "S"
      -- sms <- u .: "principalId"
      -- k <- as .: "type"
      -- kind <- k .: "Value"      
      -- subject <- sns .: "Subject"
      -- b <- sns .: "Message"
      let body = BLU.fromString b
      return NotificationRecord{..}    