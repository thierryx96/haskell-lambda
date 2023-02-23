
module Verification.Messages
    ( EmailVerificationRequest(..),
      DocVerificationRequest(..),
      DocUpdatedMessage(..),
      VerificationComplete(..),
      ABNVerificationRequest(..),
      TextVerificationRequest(..),
      Message,
      createFromS3Prefix,
      AccountID(..)
    ) where
import Control.Monad (mzero)
import Lib.AWS.Emailing (VerificationTemplateData)
import Verification.Types
import qualified Data.Aeson.Types as T

import Data.Aeson (FromJSON, ToJSON, parseJSON, decode, withText)
import GHC.Generics
import Data.Time
import qualified Data.UUID as U (UUID, fromString,  fromText, toString)
import Data.String
import qualified Data.Text as T
import Data.String

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson (FromJSON, ToJSON, parseJSON, decode, withText)

import qualified Text.Regex as R
import Text.Read (readMaybe, read)

newtype MessageType = MessageType String
newtype InitiatorKey = InitiatorKey String

class Message a where
  messageType :: a -> MessageType  
  id :: a -> CorrelationID

data EmailVerificationRequest = EmailVerificationRequest {
    correlationId :: CorrelationID
  , accountId :: AccountID
  , targetType :: TargetType
  , targetId :: U.UUID
  , topic :: VerificationTopic
  , recipient :: String
  , recipientBcc :: String
  , templateName :: String
  , templateData :: VerificationTemplateData
} deriving (Generic, Show)
  
data DocVerificationRequest = DocVerificationRequest {
    correlationId :: CorrelationID
  , accountId :: AccountID
  , targetType :: TargetType
  , targetId :: U.UUID
  , topic :: VerificationTopic
  , jurisdiction :: Maybe String
  , docUrl :: Maybe String
} deriving (Generic, Show)

data ABNVerificationRequest = ABNVerificationRequest {
    correlationId :: CorrelationID
  , accountId :: AccountID
  , targetType :: TargetType
  , targetId :: U.UUID
  , topic :: VerificationTopic
  , jurisdiction :: Maybe String
  , abn :: String
  , firstName :: String
  , lastName :: String
} deriving (Generic, Show)

data TextVerificationRequest = TextVerificationRequest {
    correlationId :: CorrelationID
  , accountId :: AccountID
  , targetType :: TargetType
  , targetId :: U.UUID
  , topic :: VerificationTopic
  , text :: String
} deriving (Generic, Show)
  
data DocUpdatedMessage = DocUpdatedMessage {
    accountId :: AccountID
  , targetType :: TargetType
  , targetId :: U.UUID
  , topic :: VerificationTopic
  , jurisdiction :: Maybe String
  , docUrl :: String
} deriving (Generic, Show)

instance FromJSON EmailVerificationRequest
instance FromJSON DocVerificationRequest
instance FromJSON ABNVerificationRequest
instance FromJSON TextVerificationRequest

-- Outbound messages

data VerificationComplete = VerificationComplete {
      correlationId :: CorrelationID
    , accountId :: AccountID
    , targetType :: TargetType
    , targetId :: U.UUID  
    , topic :: VerificationTopic
    , status :: VerificationStatus
    , expiredAt :: Maybe UTCTime
    , reason :: Maybe String
    , source :: VerificationSource
} deriving (Generic, Show)
  
instance ToJSON VerificationComplete

instance Message EmailVerificationRequest where  
  messageType a = MessageType("EmailVerificationRequest")
  id (EmailVerificationRequest i _ _ _ _ _ _ _ _) = i
instance Message DocVerificationRequest where  
  messageType a = MessageType("DocVerificationRequest")
  id (DocVerificationRequest i _ _ _ _ _ _) = i

createFromS3Prefix :: String -> String -> Maybe DocUpdatedMessage 
createFromS3Prefix b o = 
  case (R.splitRegex (R.mkRegex "/") o) of 
      (accountId:targetType:targetId:topicWithState:_) -> do
        i <- (U.fromString targetId)
        a <- AccountID <$> (U.fromString accountId)        
        case (R.splitRegex (R.mkRegex "-") topicWithState) of
              (topic:jurisdiction:_) -> do
                t <- (readMaybe topic) :: Maybe VerificationTopic
                target <- (readMaybe targetType) :: Maybe TargetType
                return (DocUpdatedMessage a target i t (Just jurisdiction) (b ++ "/" ++ o))
              (topic:_) -> do
                t <- (readMaybe topic) :: Maybe VerificationTopic
                target <- (readMaybe targetType) :: Maybe TargetType
                return (DocUpdatedMessage a target i t Nothing (b ++ "/" ++ o))

        -- return (DocUpdatedMessage a initType i t (b ++ "/" ++ o))
      _ -> Nothing
      

-- instance Correlated (EmailVerificationRequest a) where  
--   correlationId a = a.correlationId

-- instance Correlated (DocVerificationRequest a) where  
--   correlationId a = a.correlationId

-- instance Correlated (VerificationResult a) where  
--   correlationId a = a.correlationId