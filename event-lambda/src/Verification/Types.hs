
{-# LANGUAGE RankNTypes #-}


module Verification.Types
    ( 
      VerificationTopic(..)
    , VerificationStatus(..)
    , AccountID(..)
    , CorrelationID(..)
    , TargetType(..)
    , VerificationSource(..)
    ) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson (FromJSON, ToJSON, parseJSON, decode, withText)

import GHC.Generics
import qualified Data.UUID as U (UUID, fromString,  fromText, toString)
import Data.String
import qualified Data.Text as T

import Data.Time 
import qualified Text.Regex as R
import Text.Read (readMaybe)
import Data.Typeable 
import Data.Data
import Control.Monad

newtype AccountID = AccountID U.UUID deriving (Generic, Read, ToJSON)
newtype CorrelationID = CorrelationID U.UUID deriving (Generic, Read, ToJSON)

parseId :: Value -> Parser U.UUID
parseId (String s) = do 
    case U.fromText <$> (pure $ s) of
      Just(Just u) -> pure u
      _ -> mzero
parseId _ = mzero  

data TargetType = Account | CareProfile 
  deriving (Eq, Enum, Show, Generic, Read)

data VerificationSource = External | Auto
  deriving (Eq, Enum, Show, Generic, Read)

data VerificationStatus = Created | PendingApproval | Verified | Rejected
  deriving (Eq, Enum, Show, Generic, Read)

instance FromJSON TargetType where
  parseJSON (String s) = fmap read (pure $ T.unpack s)
  parseJSON _ = mzero  

instance ToJSON TargetType where
  toJSON = String . T.pack . show

data VerificationTopic = PersonalIdentification
  | PoliceCheck
  | WorkingWithChildrenQLD
  | WorkingWithChildrenNSW
  | WorkingWithChildrenVIC
  | WorkingWithChildrenSA
  | WorkingWithChildrenWA
  | WorkingWithChildrenTAS
  | WorkingWithChildrenNT
  | WorkingWithChildrenACT
  | ABN
  | ExternalProvider
  | PaymentPlan
  | AvatarImage
  | Description
  deriving (Eq, Enum, Show, Generic, Read)

instance FromJSON VerificationTopic where
  parseJSON (String s) = fmap read (pure $ T.unpack s)
  parseJSON _ = mzero  

instance ToJSON VerificationTopic where
  toJSON = String . T.pack . show

instance FromJSON VerificationStatus where
  parseJSON (String s) = fmap read (pure $ T.unpack s)
  parseJSON _ = mzero

instance ToJSON VerificationSource where
  toJSON = String . T.pack . show

instance FromJSON VerificationSource where
  parseJSON (String s) = fmap read (pure $ T.unpack s)
  parseJSON _ = mzero
  
instance ToJSON VerificationStatus where
  toJSON = String . T.pack . show

instance FromJSON CorrelationID where
  parseJSON v =  CorrelationID <$> parseId v  
instance FromJSON AccountID where
  parseJSON v =  AccountID <$> parseId v

instance Show CorrelationID where
  show (CorrelationID u) = U.toString u
instance Show AccountID where
  show (AccountID u) = U.toString u
