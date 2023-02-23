module Lib.AWS.Events
    ( S3Event(..),
      SNSMessageEvent(..),
      CognitoUserEvent(..),
      DynamoEvent(..),
      LambdaEventBatch(..),
      ApiVerifyCodeEvent(..)
    ) where

import Data.Aeson 
import GHC.Generics
-- import Data.UUID (UUID, fromString)
import Data.String
import Data.Time
import Verification.Types (CorrelationID(..))

import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import qualified Data.HashMap.Strict as HashMap

data ApiVerifyCodeEvent = ApiVerifyCodeEvent {
    requestPath :: String
  , method :: String
  , correlationId :: CorrelationID
  , code :: String
  } deriving (Generic, Show)

data S3Event = S3Event {
    bucket :: String
  , objectKey :: String
  , eventName :: String
  } deriving (Generic, Show)

data SNSMessageEvent = SNSMessageEvent {
   topicArn :: String
 , kind :: String
 , subject :: String
 , body :: ByteString
 } deriving (Generic, Show)  

data DynamoEvent a = DynamoEvent {
   userIdentityType :: String
 , userIdentityPrincipal :: String
 , eventName :: String
 , eventSource :: String
 , record :: a
 } deriving (Generic, Show)   

data CognitoUserEvent = CognitoUserEvent {
    sub:: String
   ,address:: String
   ,firstName:: String
   ,lastName:: String
   ,email:: String
} deriving (Generic, Show)  

data LambdaEventBatch a = LambdaEventBatch {
   records :: [a]
} deriving (Generic, Show)

instance FromJSON CognitoUserEvent where
   parseJSON = withObject "event" $ \v -> do
      req <- v .: "request"
      sub <- req .: "sub"
      address <- req .: "address"
      firstName <- req .: "firstName"
      lastName <- req .: "lastName"
      email <- req .: "email"
      return CognitoUserEvent{..}

instance FromJSON S3Event where
   parseJSON = withObject "event" $ \v -> do
      eventName <- v .: "eventName"
      s3 <- v .: "s3"
      b <- s3 .: "bucket"
      bucket <- b .: "name"
      o <- s3 .: "object"
      objectKey <- o .: "key"
      return S3Event{..}      

instance FromJSON SNSMessageEvent where
   parseJSON = withObject "event" $ \v -> do
      sns <- v .: "Sns"
      topicArn <- sns .: "TopicArn"
      as <- sns .: "MessageAttributes"
      k <- as .: "type"
      kind <- k .: "Value"      
      subject <- sns .: "Subject"
      b <- sns .: "Message"
      let body = BLU.fromString b
      return SNSMessageEvent{..}

instance FromJSON ApiVerifyCodeEvent where
   parseJSON = withObject "event" $ \v -> do
      requestPath <- v .: "requestPath"
      method <- v .: "method"
      q <- v .: "query"
      code <- q .: "code"
      correlationId <- q .: "id" 
       
      return ApiVerifyCodeEvent{..}      
{-

{
  "Records": [
    {
      "eventID": "f07f8ca4b0b26cb9c4e5e77e69f274ee",
      "eventName": "INSERT",
      "eventVersion": "1.1",
      "eventSource": "aws:dynamodb",
      "awsRegion": "us-east-1",
      "userIdentity":{
        "type":"Service",
        "principalId":"dynamodb.amazonaws.com"
      },
      "dynamodb": {
        "ApproximateCreationDateTime": 1480642020,
        "Keys": {
          "val": {
            "S": "data"
          },
          "key": {
            "S": "binary"
          }
        },
        "NewImage": {
          "val": {
            "S": "data"
          },
          "asdf1": {
            "B": "AAEqQQ=="
          },
          "asdf2": {
            "BS": [
              "AAEqQQ==",
              "QSoBAA=="
            ]
          },
          "key": {
            "S": "binary"
          }
        },
        "SequenceNumber": "1405400000000002063282832",
        "SizeBytes": 54,
        "StreamViewType": "NEW_AND_OLD_IMAGES"
      },
      "eventSourceARN": "arn:aws:dynamodb:us-east-1:123456789012:table/Example-Table/stream/2016-12-01T00:00:00.000"
    },
    {
      "eventID": "f07f8ca4b0b26cb9c4e5e77e42f274ee",
      "eventName": "INSERT",
      "eventVersion": "1.1",
      "eventSource": "aws:dynamodb",
      "awsRegion": "us-east-1",
      "dynamodb": {
        "ApproximateCreationDateTime": 1480642020,
        "Keys": {
          "val": {
            "S": "data"
          },
          "key": {
            "S": "binary"
          }
        },
        "NewImage": {
          "val": {
            "S": "data"
          },
          "asdf1": {
            "B": "AAEqQQ=="
          },
          "b2": {
            "B": "test"
          },
          "asdf2": {
            "BS": [
              "AAEqQQ==",
              "QSoBAA==",
              "AAEqQQ=="
            ]
          },
          "key": {
            "S": "binary"
          },
          "Binary": {
            "B": "AAEqQQ=="
          },
          "Boolean": {
            "BOOL": true
          }
        },
        "SequenceNumber": "1405400000000002063282832",
        "SizeBytes": 54,
        "StreamViewType": "NEW_AND_OLD_IMAGES"
      },
      "eventSourceARN": "arn:aws:dynamodb:us-east-1:123456789012:table/Example-Table/stream/2016-12-01T00:00:00.000"
    }
  ]
}
      "userIdentity":{
        "type":"Service",
        "principalId":"dynamodb.amazonaws.com"
      },

-}


instance (FromJSON a) => FromJSON (DynamoEvent a) where
   parseJSON = withObject "event" $ \v -> do
      u <- v .: "userIdentity"
      userIdentityType <- u .: "type"
      userIdentityPrincipal <- u .: "principalId"
      eventName <- v .: "eventName"
      eventSource <- v .: "eventSource"
      record <- v .: "dynamodb"
      return DynamoEvent{..}        


instance (FromJSON a) => FromJSON (LambdaEventBatch a) where
    parseJSON (Object o) = LambdaEventBatch <$> o .: "Records"
