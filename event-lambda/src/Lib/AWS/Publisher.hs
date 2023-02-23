module Lib.AWS.Publisher
    ( publishMessage,
      getTopicInfo
    ) where


import Lib.AWS.Types

import GHC.Generics
import           Data.Maybe
-- import qualified Network.AWS.SNS as SNS
import           Network.AWS.SNS
import           Network.AWS.SNS.Publish
import           Network.AWS.SNS.Publish (publish)

-- import           Network.AWS.SNS.Publish --(Publish(..), PublishResponse(..))


import           Network.AWS.SNS.Types
import qualified Data.Aeson as Aeson 
import           Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy as B

import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict)
import qualified Data.Text as T 
import Control.Monad.IO.Class

import Control.Lens (view, set, (^?), (<&>), (^.), (.~), (&))
import Network.AWS as AWS
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Text
import           Network.AWS.Data (fromText, toText)

import Network.AWS.SNS.Types as AWS
import System.IO (stdout)
import Data.HashMap.Strict
import Control.Monad.Trans.AWS
        ( AWST'
        , Credentials(..)
        , Env
        , HasEnv
        , LogLevel(..)
        , Region(..)
        , envLogger
        , newEnv
        , newLogger
        , reconfigure
        , runAWST
        , runResourceT
        , send
        , setEndpoint
        , within
        )

type MessageType = String
type MessageSubject = String

data TopicInfo = TopicInfo
    { env :: Env
    , topic :: String
    }

responseToEither :: PublishResponse -> Either String (Maybe Text)
responseToEither res = do
    case (stat >= 200 && stat < 300) of
        False -> Left $ "AWS request failed with status: " ++ show stat
        True -> Right mId
    where 
        stat = view prsResponseStatus  res
        mId = view prsMessageId res    
        
getTopicInfo :: LoggingState -> ServiceType -> AWSAccount -> String -> IO TopicInfo
getTopicInfo loggingState (AWS region) account name = do
    env <- getEnv loggingState
    let t = topic region account
    return $ TopicInfo env t
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover
        topic region awsAccount = "arn:aws:sns:" ++ (T.unpack $ toText region) ++ ":" ++ (unAWSAccount awsAccount) ++ ":" ++ name
        -- Run against a DynamoDB instance running on AWS in specified region
        -- serviceRegion (AWS region) = (dynamoDB, region)
        -- Run against a local DynamoDB instance on a given host and port
        -- serviceRegion (Local hostName port) = (setEndpoint False hostName port dynamoDB, NorthVirginia)


publishMessage :: ToJSON a => TopicInfo -> MessageType -> MessageSubject -> a -> IO (Either String (Maybe Text))
publishMessage TopicInfo{..} messageType subject a = do 
  let req = (set pSubject s (set pTopicARN arn (set pMessageAttributes as (publish m))))
--   env <- getEnv loggingState
  r <- liftIO
        $ runResourceT
        $ runAWS env
        $ AWS.send
        $ req
  print $ show r
  pure $ responseToEither r 
  where
      s = Just $ T.pack subject :: Maybe Text
      arn = Just $ T.pack topic :: Maybe Text
      m = toStrict $ decodeUtf8 $ encode a :: Text
      av = set mavStringValue (Just(T.pack messageType)) (messageAttributeValue "String") -- Just(T.pack messageType)
      as = Data.HashMap.Strict.singleton "type" av :: HashMap Text MessageAttributeValue 
    --   -- Standard discovery mechanism for credentials, log to standard output
    --   getEnv LoggingEnabled = do
    --       logger <- newLogger Debug stdout
    --       newEnv Discover <&> set envLogger logger
    --   -- Standard discovery mechanism for credentials, no logging
    --   getEnv LoggingDisabled = newEnv Discover  

