


module Main (main) where

-- module Main where
import qualified Data.Aeson as Aeson
import           Notification.Messages
-- import              Verification.Types(VerificationStatus(..))
-- import Data.String.Interpolate ( i )

import           Lib.Utils
import           Data.Bifunctor

-- import qualified Data.ByteString as B
import Data.ByteString.Lazy.UTF8 as BLU
-- import Data.Maybe
import AWSLambda
import AWSLambda.Events

-- import AWSLambda.Events.APIGateway
-- import AWSLambda.Events ( LambdaEvent(..) )

import Data.Aeson
import Data.Aeson.Alternative
import Data.Aeson.Embedded
import Data.Time.Format (formatTime)

import           Lib.AWS.Events 
import           Lib.AWS.Emailing
import           Lib.AWS.Types 
import           Lib.AWS.Publisher 
import           Lib.AWS.Environment 

import           Notification.Messages
import           Verification.Types
import           Data.UUID.V4 (nextRandom)
import           Data.Time  (UTCTime, getCurrentTime)
import           Network.AWS.Data (fromText, toText)
-- All imports are explicit so we can see exactly where each function comes from
import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), set, view)
import           Control.Monad (void, when)
import           Control.Monad.Trans.AWS
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

import           Data.Maybe
import           Data.Either
import           Control.Monad.IO.Class (MonadIO, liftIO)                    
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS (ByteString) 
import qualified Data.HashMap.Strict as HashMap (fromList, lookup)
import qualified Data.UUID as U

import           Data.Text.Lazy (toStrict)

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.ByteString.Char8 (unpack)
import qualified Data.Text as T (null, pack, unpack)
import           Data.Text.Read (decimal)
import           Network.AWS (Service, await, send, envRegion)
import           System.IO (stdout)
import           Data.Aeson.Text (encodeToLazyText)
import           System.Environment
import           Text.Read (readMaybe, read)
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Text.Encoding
import           Data.ByteString.Lazy


catch :: Value -> IO (Either String String)
catch v = do
     print "error: unknown event"
     print v
     pure $ Left "error: unknown event"

main = lambdaMain $ (dynamoHandler `alternative` catch)

{- "Records":[
     {
          ...
  
          "userIdentity":{
              "type":"Service",
              "principalId":"dynamodb.amazonaws.com"
          }
  
          ...
  
      }
  ]
-}




dynamoHandler :: LambdaEventBatch (DynamoEvent NotificationRecord) -> IO (Either String String)
dynamoHandler evt = do 
     print $ "received sns event:" ++ show evt
     runExceptT $ do 
          region <- ExceptT getRegion  -- >>= \r -> fmap (\a -> r) (getAccount)
          ls <- liftIO getLoggingState
          awsAccount <- ExceptT getAccount
          sns <- liftIO $ getTopicInfo ls (AWS region) awsAccount "verification" --(Local "localhost" 8000)
          liftIO $ print evt
          let LambdaEventBatch((DynamoEvent _ _ _ _ (NotificationRecord accountId messageId body)):_) = evt
          liftIO $ publishMessage sns "xxxxx" "Verification is completed" $ decodeUtf8 (Data.ByteString.Lazy.toStrict body)
          pure "done"
     -- res <- sequenceA $ fmap (\sns -> op sns evt) cfg 
     --      res <- sequenceA $ fmap (\(db, sns, ses, apiUrl, emailFrom) -> op db sns ses apiUrl emailFrom evt) cfg 

     -- pure $ show <$> res 
          
     -- where op sns evt = case evt of 
     --           LambdaEventBatch((DynamoEvent _ _ _ _ (NotificationRecord accountId messageId body)):_) -> runExceptT $ do 
     --                liftIO $ publishMessage sns "xxxxx" "Verification is completed" $ decodeUtf8 (Data.ByteString.Lazy.toStrict body)
     --                pure "Thank you. You can now close this tab."      

               -- LambdaEventBatch((SNSMessageEvent _ "DocVerificationRequest" _ body):_) -> runExceptT $ do 

     -- print $ "received dyn event:" ++ show evt
     -- runExceptT $ do 
     --      region <- ExceptT getRegion  -- >>= \r -> fmap (\a -> r) (getAccount)
     --      awsAccount <- ExceptT getAccount
     --      ls <- liftIO getLoggingState
     --      sns <- liftIO $ getTopicInfo ls (AWS region) awsAccount "account-95ed786b-d086-4de0-a52d-6ca9b22f98ae" --(Local "localhost" 8000)         
     --      liftIO $ publishMessage sns "xxxxx" "Verification is completed" "evt"
     --      liftIO $ pure "done."      

     -- res <- sequenceA $ fmap (\(db, sns, ses, apiUrl, emailFrom) -> op db sns ses apiUrl emailFrom evt) cfg 
