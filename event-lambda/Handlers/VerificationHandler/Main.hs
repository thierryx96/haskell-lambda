


module Main (main) where

-- module Main where
import qualified Data.Aeson as Aeson
import           Verification.Messages
import           Verification.Storage
import           Verification.Types
-- import Data.String.Interpolate ( i )

import           Lib.Utils
import           Data.Bifunctor

-- import qualified Data.ByteString as B
import Data.ByteString.Lazy.UTF8 as BLU
-- import Data.Maybe
import           AWSLambda
import           AWSLambda.Events
import           AWSLambda.Events.APIGateway

-- import AWSLambda.Events.APIGateway
-- import AWSLambda.Events ( LambdaEvent(..) )

import Data.Aeson
import Data.Aeson.Alternative
import Data.Aeson.Embedded
import Data.Time.Format (formatTime)
import Data.List
import Data.List.Split


import           Lib.AWS.Events 
import           Lib.AWS.Emailing
import           Lib.AWS.Types 
import           Lib.AWS.Publisher 
import           Lib.AWS.Environment 

import           Verification.Messages
import           Verification.Types
import           Language.Processor (findWords)
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
import           Data.Either
import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T

main = lambdaMain $ (handlerSns `alternative` catch)

catch :: Value -> IO (Either String String)
catch v = do
     print "error: unknown event"
     print v
     pure $ Left "error: unknown event"


-- handlerApi :: ApiVerifyCodeEvent -> IO (Either String (APIGatewayProxyResponse String))
-- handlerApi evt@ApiVerifyCodeEvent{..} = do
--      print $ "received http event:" ++ show evt
--      runExceptT $ do 
--                region <- ExceptT getRegion  -- >>= \r -> fmap (\a -> r) (getAccount)
--                awsAccount <- ExceptT getAccount
--                appEndpoint <- ExceptT $ getEnv' "WEBAPP_ENDPOINT"
--                let redirectOk = appEndpoint ++ "/verification-ok.html"
--                let redirectError = appEndpoint ++ "/verification-error.html"
--                ls <- liftIO getLoggingState
--                db <- liftIO $ getDBInfo ls (AWS region) --(Local "localhost" 8000)
--                v <- liftIO $ getVerification db correlationId
--                sns <- liftIO $ getTopicInfo ls (AWS region) awsAccount "verification" --(Local "localhost" 8000)

--                liftIO $ case v of 
--                     -- Verified already
--                     Just(Verification _ _ _ _ Verified (Just(c)) _ ) -> do 
--                          pure $ redirect redirectError
--                     Just(Verification accountId targetType targetId topic _ (Just(expectedCode)) _ ) | expectedCode == code -> do 
--                          updateVerificationResult db Verified $ VerificationResult correlationId True Nothing (Just "verified by email")
--                          let r = T.unpack $ toText region
--                          let m = VerificationComplete correlationId accountId targetType targetId topic Verified Nothing Nothing
--                          publishMessage sns "VerificationComplete" "Verification is completed" m
--                          pure $ redirect redirectOk -- "Thank you. You can now close this tab."      
--                     Just(Verification _ _ _ _ PendingApproval (Just(c)) _ ) | c /= code -> do 
--                          pure $ redirect redirectError
--                     Just(Verification _ _ _ _ Created (Just(c)) _ ) | c /= code -> do 
--                          pure $ redirect redirectError
--                     _ -> do 
--                          pure $ redirect redirectError
--                where    
--                     redirect :: String -> APIGatewayProxyResponse String
--                     redirect location = APIGatewayProxyResponse 301 [("Location", B.pack location)] Nothing



-- buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString
--              -> BC.ByteString -> Request
-- buildRequest host method path  = setRequestMethod method
--                                   $ setRequestHost host
--                                   $ setRequestPath path
--                                   $ setRequestSecure True
--                                   $ setRequestPort 443
--                                   $ defaultRequest

httpGet :: String -> IO (Either String Text)
httpGet url = do
    r <- httpLBS $ parseRequest_ url
    let s = getResponseStatusCode r
    let b = getResponseBody r
    pure $ case (s >= 200 && s < 300) of
        False -> Left $ "Http request to " ++ url ++ " failed with status: " ++ show s
        True -> Right $ T.pack $ B8.unpack $ BL8.toStrict b



abnResponseValid :: Text -> String -> String -> String -> Maybe String
abnResponseValid r abn fName lName = case sequence xs of 
     Left(e) -> Just(e)
     Right(_) -> Nothing
     where 
          xs = [
               if T.isInfixOf "No matching records found" then Left("ABN is not found") else Right(""),
               if T.isInfixOf "is not a valid ABN" r then Left("ABN is invalid") else Right(""),
               if T.isInfixOf "Cancelled from" r then Left("ABN has been cancelled") else Right("") ,
               if T.isInfixOf "Suppressed" r then Left("ABN has been suppressed") else Right(""),
               if T.isInfixOf (T.toUpper . T.pack $ fName) (T.toUpper r) && T.isInfixOf (T.toUpper . T.pack $ lName) (T.toUpper r) then Right("") else Left("Incorrect entity name"),
               if T.isInfixOf "Individual/Sole Trader" r then Right("") else Left("Incorrect entity type")
               ]



handlerSns :: LambdaEventBatch SNSMessageEvent -> IO (Either String String)
handlerSns evt = do 
     print $ "received sns event:" ++ show evt
     cfg <- runExceptT $ do 
          region <- ExceptT getRegion  -- >>= \r -> fmap (\a -> r) (getAccount)
          ls <- liftIO getLoggingState
          db <- liftIO $ getDBInfo ls (AWS region) --(Local "localhost" 8000)
          apiUrl <- ExceptT $ getEnv' "API_ENDPOINT"
          emailFrom <- ExceptT $ getEnv' "EMAIL_SENDER"
          awsAccount <- ExceptT getAccount
          sns <- liftIO $ getTopicInfo ls (AWS region) awsAccount "verification" --(Local "localhost" 8000)
          ses <- liftIO $ getSESInfo ls
          liftIO $ print evt
          pure (db, sns, ses, apiUrl, emailFrom)
     res <- sequenceA $ fmap (\(db, sns, ses, apiUrl, emailFrom) -> op db sns ses apiUrl emailFrom evt) cfg 
     pure $ show <$> res 
          
     where op db sns ses apiUrl emailFrom evt = case evt of 
               LambdaEventBatch((SNSMessageEvent _ "DocVerificationRequest" _ body):_) -> runExceptT $ do 
                    d <- ExceptT $ pure ((Aeson.eitherDecode body) :: Either String DocVerificationRequest)
                    liftIO $ (createDocVerification db Created) d

               LambdaEventBatch((SNSMessageEvent _ "EmailVerificationRequest" _ body):_) -> runExceptT $ do 
                    d <- ExceptT $ pure ((Aeson.eitherDecode body) :: Either String EmailVerificationRequest)
                    let EmailVerificationRequest{..} = d 
                    code <- liftIO $ U.toString <$> nextRandom

                    let link = apiUrl ++ "/verify?id=" ++ show(correlationId) ++ "&code=" ++ code
                    let email = createCodeEmailRequest emailFrom recipient recipientBcc link templateName templateData
                    liftIO $ createEmailVerification db Created code d
                    i <- ExceptT (sendEmailRequest ses email)
                    liftIO $ updateVerificationStatus db correlationId PendingApproval

               LambdaEventBatch((SNSMessageEvent _ "TextVerificationRequest" _ body):_) -> runExceptT $ do 
                    d <- ExceptT $ pure ((Aeson.eitherDecode body) :: Either String TextVerificationRequest)
                    w <- ExceptT $ getEnv' "CENSORED_WORDS" 
                    let cws = splitOn "," w
                    let TextVerificationRequest{..} = d
                    rs <- liftIO $  findWords cws $ T.pack text
                    let s = if rs == [] then Verified else Rejected
                    let reason = if rs == [] then Nothing else Just("Found censored words: " ++ (intercalate ", " rs))
                    let m = VerificationComplete correlationId accountId targetType targetId topic s Nothing reason Auto
                    liftIO $ print m
                    ExceptT $ fmap (\_ -> ()) <$> (publishMessage sns "VerificationComplete" "Verification is completed" m)       

               LambdaEventBatch((SNSMessageEvent _ "ABNVerificationRequest" _ body):_) -> runExceptT $ do 
                    d <- ExceptT $ pure ((Aeson.eitherDecode body) :: Either String ABNVerificationRequest)
                    let ABNVerificationRequest{..} = d 
                    let url = "https://abr.business.gov.au/ABN/View?id=" ++ abn
                    r <- ExceptT $ httpGet url
                    let reason = abnResponseValid r abn firstName lastName
                    let s = if reason == Nothing then Verified else Rejected
                    let m = VerificationComplete correlationId accountId targetType targetId topic s Nothing reason Auto
                    liftIO $ print m
                    ExceptT $ fmap (\_ -> ()) <$> (publishMessage sns "VerificationComplete" "Verification is completed" m)

                    
               -- LambdaEventBatch((SNSMessageEvent _ "VerificationResult" _ body):_) -> runExceptT $ do 
               --      d <- ExceptT $ pure ((Aeson.eitherDecode body) :: Either String VerificationResult)
               --      let VerificationResult{..} = d
               --      let status = if valid then Verified else Rejected
               --      v <- ExceptT $ maybeToEither ("verification not found: " ++ show correlationId) <$> getVerification db correlationId
               --      let Verification accountId targetType targetId topic PendingApproval _ _ = v
               --      let m = VerificationComplete correlationId accountId targetType targetId topic status expiredAt reason
               --      liftIO $ updateVerificationResult db status d
               --      ExceptT $ fmap (\_ -> ()) <$> (publishMessage sns "VerificationComplete" "Verification is completed" m)


-- https://abr.business.gov.au/ABN/View?id=64271466045

-- handlerS3 :: LambdaEventBatch S3Event -> IO (Either String String)
-- handlerS3 evt@(LambdaEventBatch((S3Event b o _):_)) = do 
--   print $ "received s3 event:" ++ show evt
--   cfg <- runExceptT $ do 
--      region <- ExceptT getRegion  -- >>= \r -> fmap (\a -> r) (getAccount)
--      ls <- liftIO getLoggingState
--      db <- liftIO $ getDBInfo ls (AWS region) --(Local "localhost" 8000)
--      apiUrl <- ExceptT $ getEnv' "API_ENDPOINT"
--      emailFrom <- ExceptT $ getEnv' "EMAIL_SENDER"
--      awsAccount <- ExceptT getAccount
--      pure (db, sns, ses, apiUrl, emailFrom)
--      ids <- fmap (\v@(DocUpdatedMessage a it ii t j uri) -> resolveVerificationId db a it ii t j) (maybeToEither("error decoding s3 key:" ++ show(o)) (createFromS3Prefix b o))
--      liftIO $ print $ "found ids:" ++ show(ids)
--   pure $ show <$> "res" 

--   res <- case maybeToEither("error decoding s3 key:" ++ show(o)) (createFromS3Prefix b o) of 
--     Right v@(DocUpdatedMessage a it ii t j uri) -> do   
--       ids <- resolveVerificationId db a it ii t j
--       print $ "found ids:" ++ show(ids)
--       vId <- case ids of 
--         (h:_) -> print ("doc correlation id=" ++ show(h) ++ " already exists in db, continue ... ") >> pure h                                                        
--         [] -> do 
--              newId <- CorrelationID <$> nextRandom
--              createDocVerification db Created $ DocVerificationRequest newId a it ii t j $ Just(uri) 
--              pure newId      
--       updateDocVerificationUpload db vId PendingApproval v
--       pure "created / updated doc verification"
      
--     Left e -> pure $ "execution error " ++ show(e)
--   print $ "result: " ++ show(res)
--   pure $ T.pack res

