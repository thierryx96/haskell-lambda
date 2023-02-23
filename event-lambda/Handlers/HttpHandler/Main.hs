


module Main (main) where

-- module Main where
import qualified Data.Aeson as Aeson
import           Verification.Messages
import           Verification.Storage
import           Verification.Types
-- import              Verification.Types(VerificationStatus(..))
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

import           Lib.AWS.Events 
import           Lib.AWS.Emailing
import           Lib.AWS.Types 
import           Lib.AWS.Publisher 
import           Lib.AWS.Environment 

import           Verification.Messages
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
import qualified Data.UUID as U

import           Data.Text.Lazy (toStrict)

import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
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
-- import qualified Network.Wreq as Http
import qualified Data.HashMap.Strict as HMS
import           Control.Exception
-- import           Partial


-- main :: IO ()
-- main = apiGatewayMain handler

-- protocol :: String
-- protocol = "https://"
          
-- getProxyBody :: Http.Response BSL.ByteString -> IO Text
-- getProxyBody resFromGivenUrl = return . LazyText.toStrict . LazyText.decodeUtf8 $ resFromGivenUrl ^. Http.responseBody

-- htmlRes :: Int -> Text -> IO (APIGatewayProxyResponse Text)
-- htmlRes status proxyBody = pure $ htmlResWithNoBody status & responseBody ?~ proxyBody
--   where
--     htmlResWithNoBody :: Int -> APIGatewayProxyResponse Text
--     htmlResWithNoBody statusCode = APIGatewayProxyResponse statusCode [("Content-Type", "text/html")] Nothing    


-- handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
-- handler request = do
--   let urlPath = HMS.lookup "url" $ request ^. agprqPathParameters
--   print request
--   case urlPath of
--     Just path ->
--       (Http.get $ protocol <> unpack path) 
--         >>= getProxyBody
--         >>= htmlRes 200
--     Nothing -> htmlRes 500 "No path found"



-- main = apiGatewayMain $ \e -> do
--      r <- handlerApi e
--      a <- throw r
--      return a

-- handle e = handlerApi e >>= \r -> throw r

-- catch :: Value -> IO (Either String (APIGatewayProxyResponse String))

-- throw v = do 
--      case v of
--           Left e -> error e
--           Right r -> r
     


-- main = lambdaMain $ (handlerApi `alternative` catch)

main = lambdaMain $ \ev -> do 
     r <- handlerApi ev
     pure $ fromRight' r

fromRight' :: Either l r -> r
fromRight' (Right x) = x
fromRight' _ = error "fromRight', given a Left"
     
catch :: Value -> IO (Either String String)
catch v = do
     print "error: unknown event"
     print v
     pure $ Left "error: unknown event"

handlerApi :: ApiVerifyCodeEvent -> IO (Either String String)
handlerApi evt@ApiVerifyCodeEvent{..} = do
     print $ "received http event:" ++ show evt
     runExceptT $ do 
               region <- ExceptT getRegion  -- >>= \r -> fmap (\a -> r) (getAccount)
               awsAccount <- ExceptT getAccount
               appEndpoint <- ExceptT $ getEnv' "WEBAPP_ENDPOINT"
               let redirectOk = appEndpoint ++ "/verification-ok.html"
               let redirectError = appEndpoint ++ "/verification-error.html"
               let redirectAlready = appEndpoint ++ "/verification-already.html"

               ls <- liftIO getLoggingState
               db <- liftIO $ getDBInfo ls (AWS region) --(Local "localhost" 8000)
               v <- liftIO $ getVerification db correlationId
               sns <- liftIO $ getTopicInfo ls (AWS region) awsAccount "verification" --(Local "localhost" 8000)

               liftIO $ case v of 
                    Just(Verification _ _ _ _ Verified (Just(c)) _ ) -> do 
                         pure $ redirectAlready
                    Just(Verification accountId targetType targetId topic _ (Just(expectedCode)) _ ) | expectedCode == code -> do 
                         updateVerificationResult db correlationId Verified Nothing (Just "verified by email")
                         let r = T.unpack $ toText region
                         let m = VerificationComplete correlationId accountId targetType targetId topic Verified Nothing Nothing External
                         publishMessage sns "VerificationComplete" "Verification is completed" m
                         pure $ redirectOk -- redirect redirectOk -- "Thank you. You can now close this tab."      
                    Just(Verification _ _ _ _ PendingApproval (Just(c)) _ ) | c /= code -> do 
                         pure $ redirectError -- redirect redirectError
                    Just(Verification _ _ _ _ Created (Just(c)) _ ) | c /= code -> do 
                         pure $ redirectError -- redirect redirectError
                    _ -> do 
                         pure $ redirectError --redirect redirectError
               -- where    
               --      redirect :: String -> APIGatewayProxyResponse String
               --      redirect location = APIGatewayProxyResponse 301 [("Location", encodeUtf8 $ T.pack location)] Nothing
