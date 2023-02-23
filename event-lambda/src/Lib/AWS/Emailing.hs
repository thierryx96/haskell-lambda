
module Lib.AWS.Emailing (
    createCodeEmailRequest,
    sendEmailRequest,
    VerificationTemplateData,
    SESInfo,
    getSESInfo
) where


import Lib.AWS.Types

import GHC.Generics
import           Data.Maybe
import           Network.AWS.SES
import           Network.AWS.SES.SendTemplatedEmail
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
import Network.AWS.SES.Types as AWS
import System.IO (stdout)
import Data.HashMap.Strict

type VerificationTemplateData = HashMap String String

data SESInfo = SESInfo
    { env :: Env
    }

getSESInfo :: LoggingState -> IO SESInfo
getSESInfo loggingState = do
    env <- getEnv loggingState
    pure $ SESInfo env
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

responseToEither :: SendTemplatedEmailResponse -> Either String Text
responseToEither res = do
    case (stat >= 200 && stat < 300) of
        False -> Left $ "AWS request failed with status: " ++ show stat
        True -> Right mId
    where 
        stat = view stersResponseStatus res
        mId = view stersMessageId res


sendEmailRequest :: SESInfo -> SendTemplatedEmail -> IO (Either String Text)
sendEmailRequest SESInfo{..} email = do 
    r <- liftIO
        $ runResourceT
        $ runAWS env
        $ AWS.send
        $ email
    print $ show r
    return $ responseToEither r 


        
createCodeEmailRequest :: String -> String -> String -> String -> String -> VerificationTemplateData-> SendTemplatedEmail
createCodeEmailRequest from to cc link tempName tempData = sendTemplatedEmail (T.pack from) (set dCCAddresses [T.pack(cc)] (set dToAddresses [T.pack(to)] destination)) (T.pack tempName) td
    where td = toStrict $ decodeUtf8 $ encode (insert "link" link tempData) 


