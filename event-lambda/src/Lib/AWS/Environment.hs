module Lib.AWS.Environment
    ( getRegion,
      getLoggingState,
      getAccount
    ) where

import           Text.Read (readMaybe, read)
import           System.Environment
import qualified Data.Text as T (null, pack, unpack)

import           Data.Maybe
import           Data.Either
import           Network.AWS.Data (fromText)
import           Control.Monad.Trans.AWS(Region(..))

import           Lib.AWS.Types 
import           Data.Bifunctor
import           Lib.Utils (maybeToEither, getEnv')

getRegion :: IO (Either String Region)
getRegion = do
  e <- getEnv' "AWS_REGION"
  pure $ e >>= fromText . T.pack

getAccount :: IO (Either String AWSAccount)
getAccount = do 
  e <- getEnv' "AWS_ACCOUNT_ID"
  pure $ fmap AWSAccount e

getLoggingState :: IO LoggingState
getLoggingState = do
    r <- getEnv "DEBUG" 
    pure $ case readMaybe r :: Maybe Bool of
         Just(True) -> LoggingEnabled
         _ -> LoggingDisabled



