
module Lib.AWS.Types (   
    LoggingState(..),
    ServiceType(..),
    AWSAccount(..),
    unAWSAccount
) where

import qualified Data.Text as T

import qualified Data.ByteString as BS (ByteString) 
import           Network.AWS (Service, await)
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

type HostName = BS.ByteString
type Port = Int

data LoggingState = LoggingEnabled | LoggingDisabled
data ServiceType = AWS Region | Local HostName Port

newtype AWSAccount = AWSAccount String
unAWSAccount :: AWSAccount -> String
unAWSAccount (AWSAccount a) = a

instance Show AWSAccount where
  show (AWSAccount s) = show s
