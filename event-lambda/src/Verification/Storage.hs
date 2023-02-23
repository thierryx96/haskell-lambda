module Verification.Storage (   
    getDBInfo,
    createEmailVerification,
    createDocVerification,
    updateDocVerificationUpload,
    updateVerificationResult,
    resolveVerificationId,
    getVerification,
    updateVerificationStatus,
    Verification(..)
) where

import Lib.AWS.Types

import Data.Text.Encoding (encodeUtf8Builder)
import Data.ByteString.Builder (toLazyByteString)
import Data.Aeson (decode)

-- module Main where
import qualified Data.Aeson as Aeson
-- import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.UTF8 as BLU
-- import Data.Maybe
import AWSLambda
import AWSLambda.Events

import Data.Aeson
import Data.Aeson.Alternative
import Data.Aeson.Embedded
import Data.Time.Format (formatTime)


import           Verification.Messages
import           Verification.Types
import qualified Data.UUID as U (toText, toString, fromText, fromString, UUID)
import           Data.UUID

import           Data.UUID.V4 (nextRandom)
import           Data.Time  (UTCTime, getCurrentTime)

-- All imports are explicit so we can see exactly where each function comes from
import           Control.Exception.Lens (handling)
import           Control.Lens ((<&>), (^.), (.~), (&), set)
import           System.IO (stdout)

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
import qualified Data.HashMap.Strict as HashMap (fromList, lookup, empty)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T (null, pack, unpack)
import           Data.Text(Text)
import           Data.HashMap.Strict (HashMap, empty)

import           Data.Text.Encoding (encodeUtf8)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy(toStrict)
import           Text.Read (readMaybe, read)


import           Data.ByteString.Char8 (unpack)
import           Data.Text.Read (decimal)
import           Network.AWS (Service, await)
import           Network.AWS.DynamoDB
                    ( _ResourceInUseException
                    , _ResourceNotFoundException
                    , KeyType(..)
                    , ScalarAttributeType(..)
                    , attributeDefinition
                    , attributeValue
                    , avN
                    , avS
                    , avB
                    , createTable
                    , ctAttributeDefinitions
                    , deleteTable
                    , describeTable
                    , dynamoDB
                    , getItem
                    , query
                    , giKey
                    , girsItem
                    , keySchemaElement
                    , piItem
                    , provisionedThroughput
                    , putItem
                    , tableExists
                    , tableNotExists
                    , uiExpressionAttributeValues
                    , uiKey
                    , uiUpdateExpression
                    , updateItem
                    , AttributeValue
                    , qKeyConditionExpression
                    , qIndexName                    
                    , qExpressionAttributeValues
                    , qrsItems
                    )
import           System.IO (stdout)
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson (FromJSON, ToJSON, parseJSON, decode, withText)

import           Lib.Utils (toLower)

verificationTableName = "Verification"
verificationTableIndex = "VerificationIndexSourceId"

data Verification = Verification {
    accountId :: AccountID
  , targetType :: TargetType
  , targetId :: UUID
  , topic :: VerificationTopic
  , status :: VerificationStatus
  , code :: Maybe String
  , docUrl :: Maybe String
} deriving (Show, Read)


class StringAttributeValue a where
    to :: a -> AttributeValue
    from :: AttributeValue -> Maybe a
            
instance StringAttributeValue U.UUID where
    to s = attributeValue & avS .~ Just (U.toText(s))
    from s = do 
        v <- s ^. avS
        u <- U.fromText v       
        return u

instance StringAttributeValue AccountID where
    to s = case s of AccountID(v) -> attributeValue & avS .~ Just (U.toText(v))
    from s = AccountID <$> (from s :: Maybe UUID) 

instance StringAttributeValue CorrelationID where
    to s = case s of CorrelationID(v) -> attributeValue & avS .~ Just (U.toText(v))
    from s = CorrelationID <$> (from s :: Maybe UUID) 

instance StringAttributeValue VerificationStatus where
    to s = attributeValue & avS .~ Just (T.pack(show s))
    from s = do 
        v <- s ^. avS
        r <- readMaybe $ T.unpack v     
        return r

instance StringAttributeValue TargetType where
    to s = attributeValue & avS .~ Just (T.pack(show s))
    from s = do 
        v <- s ^. avS
        r <- readMaybe $ T.unpack v     
        return r        

fromMap :: StringAttributeValue a => Text -> HashMap Text AttributeValue -> Maybe a
fromMap k item = HashMap.lookup k item >>= from 
    -- v <- HashMap.lookup k item >>= from
    -- return v    
    
-- instance StringAttributeValue SourceID where
--     to s = case s of CorrelationID(v) -> attributeValue & avS .~ Just (U.toText(v))
--     from s = CorrelationID <$> (from s :: Maybe UUID) 





data DBInfo = DBInfo
    { env :: Env
    , service :: Service
    , region :: Region
    , tableName :: Text
    }

getDBInfo :: LoggingState -> ServiceType -> IO DBInfo
getDBInfo loggingState serviceType = do
    env <- getEnv loggingState
    let (service, region) = serviceRegion serviceType
    return $ DBInfo env service region verificationTableName
    where
        -- Standard discovery mechanism for credentials, log to standard output
        getEnv LoggingEnabled = do
            logger <- newLogger Debug stdout
            newEnv Discover <&> set envLogger logger
        -- Standard discovery mechanism for credentials, no logging
        getEnv LoggingDisabled = newEnv Discover

        -- Run against a DynamoDB instance running on AWS in specified region
        serviceRegion (AWS region) = (dynamoDB, region)
        -- Run against a local DynamoDB instance on a given host and port
        serviceRegion (Local hostName port) = (setEndpoint False hostName port dynamoDB, NorthVirginia)

withDynamoDB :: (HasEnv r, MonadUnliftIO m) =>
    r
    -> Service
    -> Region
    -> AWST' r (ResourceT m) a
    -> m a
withDynamoDB env service region action =
    runResourceT . runAWST env . within region $ do
        reconfigure service action



doCreateTableIfNotExists :: DBInfo -> IO ()
doCreateTableIfNotExists DBInfo{..} = withDynamoDB env service region $ do
    exists <- handling _ResourceInUseException (const (pure True)) $ do
        void $ send $ createTable
            tableName
            (keySchemaElement "correlationId" Hash :| [])
            (provisionedThroughput 5 5)
            & ctAttributeDefinitions .~ [ attributeDefinition "correlationId" S ]
        return False
    when (not exists) (void $ await tableExists (describeTable tableName))


mkKey :: AccountID -> TargetType -> UUID -> VerificationTopic -> Maybe String -> Text
mkKey accountId targetType targetId topic jurisdiction = T.pack $ toLower(show(accountId) ++ ":" ++ show(targetType) ++ ":" ++ (U.toString targetId) ++ ":" ++ show(topic) ++ "-" ++ j)
    where j = maybe "" show jurisdiction

-- getItemCorrelationId :: HashMap Text AttributeValue -> Maybe UUID
-- getItemCorrelationId item = do 
--     v <- HashMap.lookup "correlationId" item >>= \x -> x ^. avS
--     U.fromText v   

toS :: Text -> HashMap Text AttributeValue -> Maybe String
toS k item = T.unpack <$> (HashMap.lookup k item >>= \x -> x ^. avS)


toU :: Text -> HashMap Text AttributeValue -> Maybe UUID
toU k item = do 
    v <- HashMap.lookup k item >>= \x -> x ^. avS
    u <- U.fromText v       
    return u

fromA :: FromJSON a => Text -> HashMap Text AttributeValue -> Maybe a
fromA k item = do 
    v <- HashMap.lookup k item >>= \x -> x ^. avS
    u <- decodeStrict $ encodeUtf8(v)    
    return u    

toR :: Read a => Text -> HashMap Text AttributeValue -> Maybe a
toR k item = do 
    v <- HashMap.lookup k item >>= \x -> x ^. avS 
    r <- readMaybe $ T.unpack v 
    return r    

resolveVerificationId :: DBInfo -> AccountID -> TargetType -> UUID -> VerificationTopic -> Maybe String -> IO [CorrelationID]
resolveVerificationId DBInfo{..} accountId targetType targetId topic jurisdiction = withDynamoDB env service region $ do
    results <- send $ query tableName
        & qIndexName .~ Just verificationTableIndex
        & qKeyConditionExpression .~ Just(T.pack("sourceId = :k"))
        & qExpressionAttributeValues .~ exprAttrValues
    return $ do
        map (fromJust.fromMap "correlationId") (results ^. qrsItems)     
    where
        exprAttrValues = HashMap.fromList(
            [ (":k", attributeValue & avS .~ Just(mkKey accountId targetType targetId topic jurisdiction))
            ])


          

-- ref: https://blog.rcook.org/blog/2017/aws-via-haskell/   

-- TODO: refact this gritness ... use maybe hashmap to json conversion ... 
getVerification :: DBInfo -> CorrelationID -> IO (Maybe Verification)
getVerification DBInfo{..} corrId = withDynamoDB env service region $ do
    r <- send $ getItem tableName & giKey .~ key
    liftIO $ print r
    -- liftIO $ print (getU "accountId" (r ^. girsItem) :: Maybe AccountID)
    pure $ toVerification(r ^. girsItem)        
    where key = HashMap.fromList [ ("correlationId", to corrId)]
          toVerification item = do
            accountId <- HashMap.lookup "accountId" item >>= from
            targetType <- toR "targetType" item
            targetId <- HashMap.lookup "targetId" item >>= from
            status <- toR "currentStatus" item
            let docUrl = toS "docUrl" item
            let code = toS "code" item
            topic <- toR "topic" item  
            pure $ Verification accountId targetType targetId topic status code docUrl   

-- Deletes a table in DynamoDB if it exists and waits until table no longer exists
doDeleteTableIfExists :: DBInfo -> IO ()
doDeleteTableIfExists DBInfo{..} = withDynamoDB env service region $ do
    exists <- handling _ResourceNotFoundException (const (pure False)) $ do
        void $ send $ deleteTable tableName
        return True
    when exists (void $ await tableNotExists (describeTable tableName))


createEmailVerification :: DBInfo -> VerificationStatus -> String -> EmailVerificationRequest -> IO ()
createEmailVerification DBInfo{..} status code EmailVerificationRequest{..} = withDynamoDB env service region $ do
    now <- liftIO getCurrentTime
    void $ send $ putItem tableName & piItem .~ item now
    where item t = HashMap.fromList
            ([    ("correlationId", to correlationId)
                , ("accountId", to accountId)
                , ("targetType", to targetType)
                , ("targetId", attributeValue & avS .~ Just(U.toText targetId ))
                , ("topic", attributeValue & avS .~ Just(T.pack $ show $ topic))
                , ("createdAt", attributeValue & avS .~ Just(T.pack(show $ t)))
                , ("currentStatus", to status)
                , ("recipient", attributeValue & avS .~ Just(T.pack recipient))
                , ("recipientBcc", attributeValue & avS .~ Just(T.pack recipientBcc))
                , ("templateName", attributeValue & avS .~ Just(T.pack templateName))
                , ("templateData", toA templateData)
                , ("code", attributeValue & avS .~ Just(T.pack code))
            ])

createDocVerification :: DBInfo -> VerificationStatus -> DocVerificationRequest -> IO ()
createDocVerification DBInfo{..} status DocVerificationRequest{..} = withDynamoDB env service region $ do
    now <- liftIO getCurrentTime
    liftIO $ print $ item now
    -- let v = HashMap.fromList(doc) ++ (item now)
    void $ send $ putItem tableName & piItem .~ item now
    where item t = HashMap.fromList
            ([    ("correlationId", to correlationId)
                , ("sourceId", attributeValue & avS .~ Just(mkKey accountId targetType targetId topic jurisdiction))
                , ("accountId", to accountId)
                , ("targetType", to targetType)
                , ("targetId", to targetId)
                , ("createdAt", attributeValue & avS .~ Just(T.pack(show $ t)))
                , ("topic", attributeValue & avS .~ Just(T.pack $ show topic))
                , ("currentStatus", to status)
            ] ++ doc)
          doc = fmap (\x -> ("docUrl", attributeValue & avS .~ Just(T.pack(x)))) (maybeToList(docUrl))
           

-- setDocVerification :: DBInfo -> VerificationStatus -> DocVerificationRequest -> IO ()
-- createDocVerification DBInfo{..} status DocVerificationRequest{..} = withDynamoDB env service region $ do
--     now <- liftIO getCurrentTime
--     liftIO $ print $ item now
--     -- let v = HashMap.fromList(doc) ++ (item now)
--     void $ send $ putItem tableName & piItem .~ item now
--     where item t = HashMap.fromList
--             ([    ("correlationId", to correlationId)
--                 , ("sourceId", attributeValue & avS .~ Just(mkKey accountId targetType targetId topic))
--                 , ("accountId", to accountId)
--                 , ("targetType", attributeValue & avS .~ Just(T.pack(targetType)))
--                 , ("targetId", to targetId)
--                 , ("createdAt", attributeValue & avS .~ Just(T.pack(show $ t)))
--                 , ("topic", attributeValue & avS .~ Just(T.pack $ show topic))
--                 , ("currentStatus", attributeValue & avS .~ Just(T.pack $ show status))
--             ] ++ doc)
--         doc = fmap (\x -> ("docUrl", attributeValue & avS .~ Just(T.pack(x)))) (maybeToList(docUrl))          
           
        --    ("docUrl", attributeValue & avS .~ (T.pack <$> docUrl))
            
            -- ++ HashMap.empty
            -- ++ if(isJust(docUrl)) then 
            --     HashMap.fromList
            --         ([
            --             maybedocUrl ("docUrl", attributeValue & avS .~ (T.pack <$> docUrl))
            --         ]) else Data.HashMap.Strict.empty
            
            -- (maybeToList docUrl))
            
            
            -- , ("docUrl", attributeValue & avS .~ (T.pack <$> docUrl))


toA v = attributeValue & avS .~ Just ( (toStrict . decodeUtf8 . encode) v )
            
fromS :: String -> (Maybe Text)
fromS v = Just $ T.pack(v)

fromJ :: ToJSON a => a -> (Maybe Text)
fromJ = Just . toStrict . decodeUtf8 . encode

fromT :: UTCTime -> (Maybe Text)
fromT v = Just $ toStrict $ decodeUtf8 $ encode v

-- updateVerificationDoc :: DBInfo -> CorrelationID -> String -> [(Text,AttributeValue)] -> IO ()
-- updateVerification DBInfo{..} corrId update values = withDynamoDB env service region $ do
--     now <- liftIO getCurrentTime
--     void $ send $ updateItem tableName
--         & uiKey .~ key
--         & uiUpdateExpression .~ Just(T.pack("SET updatedAt = :updatedAt," ++ update))
--         & uiExpressionAttributeValues .~ exprAttrValues now
--     where
--         key = HashMap.fromList [ ("correlationId", to corrId) ]
--         exprAttrValues t = HashMap.fromList(
--             [ (":updatedAt", attributeValue & avS .~ Just(T.pack(show t))) ] ++ values)

updateVerification :: DBInfo -> CorrelationID -> String -> [(Text,AttributeValue)] -> IO ()
updateVerification DBInfo{..} corrId update values = withDynamoDB env service region $ do
    now <- liftIO getCurrentTime
    void $ send $ updateItem tableName
        & uiKey .~ key
        & uiUpdateExpression .~ Just(T.pack("SET updatedAt = :updatedAt," ++ update))
        & uiExpressionAttributeValues .~ exprAttrValues now
    where
        key = HashMap.fromList [ ("correlationId", to corrId) ]
        exprAttrValues t = HashMap.fromList(
            [ (":updatedAt", attributeValue & avS .~ Just(T.pack(show t))) ] ++ values)

updateVerificationStatus :: DBInfo -> CorrelationID -> VerificationStatus -> IO ()
updateVerificationStatus db correlationId status = updateVerification db correlationId  "currentStatus=:s" [ (":s", to(status)) ]

-- TODO: Update Set of Docs here
updateDocVerificationUpload :: DBInfo -> CorrelationID -> VerificationStatus -> DocUpdatedMessage -> IO ()
updateDocVerificationUpload db correlationId status DocUpdatedMessage{..} = updateVerification db correlationId  "docUrl = :docUrl, currentStatus=:s" [ 
    (":docUrl", attributeValue & avS .~ Just(T.pack(docUrl))),
    (":s", to status)
    ]

updateVerificationResult :: DBInfo -> CorrelationID -> VerificationStatus -> Maybe UTCTime -> Maybe String -> IO ()
updateVerificationResult db correlationId status expiredAt reason = updateVerification db correlationId updateExpr updateValues
    where 
        toExpiredAt v = [(":expiredAt", toA(v))]
        toReason v = [(":reason", toA v)]
        updateValues = [(":s", to status)] ++ (maybe [] toExpiredAt expiredAt) ++ (maybe [] toReason reason)
        updateExpr = "currentStatus=:s" ++ 
            (if (isJust reason) then ", reason = :reason" else "") ++ 
            (if (isJust expiredAt) then ", expiredAt = :expiredAt" else "")

