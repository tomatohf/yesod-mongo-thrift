module Handler.Post where

import Import

import Thrift.Protocol.Binary
import Thrift.Transport.Framed
import Thrift.Transport.Handle

import qualified Dao_Client
import Dao_Types

import Text.Read (readMaybe)
import Network
import Control.Monad.Trans.Maybe
import qualified Data.Text.Lazy as Lazy

getPostsR :: Handler Value
getPostsR = do
    let filters = [] :: [Filter Post]
    total <- runDB $ count filters
    limit <- lookupGetParam "limit"
    skip <- lookupGetParam "skip"
    posts <- runDB $ selectList filters [ Desc PostId
                                        , LimitTo $ validLimit limit
                                        , OffsetBy $ validSkip skip
                                        ]
    return $ object ["total" .= total, "posts" .= posts]
  where
    validLimit = (min 100) . (max 1) . (||= 10) . readInt
    validSkip = (max 0) . (||= 0) . readInt
    readInt :: Maybe Text -> Maybe Int
    readInt = (=<<) $ readMaybe . unpack

postPostsR :: Handler Value
postPostsR = do
    result <- validate
    case result of
        Left e -> return e
        Right (title, content) -> do
            now <- liftIO getCurrentTime
            let post = Post { postTitle = title
                            , postContent = content
                            , postCreated = now
                            , postUpdated = now
                            }
            postId <- runDB $ insert post
            return $ object [savedKey .= True, "data" .= postId]


getPostR :: PostId -> Handler Value
getPostR postId = do
    post <- runDB $ get404 postId
    returnJson post

postPostR :: PostId -> Handler Value
postPostR postId = do
    result <- validate
    case result of
        Left e -> return e
        Right (title, content) -> do
            now <- liftIO getCurrentTime
            runDB $ update postId [ PostTitle =. title
                                  , PostContent =. content
                                  , PostUpdated =. now
                                  ]
            return $ object [savedKey .= True]


savedKey :: Text
savedKey = "saved"

type ValidateResult a = Handler (Either Value a)

validate :: ValidateResult (Text, Text)
validate = do
    authenticate
    title <- validateField "title" 128
    content <- validateField "content" 65536
    return $ case [title, content] of
        [Right t, Right c] -> Right (t, c)
        fields @ _ -> Left $ object [savedKey .= False, "errors" .= lefts fields]
  where
    errorJson :: Text -> Text -> Value
    errorJson key reason = object ["field" .= key, "reason" .= reason]
    validateField :: Text -> Int -> ValidateResult Text
    validateField key maxLength = do
        value <- lookupPostParam key
        return $ case value of
            Just v | not $ null v -> if length v > maxLength
                then Left $ errorJson key "超过长度限制"
                else Right v
            _ -> Left $ errorJson key "不能为空"

thriftHost :: HostName
thriftHost = "10.1.1.223"
thriftPort :: PortID
thriftPort = PortNumber 8888

sessionCookie :: Text
sessionCookie = "sessionCookie"
accountIdKey :: Lazy.Text
accountIdKey = "accountIdKey"
accessEnum :: Int32
accessEnum = -1

authenticate :: Handler ()
authenticate = do
    maybeSessionId <- lookupCookie sessionCookie
    access <- (liftIO . runMaybeT . getAccess) maybeSessionId
    case access of
        Just enums | elem accessEnum enums -> return ()
        _ -> permissionDenied "access denied"
  where
    getAccess :: Maybe Text -> MaybeT IO (Vector Int32)
    getAccess maybeSessionId = do
        sessionId <- liftMaybe maybeSessionId
        connection <- lift getDaoConnection
        sessionMap <- lift $ Dao_Client.getSession connection (fromStrict sessionId) (singleton accountIdKey)
        accountId <- liftMaybe $ lookup accountIdKey sessionMap
        MaybeT $ accessResult_data <$> Dao_Client.getAccountAccess connection accountId
    liftMaybe :: Maybe a -> MaybeT IO a
    liftMaybe = MaybeT . return

type DaoProtocol = BinaryProtocol (FramedTransport Handle)

getDaoConnection :: IO (DaoProtocol, DaoProtocol)
getDaoConnection = do
    h <- hOpen (thriftHost, thriftPort)
    transport <- openFramedTransport h
    let protocol = BinaryProtocol transport
    return (protocol, protocol)

(||=) :: Maybe a -> a -> a
(||=) (Just a) _ = a
(||=) Nothing a = a
