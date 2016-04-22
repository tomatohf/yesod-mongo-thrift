module Handler.Post where

import Import
import Text.Read

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

savedKey :: Text
savedKey = "saved"

type ValidateResult a = Handler (Either Value a)

validate :: ValidateResult (Text, Text)
validate = do
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

(||=) :: Maybe a -> a -> a
(||=) (Just a) _ = a
(||=) Nothing a = a
