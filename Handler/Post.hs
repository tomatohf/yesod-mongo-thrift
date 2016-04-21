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
    title <- lookupPostParam "title"
    content <- lookupPostParam "content"
    now <- liftIO getCurrentTime
    let post = Post { postTitle = title ||= ""
                    , postContent = content ||= ""
                    , postCreated = now
                    , postUpdated = now
                    }
    postId <- runDB $ insert post
    return $ object ["saved" .= True, "data" .= postId]

(||=) :: Maybe a -> a -> a
(||=) (Just a) _ = a
(||=) Nothing a = a
