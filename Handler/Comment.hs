module Handler.Comment where

import Import

postCommentR :: Handler Value
postCommentR = do
    -- requireJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    -- (The ToJSON and FromJSON instances are derived in the config/models file).
    --comment <- (requireJsonBody :: Handler Comment)

    now <- liftIO getCurrentTime
    let comment' = Post { postTitle = "标题"
                        , postContent = "内容"
                        , postCreated = now
                        , postUpdated = now
                        }

    insertedComment <- runDB $ insertEntity comment'
    returnJson insertedComment
