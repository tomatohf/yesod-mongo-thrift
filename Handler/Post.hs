module Handler.Post where

import Import

postPostR :: Handler Value
postPostR = do
    now <- liftIO getCurrentTime
    let post = Post { postTitle = "标题"
                        , postContent = "内容"
                        , postCreated = now
                        , postUpdated = now
                        }

    insertedPost <- runDB $ insertEntity post
    returnJson insertedPost
