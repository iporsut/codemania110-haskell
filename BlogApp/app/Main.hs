{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
module Main where

import           Data.Text (Text)
import           Yesod
import           Data.Time (UTCTime)
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runStderrLoggingT)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Network.HTTP.Types.Status
import           Data.Aeson
import           Data.String
import           Network.Wai

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Article
    title Text
    content Text
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show
|]

instance ToJSON (Entity Article) where
  toJSON (Entity pid Article{..}) = object [ "id" .= pid
                                           , "title" .= articleTitle
                                           , "content" .= articleContent
                                           , "created_at" .= articleCreatedAt
                                           , "updated_at" .= articleUpdatedAt
                                           ]

data ArticleParam =
  ArticleParam
    Text -- title
    Text -- content
    deriving (Show)

instance FromJSON ArticleParam where
  parseJSON = withObject "ArticleParam" $ \v -> ArticleParam
    <$> v .: "title"
    <*> v .: "content"

data BlogApp = BlogApp ConnectionPool

mkYesod "BlogApp" [parseRoutes|
/articles      ArticlesR GET POST
/articles/#ArticleId ArticleR GET POST DELETE
|]

instance Yesod BlogApp

instance YesodPersist BlogApp where
  type YesodPersistBackend BlogApp = SqlBackend

  runDB action = do
    BlogApp pool <- getYesod
    runSqlPool action pool

-- GET /articles -- List Article
getArticlesR :: Handler Value
getArticlesR = do
  articles <- runDB $ selectList [] [Desc ArticleUpdatedAt]
  returnJson articles

-- POST /articles -- Create Article
postArticlesR :: Handler ()
postArticlesR = do
  pResult <- hEitherDecodeStrict'
  case pResult of
    Left e -> sendResponseStatus status400 (fromString e :: Text)
    Right (ArticleParam title content) -> do
      now <- lift getCurrentTime
      let article = Article { articleTitle = title
                            , articleContent = content
                            , articleCreatedAt = now
                            , articleUpdatedAt = now
                            }
      runDB $ insert article
      sendResponseStatus status201 ("CREATED" :: Text)

-- GET /articles/#ArticleId -- Get Article by Id
getArticleR :: ArticleId -> Handler Value
getArticleR articleId = do
  mArticle <- runDB $ getEntity articleId
  case mArticle of
    Nothing -> sendResponseStatus status404 ("Not Found" :: Text)
    Just article -> returnJson article

-- POST /articles/#ArticleId -- Update Article by Id
postArticleR :: ArticleId -> Handler ()
postArticleR articleId = do
  pResult <- hEitherDecodeStrict'
  case pResult of
    Left e -> sendResponseStatus status400 (fromString e :: Text)
    Right (ArticleParam title content) -> do
      now <- lift getCurrentTime
      runDB $ update articleId [ ArticleTitle =. title
                               , ArticleContent =. content
                               , ArticleUpdatedAt =. now
                               ]
      sendResponseStatus status200 ("UPDATED" :: Text)

-- DELETE /articles/#ArticleId -- Delete Article by Id
deleteArticleR :: ArticleId -> Handler ()
deleteArticleR articleId = do
  runDB $ delete articleId
  sendResponseStatus status200 ("DELETED" :: Text)

-- Parse JSON AritcleParam
hEitherDecodeStrict' :: Handler (Either String ArticleParam)
hEitherDecodeStrict' = do
  wr <- waiRequest
  body <- liftIO $ requestBody wr
  return $ eitherDecodeStrict' body

-- Postgresql Connection String
connStr = "host=0.0.0.0 dbname=test user=postgres password=postgres port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
  warp 8080 $ BlogApp pool
