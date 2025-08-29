{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Maybe (fromMaybe, isJust)
import           Network.Wai (Application, Request, Response, responseLBS, requestMethod, queryString, strictRequestBody)
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types (status200, status400, methodPost)
import           Data.Aeson (eitherDecode, encode)
import           Transform (transformToCypher, transformToCypherList)
import           Notion.Model (DatabaseQuery)

-- | Simple HTTP server exposing transformToCypher as a webhook.
--   POST Notion database query JSON to /?label=MyLabel to get Cypher text.
main :: IO ()
main = run 8080 app

app :: Application
app req respond
  | requestMethod req /= methodPost =
      respond $ responseLBS status400 [("Content-Type", "text/plain")] "POST required"
  | otherwise = do
      body <- strictRequestBody req
      let qs = queryString req
          label = TE.decodeUtf8 $ fromMaybe "NotionPage" (lookup "label" qs >>= id)
          asJSON = isJust (lookup "json" qs)
      case eitherDecode body of
        Left err ->
          respond $ responseLBS status400 [("Content-Type", "text/plain")] (BL.fromStrict $ TE.encodeUtf8 (T.pack err))
        Right dq ->
          if asJSON
            then
              let cyphers = transformToCypherList label (dq :: DatabaseQuery)
              in respond $ responseLBS status200 [("Content-Type", "application/json")] (encode cyphers)
            else
              let cypher = transformToCypher label (dq :: DatabaseQuery)
              in respond $ responseLBS status200 [("Content-Type", "text/plain")] (BL.fromStrict $ TE.encodeUtf8 cypher)
