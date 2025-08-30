{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Options.Applicative
import           Data.Aeson (eitherDecode, encode, object, (.=))
import           Notion.Model
import           Transform

data Options = Options
  { optInput :: Maybe FilePath
  , optLabel :: T.Text
  , optJSON  :: Bool
  }

opts :: Parser Options
opts = Options
  <$> optional (strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input JSON (default: stdin)"))
  <*> ( T.pack <$> strOption (long "label" <> short 'l' <> metavar "LABEL" <> value "NotionPage" <> showDefault <> help "Label for page nodes") )
  <*> switch (long "json" <> help "Output JSON array of Cypher queries")

main :: IO ()
main = do
  Options mIn label asJSON <- execParser $ info (opts <**> helper) (fullDesc)
  bs <- maybe BL.getContents BL.readFile mIn
  case eitherDecode bs of
    Left e -> fail e
    Right dq ->
      if asJSON
        then
          let stmts = transformToCypherList label dq
              stmtObjs = map (\s -> object ["statement" .= T.snoc s ';']) stmts
          in BL.putStr $ encode (object ["statements" .= stmtObjs])
        else TIO.putStrLn $ transformToCypher label dq
