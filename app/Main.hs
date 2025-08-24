{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Options.Applicative
import           Data.Aeson (eitherDecode)
import           Notion.Model
import           Transform

data Options = Options
  { optInput :: Maybe FilePath
  , optLabel :: T.Text
  }

opts :: Parser Options
opts = Options
  <$> optional (strOption (long "input" <> short 'i' <> metavar "FILE" <> help "Input JSON (default: stdin)"))
  <*> ( T.pack <$> strOption (long "label" <> short 'l' <> metavar "LABEL" <> value "NotionPage" <> showDefault <> help "Label for page nodes") )

main :: IO ()
main = do
  Options mIn label <- execParser $ info (opts <**> helper) (fullDesc)
  bs <- maybe BL.getContents BL.readFile mIn
  case eitherDecode bs of
    Left e -> fail e
    Right dq -> TIO.putStrLn $ transformToCypher label dq
