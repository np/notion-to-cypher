{-# LANGUAGE OverloadedStrings #-}
module Cypher.Emit
  ( CyVal(..)
  , emitMap
  , emitList
  , emitString
  , sanitizeProp
  , sanitizeRel
  , sanitizeLabel
  ) where

import qualified Data.Text as T
import           Data.Scientific (Scientific)
import           Data.Char (isAlphaNum, toLower, toUpper)

data CyVal
  = CyS T.Text
  | CyN Scientific
  | CyB Bool
  | CyL [CyVal]

emitString :: T.Text -> T.Text
emitString t = T.concat ["'", escape t, "'"]
  where
    escape = T.concatMap esc
    esc '\'' = "\\'"
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = T.singleton c

emitVal :: CyVal -> T.Text
emitVal (CyS s) = emitString s
emitVal (CyN n) = T.pack (show n)
emitVal (CyB b) = if b then "true" else "false"
emitVal (CyL xs) = emitList xs

emitList :: [CyVal] -> T.Text
emitList xs = T.concat ["[", T.intercalate ", " (map emitVal xs), "]"]

emitMap :: [(T.Text, CyVal)] -> T.Text
emitMap kvs =
  let pairs = [ T.concat [sanitizeProp k, ": ", emitVal v] | (k,v) <- kvs ]
  in T.concat ["{", T.intercalate ", " pairs, "}"]

sanitizeProp :: T.Text -> T.Text
sanitizeProp = safeLower
  where
    safeLower t =
      let t' = T.map toLower $ T.map (\c -> if isAlphaNum c then c else '_') t
          prefixed = if T.null t' || not (isAlphaNum (T.head t')) || T.head t' `elem` ['0'..'9'] then T.cons 'p' (T.cons '_' t') else t'
      in squash prefixed
    squash = T.intercalate "_" . filter (not . T.null) . T.split (=='_')

sanitizeRel :: T.Text -> T.Text
sanitizeRel t =
  let base = T.map (\c -> if isAlphaNum c then toUpper c else '_') t
      b1   = if T.null base || T.head base `elem` ['0'..'9'] then T.cons 'R' (T.cons '_' base) else base
  in T.intercalate "_" $ filter (not . T.null) (T.split (=='_') b1)

sanitizeLabel :: T.Text -> T.Text
sanitizeLabel t =
  let base = T.map (\c -> if isAlphaNum c then c else '_') t
      b2   = if T.null base || T.head base `elem` ['0'..'9'] then T.cons 'L' (T.cons '_' base) else base
  in b2
