{-# LANGUAGE OverloadedStrings #-}
module Cypher.Emit
  ( CyVal(..)
  , emitMap
  , emitList
  , emitString
  , quote
  ) where

import qualified Data.Text as T
import           Data.Scientific (Scientific)

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
  let pairs = [ T.concat [quote k, ": ", emitVal v] | (k,v) <- kvs ]
  in T.concat ["{", T.intercalate ", " pairs, "}"]


quote :: T.Text -> T.Text
quote t = T.concat ["`", T.replace "`" "``" t, "`"]
