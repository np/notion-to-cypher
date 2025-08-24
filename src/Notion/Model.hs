{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Notion.Model where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.Aeson.Types (Parser, Value(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import           Data.Scientific (Scientific)

-- Top-level query response (subset)
data DatabaseQuery = DatabaseQuery
  { dqResults :: [Page]
  } deriving (Show, Generic)

instance FromJSON DatabaseQuery where
  parseJSON = withObject "DatabaseQuery" $ \o ->
    DatabaseQuery <$> o .: "results"

data Page = Page
  { pageId           :: T.Text
  , pageArchived     :: Bool
  , pageCreatedTime  :: T.Text
  , pageLastEdited   :: T.Text
  , pageUrl          :: T.Text
  , pageProperties   :: HM.HashMap T.Text PropertyValue
  } deriving (Show, Generic)

instance FromJSON Page where
  parseJSON = withObject "Page" $ \o -> do
    pid <- o .: "id"
    arch <- o .: "archived"
    ct <- o .: "created_time"
    le <- o .: "last_edited_time"
    url <- o .: "url"
    propsVal <- o .: "properties" :: Parser (HM.HashMap T.Text Value)
    props <- traverse parseJSON propsVal
    pure $ Page pid arch ct le url props

-- PropertyValue covers types needed for graph
data PropertyValue
  = PVTitle [RichText]
  | PVRichText [RichText]
  | PVNumber (Maybe Scientific)
  | PVSelect (Maybe SelectOption)
  | PVMultiSelect [SelectOption]
  | PVStatus (Maybe SelectOption)
  | PVDate (Maybe DateRange)
  | PVPeople [UserLite]
  | PVRelation [RelationRef]
  | PVFiles [FileRef]
  | PVCheckbox Bool
  | PVUrl (Maybe T.Text)
  | PVEmail (Maybe T.Text)
  | PVPhone (Maybe T.Text)
  | PVFormula FormulaVal
  | PVUniqueId UniqueId
  | PVCreatedTime (Maybe T.Text)
  | PVLastEditedTime (Maybe T.Text)
  | PVCreatedBy (Maybe UserLite)
  | PVLastEditedBy (Maybe UserLite)
  | PVUnsupported T.Text Value
  deriving (Show, Generic)

instance FromJSON PropertyValue where
  parseJSON = withObject "PropertyValue" $ \o -> do
    t <- o .: "type" :: Parser T.Text
    case t of
      "title"           -> PVTitle <$> (o .: "title" >>= parseRichTextArray)
      "rich_text"       -> PVRichText <$> (o .: "rich_text" >>= parseRichTextArray)
      "number"          -> PVNumber <$> o .:? "number"
      "select"          -> PVSelect <$> (o .:? "select" >>= traverse parseSelectOption)
      "multi_select"    -> PVMultiSelect <$> (o .: "multi_select" >>= parseSelectArray)
      "status"          -> PVStatus <$> (o .:? "status" >>= traverse parseSelectOption)
      "date"            -> PVDate <$> (o .:? "date" >>= traverse parseDateRange)
      "people"          -> PVPeople <$> (o .: "people" >>= parsePeopleArray)
      "relation"        -> PVRelation <$> (o .: "relation" >>= parseRelationArray)
      "files"           -> PVFiles <$> (o .: "files" >>= parseFilesArray)
      "checkbox"        -> PVCheckbox <$> o .: "checkbox"
      "url"             -> PVUrl <$> o .:? "url"
      "email"           -> PVEmail <$> o .:? "email"
      "phone_number"    -> PVPhone <$> o .:? "phone_number"
      "formula"         -> PVFormula <$> (o .: "formula" >>= parseFormulaVal)
      "unique_id"       -> PVUniqueId <$> (o .: "unique_id" >>= parseUniqueId)
      "created_time"    -> PVCreatedTime <$> o .:? "created_time"
      "created_by"      -> PVCreatedBy <$> (o .:? "created_by" >>= traverse parseUserLite)
      "last_edited_time"-> PVLastEditedTime <$> o .:? "last_edited_time"
      "last_edited_by"  -> PVLastEditedBy <$> (o .:? "last_edited_by" >>= traverse parseUserLite)
      other             -> pure (PVUnsupported other (Object o))

-- Minimal sub-objects

data RichText = RichText { rtPlain :: T.Text } deriving (Show, Generic)
parseRichTextArray :: Value -> Parser [RichText]
parseRichTextArray = withArray "RichText[]" $ \arr ->
  mapM (\v -> withObject "RichText" (\o -> RichText <$> o .: "plain_text") v) (V.toList arr)

data SelectOption = SelectOption
  { soId    :: T.Text
  , soName  :: T.Text
  , soColor :: Maybe T.Text
  } deriving (Show, Generic)

parseSelectOption :: Value -> Parser SelectOption
parseSelectOption = withObject "SelectOption" $ \o ->
  SelectOption <$> o .: "id" <*> o .: "name" <*> o .:? "color"

parseSelectArray :: Value -> Parser [SelectOption]
parseSelectArray = withArray "SelectOption[]" $ \arr -> mapM parseSelectOption (V.toList arr)

data DateRange = DateRange { drStart :: T.Text, drEnd :: Maybe T.Text, drTZ :: Maybe T.Text }
  deriving (Show, Generic)

parseDateRange :: Value -> Parser DateRange
parseDateRange = withObject "Date" $ \o ->
  DateRange <$> o .: "start" <*> o .:? "end" <*> o .:? "time_zone"

data UserLite = UserLite { uId :: T.Text, uName :: Maybe T.Text, uType :: Maybe T.Text }
  deriving (Show, Generic)

parseUserLite :: Value -> Parser UserLite
parseUserLite = withObject "User" $ \o ->
  UserLite <$> o .: "id" <*> o .:? "name" <*> o .:? "type"

parsePeopleArray :: Value -> Parser [UserLite]
parsePeopleArray = withArray "User[]" $ \arr -> mapM parseUserLite (V.toList arr)

data RelationRef = RelationRef { rrId :: T.Text } deriving (Show, Generic)

parseRelationArray :: Value -> Parser [RelationRef]
parseRelationArray = withArray "Relation[]" $ \arr ->
  mapM (\v -> withObject "Relation" (\o -> RelationRef <$> o .: "id") v) (V.toList arr)

data FileRef = FileRef { frName :: Maybe T.Text, frUrl :: T.Text } deriving (Show, Generic)

parseFilesArray :: Value -> Parser [FileRef]
parseFilesArray = withArray "Files[]" $ \arr -> mapM parseOne (V.toList arr)
  where
    parseOne = withObject "File" $ \o -> do
      name <- o .:? "name"
      mExt <- o .:? "external"
      mFile <- o .:? "file"
      url <- case (mExt, mFile) of
        (Just ex, _) -> withObject "External" (\e -> e .: "url") ex
        (_, Just fi) -> withObject "FileObj" (\f -> f .: "url") fi
        _            -> fail "File missing external/file url"
      pure (FileRef name url)

data FormulaVal
  = FvString (Maybe T.Text)
  | FvNumber (Maybe Scientific)
  | FvBool   (Maybe Bool)
  | FvDate   (Maybe DateRange)
  | FvUnknown Value
  deriving (Show, Generic)

parseFormulaVal :: Value -> Parser FormulaVal
parseFormulaVal = withObject "Formula" $ \o -> do
  t <- o .: "type" :: Parser T.Text
  case t of
    "string"  -> FvString <$> o .:? "string"
    "number"  -> FvNumber <$> o .:? "number"
    "boolean" -> FvBool <$> o .:? "boolean"
    "date"    -> FvDate <$> (o .:? "date" >>= traverse parseDateRange)
    _         -> pure (FvUnknown (Object o))

data UniqueId = UniqueId { uidPrefix :: Maybe T.Text, uidNumber :: Maybe Scientific }
  deriving (Show, Generic)

parseUniqueId :: Value -> Parser UniqueId
parseUniqueId = withObject "UniqueId" $ \o ->
  UniqueId <$> o .:? "prefix" <*> o .:? "number"
