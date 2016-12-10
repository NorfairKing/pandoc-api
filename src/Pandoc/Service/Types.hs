{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Pandoc.Service.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text

import           Text.Pandoc

import           Lens.Micro

import           Pandoc.Service.OptionLenses

data ConvertRequest
    = ConvertRequest
    { convertFrom    :: FromFormat
    , convertTo      :: ToFormat
    , convertContent :: Value
    , convertOptions :: Maybe ConvertOptions
    } deriving (Show)

instance FromJSON ConvertRequest where
    parseJSON (Object o) = ConvertRequest
        <$> o .: "from"
        <*> o .: "to"
        <*> o .: "content"
        <*> o .:? "options"
    parseJSON _ = fail "Unable to parse ConvertRequest from non-object."

data FromFormat
    = FromJson
    | FromMarkdown
    deriving (Show, Eq)

instance FromJSON FromFormat where
    parseJSON (String "json") = pure FromJson
    parseJSON (String "markdown") = pure FromMarkdown
    parseJSON c = fail $ "Unknown 'from' format: " ++ show c

data ToFormat
    = ToPdf
    | ToEpub
    deriving (Show, Eq)

instance FromJSON ToFormat where
    parseJSON (String "pdf") = pure ToPdf
    parseJSON (String "epub") = pure ToEpub
    parseJSON c = fail $ "Unknown 'to' format: " ++ show c

data ConvertOptions
    = ConvertOptions
    { readerOptions :: Maybe ReaderOptions
    } deriving (Show)

instance FromJSON ConvertOptions where
    parseJSON (Object o) = do
        rv <- o .:? "reader"
        ros <- case rv of
            Just (Object ro) -> Just <$> defaultDeltaReaderOptinos ro
            Nothing -> pure Nothing
            _ -> fail "ReaderOptions should be specified in an 'object'."
        pure $ ConvertOptions ros
    parseJSON _ = fail $ "ConvertOptions should be specified in an 'object'."


defaultDeltaReaderOptinos :: Object -> Parser ReaderOptions
defaultDeltaReaderOptinos o =
    -- TODO add readerExtensions
        go "smart" readerSmartL def
    >>= go "standalone" readerStandaloneL
    >>= go "parseRaw" readerParseRawL
    >>= go "columns" readerColumnsL
    >>= go "tabStop" readerTabStopL
    >>= go "oldDashes" readerOldDashesL
    >>= go "applyMacros" readerApplyMacrosL
    >>= go "indentedCodeClasses" readerIndentedCodeClassesL
    >>= go "defaultImageExtension" readerDefaultImageExtensionL
    >>= go "trace" readerTraceL
    -- TODO add readerTrackChanges
    >>= go "fileScope" readerFileScopeL
  where
    go :: FromJSON a => Text -> Lens' ReaderOptions a -> ReaderOptions -> Parser ReaderOptions
    go  label labelL  opts = do
        mv <- o .:? label
        pure $ case mv of
            Nothing -> opts
            Just b -> opts & labelL .~ b
