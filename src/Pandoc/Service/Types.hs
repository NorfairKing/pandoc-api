{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

module Pandoc.Service.Types where

import           Control.Monad
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
    , writerOptions :: Maybe WriterOptions
    } deriving (Show)

instance FromJSON ConvertOptions where
    parseJSON (Object o) = do
        rv <- o .:? "reader"
        ros <- case rv of
            Just (Object ro) -> Just <$> defaultDeltaReaderOptions ro
            Nothing -> pure Nothing
            _ -> fail "ReaderOptions should be specified in an 'Object'."
        wv <- o .:? "writer"
        wos <- case wv of
            Just (Object wo) -> Just <$> defaultDeltaWriterOptions wo
            Nothing -> pure Nothing
            _ -> fail "WriterOptions should be specified in an 'Object'."
        pure $ ConvertOptions ros wos
    parseJSON _ = fail $ "ConvertOptions should be specified in an 'object'."

defaultDeltaReaderOptions :: Object -> Parser ReaderOptions
defaultDeltaReaderOptions o = myDefaultReaderOptions &
    -- TODO add readerExtensions
   (    editWhileParsing o "smart" readerSmartL
    >=> editWhileParsing o "standalone" readerStandaloneL
    >=> editWhileParsing o "parseRaw" readerParseRawL
    >=> editWhileParsing o "columns" readerColumnsL
    >=> editWhileParsing o "tabStop" readerTabStopL
    >=> editWhileParsing o "oldDashes" readerOldDashesL
    >=> editWhileParsing o "applyMacros" readerApplyMacrosL
    >=> editWhileParsing o "indentedCodeClasses" readerIndentedCodeClassesL
    >=> editWhileParsing o "defaultImageExtension" readerDefaultImageExtensionL
    >=> editWhileParsing o "trace" readerTraceL
    -- TODO add readerTrackChanges
    >=> editWhileParsing o "fileScope" readerFileScopeL)

myDefaultReaderOptions :: ReaderOptions
myDefaultReaderOptions = def { readerStandalone = True }

defaultDeltaWriterOptions :: Object -> Parser WriterOptions
defaultDeltaWriterOptions o = myDefaultWriterOptions &
   (    editWhileParsing o "Template" writerTemplateL
    -- TODO add writerVariables
    >=> editWhileParsing o "TabStop" writerTabStopL
    >=> editWhileParsing o "TableOfContents" writerTableOfContentsL
    -- TODO add writerSlideVariant
    >=> editWhileParsing o "Incremental" writerIncrementalL
    -- TODO add writerHTMLMathMethod
    >=> editWhileParsing o "IgnoreNotes" writerIgnoreNotesL
    >=> editWhileParsing o "NumberSections" writerNumberSectionsL
    >=> editWhileParsing o "NumberOffset" writerNumberOffsetL
    >=> editWhileParsing o "SectionDivs" writerSectionDivsL
    -- TODO add writerExtensions
    >=> editWhileParsing o "ReferenceLinks" writerReferenceLinksL
    >=> editWhileParsing o "Dpi" writerDpiL
    -- TODO add writerWrapTextL
    >=> editWhileParsing o "Columns" writerColumnsL
    -- TODO add writerEmailObfuscation
    >=> editWhileParsing o "IdentifierPrefix" writerIdentifierPrefixL
    >=> editWhileParsing o "SourceURL" writerSourceURLL
    >=> editWhileParsing o "UserDataDir" writerUserDataDirL
    -- TODO add writerCiteMethod
    >=> editWhileParsing o "Docbook5" writerDocbook5L
    >=> editWhileParsing o "Html5" writerHtml5L
    >=> editWhileParsing o "HtmlQTags" writerHtmlQTagsL
    >=> editWhileParsing o "Beamer" writerBeamerL
    >=> editWhileParsing o "SlideLevel" writerSlideLevelL
    -- TODO add writerTopLevelDivision
    >=> editWhileParsing o "Listings" writerListingsL
    >=> editWhileParsing o "Highlight" writerHighlightL
    -- TODO add writerHighlightStyle
    >=> editWhileParsing o "SetextHeaders" writerSetextHeadersL
    >=> editWhileParsing o "TeXLigatures" writerTeXLigaturesL
    -- TODO add writerEpubVersion
    >=> editWhileParsing o "EpubMetadata" writerEpubMetadataL
    >=> editWhileParsing o "EpubStylesheet" writerEpubStylesheetL
    -- TODO add writerEpubFonts
    >=> editWhileParsing o "EpubChapterLevel" writerEpubChapterLevelL
    >=> editWhileParsing o "TOCDepth" writerTOCDepthL
    >=> editWhileParsing o "ReferenceODT" writerReferenceODTL
    >=> editWhileParsing o "ReferenceDocx" writerReferenceDocxL
    -- TODO add writerMediaBag
    >=> editWhileParsing o "Verbose" writerVerboseL)
    -- TODO add writerLaTeXArgs
    -- TODO add writerReferenceLocationL

myDefaultWriterOptions :: WriterOptions
myDefaultWriterOptions = def { writerStandalone = True }

editWhileParsing :: FromJSON a => Object -> Text -> Lens' o a -> o -> Parser o
editWhileParsing o label labelL opts = do
    mv <- o .:? label
    pure $ case mv of
        Nothing -> opts
        Just b -> opts & labelL .~ b

