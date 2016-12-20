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
    parseJSON (String "json")     = pure FromJson
    parseJSON (String "markdown") = pure FromMarkdown
    parseJSON c                   = fail $ "Unknown 'from' format: " ++ show c

data ToFormat
    = ToPdf
    | ToEpub
    deriving (Show, Eq)

instance FromJSON ToFormat where
    parseJSON (String "pdf")  = pure ToPdf
    parseJSON (String "epub") = pure ToEpub
    parseJSON c               = fail $ "Unknown 'to' format: " ++ show c

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
   (    editWhileParsing o "template" writerTemplateL
    -- TODO add writerVariables
    >=> editWhileParsing o "tabStop" writerTabStopL
    >=> editWhileParsing o "tableOfContents" writerTableOfContentsL
    -- TODO add writerSlideVariant
    >=> editWhileParsing o "incremental" writerIncrementalL
    -- TODO add writerHTMLMathMethod
    >=> editWhileParsing o "ignoreNotes" writerIgnoreNotesL
    >=> editWhileParsing o "numberSections" writerNumberSectionsL
    >=> editWhileParsing o "numberOffset" writerNumberOffsetL
    >=> editWhileParsing o "sectionDivs" writerSectionDivsL
    -- TODO add writerExtensions
    >=> editWhileParsing o "referenceLinks" writerReferenceLinksL
    >=> editWhileParsing o "dpi" writerDpiL
    -- TODO add writerWrapTextL
    >=> editWhileParsing o "columns" writerColumnsL
    -- TODO add writerEmailObfuscation
    >=> editWhileParsing o "identifierPrefix" writerIdentifierPrefixL
    >=> editWhileParsing o "sourceURL" writerSourceURLL
    >=> editWhileParsing o "userDataDir" writerUserDataDirL
    -- TODO add writerCiteMethod
    >=> editWhileParsing o "docbook5" writerDocbook5L
    >=> editWhileParsing o "html5" writerHtml5L
    >=> editWhileParsing o "htmlQTags" writerHtmlQTagsL
    >=> editWhileParsing o "beamer" writerBeamerL
    >=> editWhileParsing o "slideLevel" writerSlideLevelL
    -- TODO add writerTopLevelDivision
    >=> editWhileParsing o "listings" writerListingsL
    >=> editWhileParsing o "highlight" writerHighlightL
    -- TODO add writerHighlightStyle
    >=> editWhileParsing o "setextHeaders" writerSetextHeadersL
    >=> editWhileParsing o "teXLigatures" writerTeXLigaturesL
    -- TODO add writerEpubVersion
    >=> editWhileParsing o "epubMetadata" writerEpubMetadataL
    >=> editWhileParsing o "epubStylesheet" writerEpubStylesheetL
    -- TODO add writerEpubFonts
    >=> editWhileParsing o "epubChapterLevel" writerEpubChapterLevelL
    >=> editWhileParsing o "tOCDepth" writerTOCDepthL
    >=> editWhileParsing o "referenceODT" writerReferenceODTL
    >=> editWhileParsing o "referenceDocx" writerReferenceDocxL
    -- TODO add writerMediaBag
    >=> editWhileParsing o "verbose" writerVerboseL
    >=> editWhileParsing o "latexArgs" writerLaTeXArgsL)
    -- TODO add writerReferenceLocationL

myDefaultWriterOptions :: WriterOptions
myDefaultWriterOptions = def { writerStandalone = True }

editWhileParsing :: FromJSON a => Object -> Text -> Lens' o a -> o -> Parser o
editWhileParsing o label labelL opts = do
    mv <- o .:? label
    pure $ case mv of
        Nothing -> opts
        Just b  -> opts & labelL .~ b

