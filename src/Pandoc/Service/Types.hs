{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Pandoc.Service.Types where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Maybe
import Data.Text

import Text.Pandoc

import Lens.Micro

import Pandoc.Service.OptionLenses

-- | A request to convert the 'convertContent' from the format 'convertFrom' to the format 'convertTo' according to the 'convertOptions'.
data ConvertRequest = ConvertRequest
    { convertFrom :: FromFormat
    , convertTo :: ToFormat
    , convertContent :: Value
    , convertOptions :: ConvertOptions
    } deriving (Show)

instance FromJSON ConvertRequest where
    parseJSON (Object o) =
        ConvertRequest <$> o .: "from" <*> o .: "to" <*> o .: "content" <*>
        o .:? "options" .!= def
    parseJSON _ = fail "Unable to parse ConvertRequest from non-object."

-- | The format to convert from.
--
-- This is not just a 'String' because only a select few formats are supported.
data FromFormat
    = FromJson
    | FromMarkdown
    deriving (Show, Eq)

instance FromJSON FromFormat where
    parseJSON (String "json") = pure FromJson
    parseJSON (String "markdown") = pure FromMarkdown
    parseJSON c = fail $ "Unknown 'from' format: " ++ show c

-- | The format to convert to.
--
-- See 'FromFormat'.
data ToFormat
    = ToPdf
    | ToEpub
    | ToJSON
    deriving (Show, Eq)

instance FromJSON ToFormat where
    parseJSON (String "pdf") = pure ToPdf
    parseJSON (String "epub") = pure ToEpub
    parseJSON (String "json") = pure ToJSON
    parseJSON c = fail $ "Unknown 'to' format: " ++ show c

-- | Conversion options contain both reader optinos and writer options.
data ConvertOptions = ConvertOptions
    { readerOptions :: ReaderOptions
    , writerOptions :: ServiceWriterOptions
    } deriving (Show)

instance Default ConvertOptions where
    def = ConvertOptions def def

instance FromJSON ConvertOptions where
    parseJSON (Object o) = do
        mros <- o .:? "reader"
        ros <-
            case mros of
                Just (Object ro) -> defaultDeltaReaderOptions ro
                Nothing -> pure def
                _ -> fail "ReaderOptions should be specified in an 'Object'."
        mwos <- o .:? "writer"
        let wos = fromMaybe def mwos
        pure $ ConvertOptions ros wos
    parseJSON _ = fail $ "ConvertOptions should be specified in an 'Object'."

defaultDeltaReaderOptions :: Object -> Parser ReaderOptions
defaultDeltaReaderOptions o =
    def &
    -- TODO add readerExtensions
    (editWhileParsing o "smart" readerSmartL >=>
     editWhileParsing o "standalone" readerStandaloneL >=>
     editWhileParsing o "parseRaw" readerParseRawL >=>
     editWhileParsing o "columns" readerColumnsL >=>
     editWhileParsing o "tabStop" readerTabStopL >=>
     editWhileParsing o "oldDashes" readerOldDashesL >=>
     editWhileParsing o "applyMacros" readerApplyMacrosL >=>
     editWhileParsing o "indentedCodeClasses" readerIndentedCodeClassesL >=>
     editWhileParsing o "defaultImageExtension" readerDefaultImageExtensionL >=>
     editWhileParsing o "trace" readerTraceL
    -- TODO add readerTrackChanges
      >=>
     editWhileParsing o "fileScope" readerFileScopeL)

-- | Writer options are specified in two parts: pandoc 'WriterOptions', as well as an optional 'named template'.
--
-- This allows the specification of a template by name instead of by contents.
data ServiceWriterOptions = ServiceWriterOptions
    { pandocWriterOptions :: WriterOptions
    , namedTemplate :: Maybe String
    } deriving (Show)

instance Default ServiceWriterOptions where
    def = ServiceWriterOptions def Nothing

instance FromJSON ServiceWriterOptions where
    parseJSON (Object o) = do
        wos <- defaultDeltaWriterOptions o
        mnt <- o .:? "named-template"
        pure
            ServiceWriterOptions
            {pandocWriterOptions = wos, namedTemplate = mnt}
    parseJSON _ =
        fail $ "ServiceWriterOptions should be specified in an 'Object'."

defaultDeltaWriterOptions :: Object -> Parser WriterOptions
defaultDeltaWriterOptions o =
    def &
    (editWhileParsing o "template" writerTemplateL >=>
     editWhileParsing o "variables" writerVariablesL >=>
     editWhileParsing o "tabStop" writerTabStopL >=>
     editWhileParsing o "tableOfContents" writerTableOfContentsL
    -- TODO add writerSlideVariant
      >=>
     editWhileParsing o "incremental" writerIncrementalL
    -- TODO add writerHTMLMathMethod
      >=>
     editWhileParsing o "ignoreNotes" writerIgnoreNotesL >=>
     editWhileParsing o "numberSections" writerNumberSectionsL >=>
     editWhileParsing o "numberOffset" writerNumberOffsetL >=>
     editWhileParsing o "sectionDivs" writerSectionDivsL
    -- TODO add writerExtensions
      >=>
     editWhileParsing o "referenceLinks" writerReferenceLinksL >=>
     editWhileParsing o "dpi" writerDpiL
    -- TODO add writerWrapTextL
      >=>
     editWhileParsing o "columns" writerColumnsL
    -- TODO add writerEmailObfuscation
      >=>
     editWhileParsing o "identifierPrefix" writerIdentifierPrefixL >=>
     editWhileParsing o "sourceURL" writerSourceURLL >=>
     editWhileParsing o "userDataDir" writerUserDataDirL
    -- TODO add writerCiteMethod
      >=>
     editWhileParsing o "docbook5" writerDocbook5L >=>
     editWhileParsing o "html5" writerHtml5L >=>
     editWhileParsing o "htmlQTags" writerHtmlQTagsL >=>
     editWhileParsing o "beamer" writerBeamerL >=>
     editWhileParsing o "slideLevel" writerSlideLevelL
    -- TODO add writerTopLevelDivision
      >=>
     editWhileParsing o "listings" writerListingsL >=>
     editWhileParsing o "highlight" writerHighlightL
    -- TODO add writerHighlightStyle
      >=>
     editWhileParsing o "setextHeaders" writerSetextHeadersL >=>
     editWhileParsing o "teXLigatures" writerTeXLigaturesL
    -- TODO add writerEpubVersion
      >=>
     editWhileParsing o "epubMetadata" writerEpubMetadataL >=>
     editWhileParsing o "epubStylesheet" writerEpubStylesheetL
    -- TODO add writerEpubFonts
      >=>
     editWhileParsing o "epubChapterLevel" writerEpubChapterLevelL >=>
     editWhileParsing o "tOCDepth" writerTOCDepthL >=>
     editWhileParsing o "referenceODT" writerReferenceODTL >=>
     editWhileParsing o "referenceDocx" writerReferenceDocxL
    -- TODO add writerMediaBag
      >=>
     editWhileParsing o "verbose" writerVerboseL >=>
     editWhileParsing o "latexArgs" writerLaTeXArgsL)
    -- TODO add writerReferenceLocationL

editWhileParsing
    :: FromJSON a
    => Object -> Text -> Lens' o a -> o -> Parser o
editWhileParsing o label labelL opts = do
    mv <- o .:? label
    pure $
        case mv of
            Nothing -> opts
            Just b -> opts & labelL .~ b
