{-# LANGUAGE OverloadedStrings #-}

module Pandoc.Service.Types where

import           Data.Aeson

data ConvertRequest
    = ConvertRequest
    { convertFrom    :: FromFormat
    , convertTo      :: ToFormat
    , convertContent :: Value
    } deriving (Show, Eq)

instance FromJSON ConvertRequest where
    parseJSON (Object o) = ConvertRequest
        <$> o .: "from"
        <*> o .: "to"
        <*> o .: "content"
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

