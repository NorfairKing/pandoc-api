{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Pandoc.Service where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                    hiding (Header)
import           Text.Pandoc
import           Text.Pandoc.Options
import           Text.Pandoc.PDF

runPandocService :: IO ()
runPandocService = run 8081 pandocApp

pandocApp :: Application
pandocApp = serve apiProxy pandocServer

apiProxy :: Proxy PandocAPI
apiProxy = Proxy

pandocServer :: Server PandocAPI
pandocServer = serveConvert

serveConvert :: ConvertRequest -> Handler LB.ByteString
serveConvert cr = do
    pd <- getPandoc cr
    makePdf pd

makePdf :: Pandoc -> Handler LB.ByteString
makePdf pd = do
    Right template <- liftIO $ getDefaultTemplate Nothing "latex"

    let wOpts = def
            { writerStandalone = True
            , writerTemplate = template
            }
    eepdf <- liftIO $ makePDF "xelatex" writeLaTeX wOpts pd
    case eepdf of
        Left err -> throwError $ err500 { errBody = err }
        Right bs -> pure bs

getPandoc :: ConvertRequest -> Handler Pandoc
getPandoc cr =
    case convertFrom cr of
        FromJson ->  case fromJSON $ convertContent cr of
            Error err -> throwError $ err400 { errBody = "Invalid json input." }
            Success pd -> return pd
        FromMarkdown -> case convertContent cr of
            String s -> do
                let rOpts = def
                        { readerStandalone = True
                        }
                case readMarkdown rOpts $ T.unpack s of
                    Left pde -> throwError $ err400 { errBody = "Unable to read markdown." }
                    Right pd -> return pd
            _ -> throwError $ err400 { errBody = "Invalid md format in JSON (expecting just a String)." }


type PandocAPI =
        "convert"
    :> ReqBody '[JSON] ConvertRequest
    :> Post '[OctetStream] LB.ByteString

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
    parseJSON _ = mempty

newtype ConvertedDocument
    = ConvertedDocument String
    deriving (Show, Eq)

instance ToJSON ConvertedDocument where
    toJSON (ConvertedDocument s) = toJSON s
