{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Pandoc.Service where

import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.Monoid
import qualified Data.Text                  as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant                    hiding (Header)
import           Text.Pandoc
import           Text.Pandoc.PDF

import           Pandoc.Service.API
import           Pandoc.Service.Types

runPandocService :: IO ()
runPandocService = run 8081 pandocApp

pandocApp :: Application
pandocApp = serve apiProxy pandocServer

apiProxy :: Proxy PandocAPI
apiProxy = Proxy

pandocServer :: Server PandocAPI
pandocServer = serveConvert

serveConvert :: ConvertRequest -> Handler LB.ByteString
serveConvert = getPandoc >=> makePdf

makePdf :: Pandoc -> Handler LB.ByteString
makePdf pd = do
    Right template <- liftIO $ getDefaultTemplate Nothing "latex"

    let wOpts = def
            { writerStandalone = True
            , writerTemplate = template
            }
    eepdf <- liftIO $ makePDF "pdflatex" writeLaTeX wOpts pd
    case eepdf of
        Left err -> throwError $ err500 { errBody = "Unable to make PDF:\n" <> err }
        Right bs -> pure bs

getPandoc :: ConvertRequest -> Handler Pandoc
getPandoc cr = ($ convertContent cr) $ case convertFrom cr of
    FromJson -> getPandocFromJSON
    FromMarkdown -> getPandocFromMarkdown

getPandocFromJSON :: Value -> Handler Pandoc
getPandocFromJSON c = case fromJSON c of
    Error err -> throwError $ err400 { errBody = "Invalid json input: " <> LB8.pack err }
    Success pd -> return pd

getPandocFromMarkdown :: Value -> Handler Pandoc
getPandocFromMarkdown c = case c of
    String s -> do
        let rOpts = def
                { readerStandalone = True
                }
        case readMarkdown rOpts $ T.unpack s of
            Left pde -> throwError $ err400 { errBody = "Unable to read markdown: " <> LB8.pack (show pde) }
            Right pd -> return pd
    _ -> throwError $ err400
        { errBody = "Invalid md format in JSON (expecting just a String)." }

