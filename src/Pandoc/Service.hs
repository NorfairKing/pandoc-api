{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Pandoc.Service where

import Control.Monad.IO.Class
import Data.List (find)
import Data.Maybe

import Control.Arrow (left)
import Control.Exception (catch, throwIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Monoid
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant hiding (Header)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Process (system)
import Text.Pandoc
import Text.Pandoc.PDF

import Pandoc.Service.API
import Pandoc.Service.Types

runPandocService :: IO ()
runPandocService = run 8081 pandocApp

pandocApp :: Application
pandocApp = serve apiProxy pandocServer

apiProxy :: Proxy PandocAPI
apiProxy = Proxy

pandocServer :: Server PandocAPI
pandocServer = serveConvert

serveConvert :: ConvertRequest -> Handler LB.ByteString
serveConvert cr = getPandoc cr >>= makeResult cr

getPandoc :: ConvertRequest -> Handler Pandoc
getPandoc cr =
    ($ convertContent cr) $
    case convertFrom cr of
        FromJson -> getPandocFromJSON
        FromMarkdown ->
            getPandocFromMarkdown
                (fromMaybe myDefaultReaderOptions $
                 readerOptions =<< convertOptions cr)

getPandocFromJSON :: Value -> Handler Pandoc
getPandocFromJSON c =
    case fromJSON c of
        Error err ->
            throwError $
            err400 {errBody = "Invalid json input: " <> LB8.pack err}
        Success pd -> return pd

getPandocFromMarkdown :: ReaderOptions -> Value -> Handler Pandoc
getPandocFromMarkdown rOpts c =
    case c of
        String s ->
            case readMarkdown rOpts $ T.unpack s of
                Left pde ->
                    throwError $
                    err400
                    { errBody =
                          "Unable to read markdown: " <> LB8.pack (show pde)
                    }
                Right pd -> return pd
        _ ->
            throwError $
            err400
            {errBody = "Invalid md format in JSON (expecting just a String)."}

makeResult :: ConvertRequest -> Pandoc -> Handler LB.ByteString
makeResult cr pd =
    let wOpts =
            (fromMaybe myDefaultServiceWriterOptions $
             writerOptions =<< convertOptions cr)
        func =
            case convertTo cr of
                ToPdf -> makePdf
                ToEpub -> makeEpub
                ToJSON -> makeJSON
    in func wOpts pd

makePdf :: ServiceWriterOptions -> Pandoc -> Handler LB.ByteString
makePdf opts pd = do
    let pdfLatexVersionCmd = "pdflatex --version"
    ec <- liftIO $ system pdfLatexVersionCmd
    case ec of
        ExitSuccess -> pure ()
        ExitFailure c ->
            throwError $
            err500
            { errBody =
                  LB8.unwords
                      [ LB8.pack pdfLatexVersionCmd
                      , "failed with exit code"
                      , LB8.pack $ show c
                      , "unable to perform latex -> pdf conversion"
                      ]
            }
    wOpts <- figureOutTemplateFor opts "latex"
    eepdf <- liftIO $ makePDF "pdflatex" writeLaTeX wOpts pd
    case eepdf of
        Left err ->
            throwError $ err500 {errBody = "Unable to make PDF:\n" <> err}
        Right bs -> pure bs

makeEpub :: ServiceWriterOptions -> Pandoc -> Handler LB.ByteString
makeEpub opts pd = do
    wOpts <- figureOutTemplateFor opts "epub"
    liftIO $ writeEPUB wOpts pd

makeJSON :: ServiceWriterOptions -> Pandoc -> Handler LB.ByteString
makeJSON opts pd = do
    wOpts <- figureOutTemplateFor opts "json"
    pure $ LB8.pack $ writeJSON wOpts pd

figureOutTemplateFor :: ServiceWriterOptions -> String -> Handler WriterOptions
figureOutTemplateFor wopts kind = do
    mtempl <-
        case writerTemplate $ pandocWriterOptions wopts of
            Nothing ->
                case namedTemplate wopts of
                    Nothing -> do
                        eet <-
                            liftIO $
                            (left show <$> getDefaultTemplate (Just ".") kind) `catch`
                            -- Ad-hoc handling of pandoc's ExitFailure's.
                            (\e ->
                                 case e of
                                     ExitFailure 97 ->
                                         pure $
                                         Left $
                                         unwords
                                             [ "Pandoc was unable to find a default template for"
                                             , kind ++ ":"
                                             , show e
                                             ]
                                     _ -> throwIO e)
                        case eet of
                            Left err ->
                                throwError $
                                err500
                                { errBody =
                                      mconcat
                                          [ "Unable to find defaulte template for: "
                                          , LB8.pack kind
                                          , "\n" <> LB8.pack err
                                          ]
                                }
                            Right t -> pure $ Just t
                    Just templateName -> Just <$> getNamedTemplate templateName
            mt -> pure mt
    pure $ (pandocWriterOptions wopts) {writerTemplate = mtempl}

templateDir :: FilePath
templateDir = "templates"

getNamedTemplate :: String -> Handler String
getNamedTemplate name = do
    dirContents <-
        liftIO $
        (listDirectory templateDir) `catch`
        (\e ->
             if isDoesNotExistError e
                 then pure []
                 else throwIO e)
    case find (== name) dirContents of
        Nothing ->
            throwError $
            err404
            { errBody =
                  mconcat ["Named template '", LB8.pack name, "' not found."]
            }
        Just templfn -> liftIO $ readFile $ templateDir </> templfn
