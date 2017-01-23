{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Pandoc.Service where

import Control.Monad.IO.Class
import Data.List (find)

import Control.Arrow (left)
import Control.Exception (catch, throwIO)
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Monoid
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant hiding (Header)
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Process (readCreateProcessWithExitCode, shell)
import Text.Pandoc
import Text.Pandoc.PDF

import Pandoc.Service.API
import Pandoc.Service.OptParse
import Pandoc.Service.Types

runPandocService :: IO ()
runPandocService = do
    sets <- getSettings
    run (setsPort sets) (pandocApp sets)

pandocApp :: Settings -> Application
pandocApp = serve apiProxy . pandocServer

apiProxy :: Proxy PandocAPI
apiProxy = Proxy

pandocServer :: Settings -> Server PandocAPI
pandocServer = serveConvert

serveConvert :: Settings -> ConvertRequest -> Handler LB.ByteString
serveConvert sets cr = flip runReaderT sets $ getPandoc cr >>= makeResult cr

type PHandler = ReaderT Settings Handler

getPandoc :: ConvertRequest -> PHandler Pandoc
getPandoc cr =
    ($ convertContent cr) $
    case convertFrom cr of
        FromJson -> getPandocFromJSON
        FromMarkdown ->
            getPandocFromMarkdown $ readerOptions $ convertOptions cr

getPandocFromJSON :: Value -> PHandler Pandoc
getPandocFromJSON c =
    case fromJSON c of
        Error err ->
            throwError $
            err400 {errBody = "Invalid json input: " <> LB8.pack err}
        Success pd -> return pd

getPandocFromMarkdown :: ReaderOptions -> Value -> PHandler Pandoc
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

makeResult :: ConvertRequest -> Pandoc -> PHandler LB.ByteString
makeResult cr pd =
    let wOpts = writerOptions $ convertOptions cr
        func =
            case convertTo cr of
                ToPdf -> makePdf
                ToEpub -> makeEpub
                ToJSON -> makeJSON
    in func wOpts pd

makePdf :: ServiceWriterOptions -> Pandoc -> PHandler LB.ByteString
makePdf opts pd = do
    let pdfLatexVersionCmd = "pdflatex --version"
    (ec, sout, serr) <-
        liftIO $ readCreateProcessWithExitCode (shell pdfLatexVersionCmd) ""
    case ec of
        ExitSuccess -> pure ()
        ExitFailure c ->
            throwError $
            err500
            { errBody =
                  LB8.unlines
                      [ LB8.unwords
                            [ LB8.pack pdfLatexVersionCmd
                            , "failed with exit code"
                            , LB8.pack $ show c
                            ]
                      , "unable to perform latex -> pdf conversion"
                      , LB8.pack sout
                      , LB8.pack serr
                      ]
            }
    wOpts <- figureOutTemplateFor opts "latex"
    eepdf <- liftIO $ makePDF "pdflatex" writeLaTeX wOpts pd
    case eepdf of
        Left err ->
            throwError $ err500 {errBody = "Unable to make PDF:\n" <> err}
        Right bs -> pure bs

makeEpub :: ServiceWriterOptions -> Pandoc -> PHandler LB.ByteString
makeEpub opts pd = do
    wOpts <- figureOutTemplateFor opts "epub"
    liftIO $ writeEPUB wOpts pd

makeJSON :: ServiceWriterOptions -> Pandoc -> PHandler LB.ByteString
makeJSON opts pd = do
    wOpts <- figureOutTemplateFor opts "json"
    pure $ LB8.pack $ writeJSON wOpts pd

figureOutTemplateFor :: ServiceWriterOptions -> String -> PHandler WriterOptions
figureOutTemplateFor wopts kind = do
    mtempl <-
        case writerTemplate $ pandocWriterOptions wopts of
            Nothing ->
                case namedTemplate wopts of
                    Nothing -> do
                        dd <- getDataDir
                        eet <-
                            liftIO $
                            (left show <$> getDefaultTemplate (Just dd) kind) `catch`
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

getDataDir :: PHandler FilePath
getDataDir = asks setsDataDir

getNamedTemplate :: String -> PHandler String
getNamedTemplate name = do
    dd <- getDataDir
    dirContents <-
        liftIO $
        (listDirectory $ dd </> templateDir) `catch`
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
