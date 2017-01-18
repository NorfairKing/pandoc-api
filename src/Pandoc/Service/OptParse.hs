module Pandoc.Service.OptParse where

import Options.Applicative

data Settings = Settings
    { setsPort :: Int
    , setsDataDir :: FilePath
    } deriving (Show, Eq)

getSettings :: IO Settings
getSettings =
    execParser $
    info
        (helper <*> settingsParser)
        (fullDesc <> progDesc "Pandoc REST service." <> header "pandoc-service")

settingsParser :: Parser Settings
settingsParser =
    Settings <$>
    option
        auto
        (mconcat
             [ long "port"
             , short 'p'
             , metavar "PORT"
             , value 8081
             , showDefault
             , help "The port to run the service on"
             ]) <*>
    strOption
        (mconcat
             [ long "data-dir"
             , short 't'
             , metavar "DIRECTORY"
             , value "."
             , showDefault
             , help "The directory to look for templates in"
             ])
