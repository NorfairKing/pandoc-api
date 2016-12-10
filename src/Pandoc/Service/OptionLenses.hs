{-# LANGUAGE TemplateHaskell #-}
module Pandoc.Service.OptionLenses where

import           Pandoc.Service.LensTH

import           Text.Pandoc

makeMyLenses ''ReaderOptions
makeMyLenses ''WriterOptions

