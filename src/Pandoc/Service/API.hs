{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Pandoc.Service.API where

import Servant

import qualified Data.ByteString.Lazy as LB

import Pandoc.Service.Types

type PandocAPI = "convert" :> ReqBody '[ JSON] ConvertRequest :> Post '[ OctetStream] LB.ByteString
