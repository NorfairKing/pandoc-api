module Pandoc.Service.LensTH where

import Lens.Micro
import Lens.Micro.TH

import Language.Haskell.TH

addLensLensrules :: LensRules
addLensLensrules =
    lensRules & lensField .~ (\_ _ n -> [TopName (mkName $ nameBase n ++ "L")])

makeMyLenses :: Name -> Q [Dec]
makeMyLenses = makeLensesWith addLensLensrules
