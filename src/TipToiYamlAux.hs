module TipToiYamlAux where

import Data.Aeson.Types
import Data.Char

gameYamlOptions = defaultOptions
    { fieldLabelModifier = map fix . map toLower . drop 2
    , allNullaryToStringTag = True
    , omitNothingFields = True
    , constructorTagModifier = rename
    }
       where fix '_' = '-'
             fix c   = c
             -- speaking names for the game variants (the "tag" YAML field)
             rename "CommonGameYaml"  = "common"
             rename "Game6Yaml"       = "bonusstage"
             rename "Game7Yaml"       = "subgamegroups"
             rename "Game8Yaml"       = "gameselect"
             rename "Game9Yaml"       = "productspecific"
             rename "Game10Yaml"      = "endless"
             rename "Game16Yaml"      = "subgameselect"
             rename "Game253Yaml"     = "placeholder"
             rename other             = other
