{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OrphanedLenses where

import           Control.Lens (makeLensesWith)

import           Pos.Util (postfixLFields)
import           Pos.Chain.Update (BlockVersionData, BlockVersionModifier)

makeLensesWith postfixLFields ''BlockVersionModifier
makeLensesWith postfixLFields ''BlockVersionData
