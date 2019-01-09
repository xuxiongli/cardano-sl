{-# LANGUAGE NoImplicitPrelude #-}

module BrickUITypes (AppState(..), Name(..), CustomEvent(..), Reply(..), SlotStart(..), NodeInfo(..), MenuChoice(..)) where

import           Brick.Widgets.List
import           Brick.BChan
import           Pos.Core (EpochOrSlot)
import           Universum hiding (on, state, when)

data AppState = AppState
  { asLocalHeight      :: Word64
  , asGlobalHeight     :: Maybe Word64
  , asLastMsg          :: String
  , asLocalEpochOrSlot :: Maybe EpochOrSlot
  , asReplyChan        :: BChan Reply
  , asChoiceList       :: List Name MenuChoice
  }

data MenuChoice = ListProposals | Dummy1 | Dummy2 deriving (Show, Eq)

data Reply = TriggerShutdown

data SlotStart = SlotStart
  { ssEpoch :: Word64
  , ssSlot  :: Word16
  } deriving Show

data NodeInfo = NodeInfo
  { niLocalHeight      :: Word64
  , niLocalEpochOrSlot :: EpochOrSlot
  , niGlobalHeight     :: Maybe Word64
  } deriving Show

data CustomEvent
    = CESlotStart SlotStart
    | CENodeInfo NodeInfo
    | QuitEvent
    deriving Show

data Name = MainMenu | None deriving (Show, Ord, Eq)
