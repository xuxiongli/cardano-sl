{-# LANGUAGE NoImplicitPrelude #-}

module BrickUITypes (AppState(..), Name(..), CustomEvent(..), Reply(..), SlotStart(..), NodeInfo(..), MenuChoice(..), CurrentScreen(..)) where

import           Brick.BChan
import           Brick.Widgets.List
import           Pos.Core (EpochOrSlot)
import           Universum hiding (on, state, when)

import           Pos.Chain.Update (ConfirmedProposalState)

data AppState = AppState
  { asLocalHeight      :: Word64
  , asGlobalHeight     :: Maybe Word64
  , asLastMsg          :: String
  , asLocalEpochOrSlot :: Maybe EpochOrSlot
  , asReplyChan        :: BChan Reply
  , asChoiceList       :: List Name MenuChoice
  , asCurrentScreen    :: CurrentScreen
  , asProposalList     :: List Name ConfirmedProposalState
  }

data MenuChoice = ListProposals | Dummy1 | Dummy2 deriving (Show, Eq)
data CurrentScreen = MainScreen | ProposalListing deriving (Show, Eq)

data Reply = TriggerShutdown | QueryProposals

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
    | ProposalReply [ConfirmedProposalState]
    deriving Show

data Name = ProposalName | MainMenu | None deriving (Show, Ord, Eq)
