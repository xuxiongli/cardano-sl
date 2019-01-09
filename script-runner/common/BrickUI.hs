{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUI (ui, handleEvent, defaultState, theMap) where

import           Brick hiding (on)
import           Brick.BChan (BChan)
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.AttrMap as A
import           BrickUITypes
import           Formatting
import           Graphics.Vty (Event (EvKey), Key (KChar))
import           Universum hiding (on, state, when, list)
import           Data.Vector as V
import qualified Graphics.Vty as V

defaultState :: BChan Reply -> AppState
defaultState replyChan = AppState 0 Nothing "" Nothing replyChan defaultList

defaultList :: L.List Name MenuChoice
defaultList = L.list MainMenu (fromList [ ListProposals, Dummy1, Dummy2 ]) 1

localHeight :: AppState -> Widget Name
localHeight AppState{asLocalHeight} = str $ "Local Block Count: " <> show asLocalHeight

globalHeight :: AppState -> Widget Name
globalHeight AppState{asGlobalHeight} = str $ "global: " <> maybe "unknown" show asGlobalHeight

percent :: AppState -> Widget Name
percent AppState{asLocalHeight,asGlobalHeight} = do
  let
    fmt :: Format Text (Double -> Text)
    fmt = "Percent: " % float % "%"
    go :: Maybe Word64 -> Widget Name
    go (Just global) = txt $ sformat fmt (((fromIntegral asLocalHeight) / (fromIntegral global)) * 100)
    go Nothing = emptyWidget
  go asGlobalHeight

lastMessage :: AppState -> Widget Name
lastMessage AppState{asLastMsg} = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "last debug msg")
  $ padAll 1
  $ strWrap asLastMsg

currentTip :: AppState -> Widget Name
currentTip AppState{asLocalEpochOrSlot} = strWrap $ "Local Slot: " <> show asLocalEpochOrSlot

ui :: AppState -> [ Widget Name ]
ui state = [ vBox [ localHeight state, globalHeight state, percent state, lastMessage state, currentTip state, actionsList state ] ]

actionsList :: AppState -> Widget Name
actionsList state = do
  let
    renderRow :: Bool -> MenuChoice -> Widget Name
    renderRow hasFocus name = do
      if hasFocus
        then withAttr customAttr (str $ "<" <> (show name) <> ">")
        else str $ show name
  L.renderList renderRow True (asChoiceList state)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (customAttr, fg V.cyan)
  ]

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent state (VtyEvent evt) = do
  newlist <- L.handleListEvent evt (asChoiceList state)
  let
    state2 = state { asChoiceList = newlist }
  case evt of
    (EvKey (KChar 'q') []) -> do
      halt state2
    _ -> continue $ state2 { asLastMsg = show evt }

handleEvent state (AppEvent ae) = do
  case ae of
    CENodeInfo (NodeInfo{niLocalHeight,niGlobalHeight,niLocalEpochOrSlot}) -> do
      continue $ state
        { asLocalHeight = niLocalHeight
        , asGlobalHeight = niGlobalHeight
        , asLocalEpochOrSlot = Just niLocalEpochOrSlot
        }
    QuitEvent -> halt state
    CESlotStart (SlotStart e s) -> continue $ state { asLastMsg = (show e) <> " " <> (show s) }

handleEvent state evt = do
  continue $ state { asLastMsg = show evt }
