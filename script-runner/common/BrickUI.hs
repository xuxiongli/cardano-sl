{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUI (drawUi, handleEvent, defaultState, theMap) where

import           Brick
import qualified Brick.AttrMap as A
import           Brick.BChan (BChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.List as L
import           BrickUITypes
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import           Formatting
import           Graphics.Vty (Event (EvKey), Key (KChar, KEnter))
import qualified Graphics.Vty as V
import           Pos.Chain.Update (ConfirmedProposalState (cpsUpdateProposal),
                     SystemTag (getSystemTag), UpdateData,
                     UpdateProposal (upData, upSoftwareVersion))
import           Universum hiding (HashMap, list, on, state, when)

import qualified Data.Text as T

import qualified Brick.Widgets.ProgressBar as P

defaultState :: BChan Reply -> AppState
defaultState replyChan = AppState 0 Nothing "" Nothing replyChan defaultList MainScreen emptyList

defaultList :: L.List Name MenuChoice
defaultList = L.list MainMenu (V.fromList [ ListProposals, Dummy1, Dummy2 ]) 1

emptyList :: L.List Name a
emptyList = L.list None (V.fromList []) 1

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

progressBar :: AppState -> Widget Name
progressBar AppState{asLocalHeight,asGlobalHeight} = do
  let
    fmt :: Format Text (Float -> Text)
    fmt = "Percent: " % float % "%"
    go :: Maybe Word64 -> Widget Name
    go (Just global) = do
      let
        percent :: Float
        percent = (fromIntegral asLocalHeight) / (fromIntegral global)
      P.progressBar (Just $ T.unpack $ sformat fmt percent) percent
    go Nothing = emptyWidget
  go asGlobalHeight

lastMessage :: AppState -> Widget Name
lastMessage AppState{asLastMsg} = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "last debug msg")
  $ padAll 1
  $ strWrap asLastMsg

currentTip :: AppState -> Widget Name
currentTip AppState{asLocalEpochOrSlot} = strWrap $ "Local Slot: " <> show asLocalEpochOrSlot

drawUi :: AppState -> [ Widget Name ]
drawUi state = do
  case (asCurrentScreen state) of
    MainScreen      -> ui state
    ProposalListing -> proposalUi state

ui :: AppState -> [ Widget Name ]
ui state = [ vBox [ localHeight state, globalHeight state, percent state, progressBar state, lastMessage state, currentTip state, actionsList state ] ]

proposalUi :: AppState -> [ Widget Name ]
proposalUi state = do
  let
    renderProposal :: ConfirmedProposalState -> Widget Name
    renderProposal proposal = do
      let
        rawProposal = cpsUpdateProposal proposal
        version :: Widget Name
        version = padLeftRight 1 $ str $ show $ upSoftwareVersion rawProposal
        updata :: [ (SystemTag, UpdateData) ]
        updata = HM.toList $ upData rawProposal
        renderUpData :: (SystemTag,UpdateData) -> Widget Name
        renderUpData (tag,_data) = padLeftRight 1 $ str $ T.unpack $ getSystemTag tag
      hBox ([ version ] <> (map renderUpData updata))
    proposalList = L.renderList (const $ renderProposal) True (asProposalList state)
  [ B.borderWithLabel (str "Proposals") $ padAll 1 proposalList ]

actionsList :: AppState -> Widget Name
actionsList state = do
  let
    renderRow :: Bool -> MenuChoice -> Widget Name
    renderRow _ name = str $ show name
  B.borderWithLabel (str "Main Menu") $ padAll 1 $ L.renderList renderRow True (asChoiceList state)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
  [ (customAttr, fg V.cyan)
  , (L.listAttr,            V.white `on` V.blue)
  , (L.listSelectedAttr,    V.blue `on` V.white)
  , (P.progressCompleteAttr, V.blue `on` V.green)
  , (P.progressIncompleteAttr, V.blue `on` V.red)
  ]

handleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Next AppState)
handleEvent state (VtyEvent evt) = do
  case (asCurrentScreen state) of
    MainScreen -> do
      case evt of
        EvKey (KChar 'q') [] -> do
          halt state
        EvKey KEnter [] -> do
          case L.listSelectedElement (asChoiceList state) of
            Just (_, ListProposals) -> do
              liftIO $ writeBChan (asReplyChan state) QueryProposals
              continue $ state
            Just (_, item) -> do
              continue $ state { asLastMsg = show item }
            Nothing -> do
              continue state
        _ -> do
          newlist <- L.handleListEvent evt (asChoiceList state)
          continue $ state { asLastMsg = show evt, asChoiceList = newlist }
    ProposalListing -> do
      case evt of
        EvKey (KChar 'q') [] -> do
          continue $ state { asCurrentScreen = MainScreen }
        _ -> do
          newlist <- L.handleListEvent evt (asProposalList state)
          continue $ state { asLastMsg = show evt, asProposalList = newlist }

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
    ProposalReply proposals -> do
      continue $ state
        { asCurrentScreen = ProposalListing
        , asProposalList = L.list ProposalName (V.fromList proposals) 1
        }

handleEvent state evt = do
  continue $ state { asLastMsg = show evt }
