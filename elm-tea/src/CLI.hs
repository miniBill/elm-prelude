{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module CLI
  ( CLI
  , Cmd
  , InputType(..)
  , Program(..)
  , Sub
  , button
  , element
  , input
  , row
  , run
  , run_
  , sandbox
  , text
  ) where

import qualified CLI.Focus          as Focus
import qualified CLI.Layout         as Layout
import           CLI.Types          (CLI, InputType (..), Program (..), button,
                                     input, row, text)
import           CLI.Types.Internal (Cmd (..), Focus (..), Sub (..))
import qualified Cmd
import           Compat             (Monad (..), TChan, atomically, forever,
                                     forkIO, liftIO, mapM_, newTChanIO, orElse,
                                     readTChan, sequence, writeTChan)
import qualified Compat
import           Graphics.Vty       (Vty)
import qualified Graphics.Vty       as Vty

import qualified Maybe
import qualified Sub

--import qualified Char
--import           CLI.Attributes     (Attribute (..))
--import qualified Graphics.Vty.Output.Interface as Vty
--import qualified List
--import qualified String
run_ :: Program () model msg -> IO ()
run_ = run ()

run :: flags -> Program flags model msg -> IO ()
run flags (Program init view update subscriptions) =
  let (model, initialCmd) = init flags
   in do cmdTChan <- newTChanIO
         msgTChan <- newTChanIO
         atomically $ writeTChan cmdTChan initialCmd
         _ <- forkIO $ worker cmdTChan msgTChan
         cfg <- Vty.standardIOConfig
         vty <- Vty.mkVty $ cfg {Vty.mouseMode = Just True}
         mainLoop cmdTChan msgTChan vty view update subscriptions model
         Vty.shutdown vty

data Msg msg
  = Terminate
  | Focus (Maybe Focus)
  | Msg msg

worker :: TChan (Cmd msg) -> TChan (Msg msg) -> IO ()
worker cmdTChan msgTChan =
  forever $ do
    cmd <- atomically $ readTChan cmdTChan
    msgs <- sequence $ runCmd cmd
    mapM_ (atomically . writeTChan msgTChan . Msg) msgs

mainLoop ::
     TChan (Cmd msg)
  -> TChan (Msg msg)
  -> Vty
  -> (model -> CLI msg)
  -> (msg -> model -> (model, Cmd msg))
  -> (model -> Sub msg)
  -> model
  -> IO ()
mainLoop cmdTChan msgTChan vty view update _ initialModel =
  let readVty root focus = do
        event <- readTChan $ Vty._eventChannel $ Vty.inputIface vty
        case eventToMsgs root focus event of
          [] -> readVty root focus
          (msg:msgs) -> do
            mapM_ (writeTChan msgTChan) msgs
            return msg
      go focus model = do
        let root = view model
        (sizeW, sizeH) <- Vty.displayBounds $ Vty.outputIface vty
        Vty.update vty $
          Layout.display
            (Compat.fromIntegral sizeW, Compat.fromIntegral sizeH)
            root
        case Maybe.andThen (Focus.getFocusPosition root) focus of
          Just (r, c) -> do
            Vty.showCursor $ Vty.outputIface vty
            Vty.setCursorPos
              (Vty.outputIface vty)
              (Compat.fromIntegral r)
              (Compat.fromIntegral c)
          Nothing -> Vty.hideCursor $ Vty.outputIface vty
        msg <-
          liftIO $ atomically $ orElse (readTChan msgTChan) (readVty root focus)
        case msg of
          Terminate -> return () -- Exit
          Focus focus' -> go focus' model
          Msg mmsg -> do
            let (model', cmd') = update mmsg model
            atomically $ writeTChan cmdTChan cmd'
            -- TODO: use subscriptions
            go focus model'
   in go (Focus.initialFocus $view initialModel) initialModel

eventToMsgs :: CLI msg -> Maybe Focus -> Vty.Event -> List (Msg msg)
eventToMsgs root _ (Vty.EvMouseUp x y _) =
  onClick (Compat.fromIntegral x) (Compat.fromIntegral y) root
eventToMsgs _ _ (Vty.EvKey Vty.KEsc []) = [Terminate]
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KChar '\t') []) =
  [ Focus $
    case Focus.nextFocus root focus of
      Just f  -> Just f
      Nothing -> Focus.initialFocus root
  ]
eventToMsgs root Nothing (Vty.EvKey (Vty.KChar '\t') []) =
  [Focus $ Focus.initialFocus root]
eventToMsgs root (Just focus) (Vty.EvKey (Vty.KBackTab) []) =
  [ Focus $
    case Focus.previousFocus root focus of
      Just f  -> Just f
      Nothing -> Focus.finalFocus root
  ]
eventToMsgs root Nothing (Vty.EvKey (Vty.KBackTab) []) =
  [Focus $ Focus.finalFocus root]
eventToMsgs root (Just focus) (Vty.EvKey key modifiers) =
  onKeyUp key modifiers root focus
eventToMsgs _ _ _ = []

{-
mapFocus :: (Focus -> Focus) -> List (Msg msg) -> List (Msg msg)
mapFocus f =
  List.map
    (\case
       Focus fo -> Focus $ Maybe.map f fo
       fo -> fo)
-}
onKeyUp :: Vty.Key -> List Vty.Modifier -> CLI msg -> Focus -> List (Msg msg)
onKeyUp _ _ _ _ = [] {- key modifiers =
  let containerKeyUp :: List (CLI msg) -> Int -> Focus -> List (Msg msg)
      containerKeyUp children i cfocus =
        children & List.drop i & List.head &
        (\case
           Nothing -> [Focus Nothing]
           Just x -> go x cfocus & mapFocus (ChildFocus i))
      go :: CLI msg -> Focus -> List (Msg msg)
      go (Border child) focus = go child focus
      go (Attributes _ child) focus = go child focus
      go (Input _ v onInput) (This i) = onInputKeyUp v onInput i key modifiers
      go (Container _ _ children) (ChildFocus i cfocus) =
        containerKeyUp children i cfocus
      go (Text _) _ = [Focus Nothing]
      go _ _ = [Focus Nothing]
   in go

onInputKeyUp ::
     String
  -> (String -> msg)
  -> Int
  -> Vty.Key
  -> List Vty.Modifier
  -> List (Msg msg)
onInputKeyUp v onInput i key modifiers =
  let inner Vty.KBS
        | i > 0 =
          [ Msg $ onInput $ String.left (i - 1) v ++ String.dropLeft i v
          , Focus $ Just $ This (i - 1)
          ]
      inner (Vty.KChar char) =
        let char' =
              if List.any
                   (\m ->
                      case m of
                        Vty.MShift -> True
                        _          -> False)
                   modifiers
                then Char.toUpper char
                else char
            v' =
              String.left i v ++ String.fromList [char'] ++ String.dropLeft i v
         in [Msg $ onInput v', Focus $ Just $ This (i + 1)]
      inner Vty.KLeft
        | i > 0 = [Focus $ Just $ This $ i - 1]
      inner Vty.KRight
        | i < String.length v = [Focus $ Just $ This $ i + 1]
      inner Vty.KHome = [Focus $ Just $ This 0]
      inner Vty.KEnd = [Focus $ Just $ This $ String.length v]
      inner _ = [Focus $ Just $ This i]
   in inner key
-}

onClick :: Int -> Int -> CLI msg -> List (Msg msg)
onClick _ _ _ = []

{-
onClick _ _ (Text _) = [Focus Nothing]
onClick relx rely (Container layout alignment children) =
  Layout.childrenPositions layout alignment children &
  List.indexedMap (\i (pos, child) -> (i, pos, child)) &
  List.filter
    (\(_, (cx, cy), child) ->
       let (cwidth, cheight) = Layout.getSize child
        in cx <= relx && relx < cx + cwidth && cy <= rely && rely < cy + cheight) &
  List.head &
  (\found ->
     case found of
       Nothing -> [Focus Nothing]
       Just (i, (cx, cy), child) ->
         onClick (relx - cx) (rely - cy) child & mapFocus (ChildFocus i))
onClick relx rely (Border child) =
  let (w, h) = Layout.getSize child
   in if relx < (w + 2) && rely < (h + 2)
        then onClick (relx - 1) (rely - 1) child
        else [Focus Nothing]
onClick relx rely (Input _ v _) =
  let (w, h) = (max 10 $ String.length v + 1, 1)
   in if relx < (w + 2) && rely < (h + 2)
        then [Focus $ Just $ This 0]
        else [Focus Nothing]
onClick relx rely (Attributes as child) =
  let (w, h) = Layout.getSize child
   in if relx < w && rely < h
        then (List.filterMap
                (\attr ->
                   case attr of
                     OnClick msg -> Just $ Msg msg
                     _           -> Nothing)
                as) ++
             (List.filter isFocus $ onClick relx rely child)
        else [Focus Nothing]

isFocus :: Msg msg -> Bool
isFocus =
  \case
    Focus _ -> True
    _ -> False
-}
sandbox ::
     model
  -> (model -> CLI msg)
  -> (msg -> model -> model)
  -> Program () model msg
sandbox init view update =
  let init' _ = (init, Cmd.none)
      update' msg model = (update msg model, Cmd.none)
      subscriptions _ = Sub.none
   in Program init' view update' subscriptions

element ::
     (flags -> (model, Cmd msg))
  -> (model -> CLI msg)
  -> (msg -> model -> (model, Cmd msg))
  -> (model -> Sub msg)
  -> Program flags model msg
element = Program
