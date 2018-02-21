module Main where

import Prelude
import Types
import UI.Elements
import UI.Events
import UI.Properties
import Data.Record.Unsafe
import Debug.Trace
import Data.Tuple
import FRP as F
import FRP.Event as E
import UI.Util as U
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (stack)
import Control.Monad.RWS (get, state)
import Control.Plus ((<|>))
import DOM.HTML.Event.ErrorEvent (lineNo)
import DOM.HTML.Event.EventTypes (play, reset)
import DOM.HTML.HTMLInputElement (checked)
import DOM.HTML.HTMLMetaElement (content)
import DOM.HTML.History (back)
import DOM.Node.Node (firstChild)
import Data.Array (any, group)
import Data.Array (null, filter, length)
import Data.DateTime (second)
import Data.Foldable (indexl)
import Halogen.VDom.Types (graft)
import UI.Core (MEvent, AttrValue(..), Attr(..), Prop)
import Data.String hiding (null)

foreign import click :: MEvent
foreign import change :: MEvent


getApplyButtonUI state = textView
                           [
                              id_ (state.id_)
                            , height "40"
                            , width "40"
                            , text (state.text)
                            , margin "0,0,0,0"
                            , background "#ffffff"
                            ,stroke "1,#000000"
                            , onClick (Some click)
                            , fontStyle "Source Sans Pro-Regular"
                            , gravity "center"
                           ]
                            
getPlayerStatus state = textView
                           [
                              id_ (state.id_)
                            , height "20"
                            , width "250"
                            , text (state.text)
                            , margin "120,10,10,0"
                            , background (state.buttonApplyColor)
                            , fontStyle "Source Sans Pro-Regular"
                            , gravity "center"
                           ]

getWinnersLayout state = linearLayout
                        [ id_ (state.id_)
                        , height "90"
                        , width "150"
                        , margin "170,20,0,0"
                        , background (state.buttonApplyColor)
                        , gravity "center"
                        , orientation "vertical"
                        ]
                         [
                            textView
                           [
                              id_ (state.id1)
                            , height "20"
                            , width "150"
                            , text (state.text1)
                            , margin "0,0,0,5"
                            , fontStyle "Source Sans Pro-Regular"
                            , gravity "center"
                           ],
                           textView
                           [
                              id_ (state.id2)
                            , height "20"
                            , width "150"
                            , margin "0,0,0,5"
                            , text (state.text2)
                            , fontStyle "Source Sans Pro-Regular"
                            , gravity "center"
                           ],
                           textView
                           [
                              id_ (state.id3)
                            , height "20"
                            , width "150"
                            , margin "0,0,0,5"
                            , text (state.text3)
                            , fontStyle "Source Sans Pro-Regular"
                            , gravity "center"
                           ]
                         ]
                                                        


getControlPane state = linearLayout
                        [ id_ (state.id_)
                        , height "40"
                        , width "400"
                        , margin state.margin
                        , background "#ffffff"
                        , gravity "center"
                        , orientation "horizontal"
                        ]
                        [
                            getApplyButtonUI { id_ : state.id1  , buttonApplyColor : state.buttonApplyColor, text:state.text1}
                          , getApplyButtonUI { id_ : state.id2 ,  buttonApplyColor : state.buttonApplyColor, text:state.text2}
                          , getApplyButtonUI { id_ : state.id3 ,  buttonApplyColor : state.buttonApplyColor, text:state.text3}
                         ]

-- getHyperCanvasHolder state { id_ : "svgContainer" , height : "300" , width : "300" , cubeState : state },

widget state = linearLayout
              [ id_ "1"
              , height "400"
              , width "500"
              , background "#ffffff"
              , orientation "vertical"
              , margin "350,100,0,0"
              , stroke "1,#000000"
              ]
              [
                textView
                           [
                              id_ ("title")
                            , height "40"
                            , width "150"
                            , margin "170,30,0,5"
                            , color "#D87F6B"
                            , text "Tic Tac Toe"
                            , textSize "30"
                            , fontStyle "Source Sans Pro-Regular"
                            , gravity "center"
                           ]
                , getControlPane {id_ : "pane1" , id1 : "firstBox", id2 : "secondBox", id3 : "thirdBox" , buttonApplyColor : "#76b852" , text1 : state.firstText, text2 : state.secondText, text3 : state.thirdText , margin : "50,20,0,0"}
                , getControlPane {id_ : "pane2" , id1 : "fourthBox", id2 : "fifthBox", id3 : "sixthBox" , buttonApplyColor : "#76b852", text1 : state.fourthText, text2 : state.fifthText, text3 : state.sixthText , margin : "50,0,0,0"}
                , getControlPane {id_ : "pane3" , id1 : "seventhBox", id2 : "eightBox", id3 : "ninthBox" , buttonApplyColor : "#76b852", text1 : state.seventhText, text2 : state.eightText, text3 : state.ninthText, margin : "50,0,0,10" }
                , getPlayerStatus { id_ : "playerId" ,  buttonApplyColor : "#76b852", text:state.playername}
                , getWinnersLayout { id_ : "winnerId" , id1 : "w1" , id2 : "w2" , id3 : "w3" , buttonApplyColor : "#76b852", text1: ("X Wins : " <> state.xcount), text2: ("O Wins : " <> state.ocount), text3: ("Tie : " <> state.tcount)}
                
              ]


resetCubeState = do
  _ <- U.updateState "buttonApplyColor" "#C0C0C0"
  _ <- U.updateState "firstText" ""
  _ <- U.updateState "secondText" ""
  _ <- U.updateState "thirdText" ""
  _ <- U.updateState "fourthText" ""
  _ <- U.updateState "fifthText" ""
  _ <- U.updateState "sixthText" ""
  _ <- U.updateState "seventhText" ""
  _ <- U.updateState "eightText" ""
  _ <- U.updateState "ninthText" ""
  _ <- U.updateState "count" 0
  U.updateState "size" "100"
  

main = do
  --- Init State {} empty record--
  U.initializeState

  --- Update State ----
  state <- resetCubeState
  _ <- U.updateState "playername" "Its X's turn!"
  _ <- U.updateState "player" true
  _ <- U.updateState "xcount" 0
  _ <- U.updateState "ocount" 0
  _ <- U.updateState "tcount" 0
  ---- Render Widget ---
  U.render (widget state) listen

  pure unit

getContent state = if state.player then "O" else "X"

checkAndUpdate state key = do
  if key == "" 
    then  
      if state.player 
        then do
          _ <- U.updateState "player" false
          _ <- U.updateState "count" (state.count+1)
          U.updateState "playername" "Its O's turn!"
        else do
          _ <- U.updateState "player" true
          _ <- U.updateState "count" (state.count+1)
          U.updateState "playername" "Its X's turn!"
      else
        U.updateState "player" state.player 

-- checkWinner :: Int -> String -> Array Tuple -> State -> Boolean
checkWinner index lis = do
  state <- U.getState
  let p =  (getContent state)
  let array = filter (\(Tuple x y) -> (p /= unsafeGet x state && p /= unsafeGet y state && unsafeGet x state /= "" && unsafeGet y state /= "")) lis
  _ <-checkAndUpdate state (unsafeGet index state)
  _<- U.updateState index (getContent state)
  _ <- if null array then U.updateState "dfg" "f"
          else do 
            _ <- if state.player 
                  then do
                      _ <- U.updateState "ocount" (state.ocount+1) 
                      _ <- resetCubeState
                      U.updateState "playername" "O Wins. Hey X Give a try again"
                    else do 
                      _ <- U.updateState "xcount" (state.xcount+1)
                      _ <- resetCubeState
                      U.updateState "playername" "X Wins. Hey O Give a try again"
            U.updateState "dfg" "s"            
  if state.count == 9 then do
      _ <- resetCubeState
      _ <- U.updateState "tcount" (state.tcount+1)
      U.updateState "playername" "Game Over. Click anywhere to start "
        else
          U.updateState "dfg" "d"



eval a b c d e f g h i= U.updateState "dfg" "e" 

listen = do
  firstBox <- U.signal "firstBox" false
  secondBox <- U.signal "secondBox" false
  thirdBox <- U.signal "thirdBox" false
  fourthBox <- U.signal "fourthBox" false
  fifthBox <- U.signal "fifthBox" false
  sixthBox <- U.signal "sixthBox" false
  seventhBox <- U.signal "seventhBox" false
  eightBox <- U.signal "eightBox" false
  ninthBox <- U.signal "ninthBox" false
  _ <- firstBox.event `E.subscribe` (\_ -> do
                                      checkWinner "firstText" [(Tuple "secondText" "thirdText"),(Tuple "fourthText" "seventhText"),(Tuple "fifthText" "ninthText")]
                                       )
  _ <- secondBox.event `E.subscribe` (\_ -> do
                                      checkWinner "secondText" [(Tuple "firstText" "thirdText"),(Tuple "fifthText" "eightText")]
                                       )
  _ <- thirdBox.event `E.subscribe` (\_ -> do
                                      checkWinner "thirdText" [(Tuple "firstText" "secondText"),(Tuple "fifthText" "seventhText"),(Tuple "sixthText" "ninthText")]
                                       )
  _ <- fourthBox.event `E.subscribe` (\_ -> do
                                      checkWinner "fourthText" [(Tuple "firstText" "seventhText"),(Tuple "fifthText" "sixthText")]
                                       )
  _ <- fifthBox.event `E.subscribe` (\_ -> do
                                      checkWinner "fifthText" [(Tuple "firstText" "ninthText"),(Tuple "thirdText" "seventhText"),(Tuple "secondText" "eightText"),(Tuple "fourthText" "sixthText")]
                                       )
  _ <- sixthBox.event `E.subscribe` (\_ -> do
                                      checkWinner "sixthText" [(Tuple "fourthText" "fifthText"),(Tuple "thirdText" "ninthText")]
                                       )
  _ <- seventhBox.event `E.subscribe` (\_ -> do
                                      checkWinner "seventhText" [(Tuple "firstText" "fourthText"),(Tuple "eightText" "ninthText"),(Tuple "thirdText" "fifthText")]
                                       )
  _ <- eightBox.event `E.subscribe` (\_ -> do
                                      checkWinner "eightText" [(Tuple "seventhText" "ninthText"),(Tuple "secondText" "fifthText")]
                                       )
  _ <- ninthBox.event `E.subscribe` (\_ -> do
                                      checkWinner "ninthText" [(Tuple "firstText" "fifthText"),(Tuple "seventhText" "eightText"),(Tuple "thirdText" "sixthText")]
                                       )


  -- Cube3D


  let behavior = eval <$> firstBox.behavior <*> secondBox.behavior <*> thirdBox.behavior <*> fourthBox.behavior <*> fifthBox.behavior <*> sixthBox.behavior <*> seventhBox.behavior <*> eightBox.behavior <*> ninthBox.behavior
  let events = ( firstBox.event <|> secondBox.event <|> thirdBox.event <|> fourthBox.event <|> fifthBox.event <|> sixthBox.event <|> seventhBox.event <|> eightBox.event <|> ninthBox.event)

  U.patch widget behavior events