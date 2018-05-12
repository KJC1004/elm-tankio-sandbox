module Main exposing (..)

import Model exposing (Model, init)
import Messages exposing (..)
import View exposing (view)
import Update exposing (update)

import Html
import Window
import Keyboard
import AnimationFrame
import Mouse

main : Program Never Model Msg
main =
  Html.program { init = init, view = view, update = update, subscriptions = subs }


subs : Model -> Sub Msg
subs model =
  Sub.batch
  [ if model.paused then
      Sub.none
    else
      AnimationFrame.diffs GameUpdate
  , Window.resizes WindowResize
  , Keyboard.downs KeyDown
  , Keyboard.ups KeyUp
  , Mouse.moves MouseMove
  , Mouse.downs MouseDown
  , Mouse.ups MouseUp
  ]
