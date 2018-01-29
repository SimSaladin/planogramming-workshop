module Main exposing (..)

import Api exposing (..)
import Model exposing (..)
import State exposing (..)
import View exposing (..)
import Html
import WebSocket
import Json.Decode exposing (decodeString)

main = Html.program
    { init = (initialModel, initialCmd)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.upstream (decodeString decodeResponse >> Receive)
