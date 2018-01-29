module View exposing (..)

import Api exposing (..)
import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

view : Model -> Html Msg
view model =
    text "Hello world!"
