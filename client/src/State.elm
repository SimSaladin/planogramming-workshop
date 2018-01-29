module State exposing (..)

import Api
import Model exposing (..)
import WebSocket
import Json.Encode exposing (encode)

initialModel : Model
initialModel =
    { upstream = "ws://planogramming-workshop.relexsolutions.com"
    }

initialCmd : Cmd Msg
initialCmd = Cmd.none

sendRequest : Model -> Api.Request -> Cmd Msg
sendRequest model request =
    WebSocket.send model.upstream (encode 0 <| Api.encodeRequest request)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    Receive (Ok response) -> updateResponse response model
    Receive (Err string) -> Debug.log string (model, Cmd.none)

updateResponse : Api.Response -> Model -> (Model, Cmd Msg)
updateResponse response model =
    -- TODO
    (model, Cmd.none)
