module Model exposing (..)


import Api

type alias Model =
    { upstream : String
    }

type Msg
    = Receive (Result String Api.Response)
