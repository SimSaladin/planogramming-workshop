module Api exposing (..)

import Json.Encode

import Json.Decode exposing (..)

import Json.Decode.Pipeline exposing (..)

import Exts.Json.Encode

type Request
    = Identify String
    | GetProducts
    | GetReports
    | GetStore String
    | SubmitPlanogram Planogram
    | InternalRefresh

encodeRequest : Request -> Json.Encode.Value
encodeRequest x =
    case x of
        Identify y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Identify" )
                , ( "contents", Json.Encode.string y0 )
                ]

        GetProducts ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "GetProducts" )
                , ( "contents", Json.Encode.list [] )
                ]

        GetReports ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "GetReports" )
                , ( "contents", Json.Encode.list [] )
                ]

        GetStore y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "GetStore" )
                , ( "contents", Json.Encode.string y0 )
                ]

        SubmitPlanogram y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SubmitPlanogram" )
                , ( "contents", encodePlanogram y0 )
                ]

        InternalRefresh ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "InternalRefresh" )
                , ( "contents", Json.Encode.list [] )
                ]

type Response
    = AllProducts (List (Product))
    | StoreShelfSpace String (List (Int))
    | Reports (List (Report))
    | ErrorMsg String

decodeResponse : Decoder Response
decodeResponse =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "AllProducts" ->
                        decode AllProducts
                            |> required "contents" (list decodeProduct)

                    "StoreShelfSpace" ->
                        decode StoreShelfSpace
                            |> required "contents" (index 0 string)
                            |> required "contents" (index 1 (list int))

                    "Reports" ->
                        decode Reports
                            |> required "contents" (list decodeReport)

                    "ErrorMsg" ->
                        decode ErrorMsg
                            |> required "contents" string

                    _ ->
                        fail "Constructor not matched"
            )

type alias Product =
    { ident : Int
    , width : Int
    , priceBuy : Float
    , priceSell : Float
    , attributes : List (Int)
    }

decodeProduct : Decoder Product
decodeProduct =
    decode Product
        |> required "ident" int
        |> required "width" int
        |> required "priceBuy" float
        |> required "priceSell" float
        |> required "attributes" (list int)

encodeProduct : Product -> Json.Encode.Value
encodeProduct x =
    Json.Encode.object
        [ ( "ident", Json.Encode.int x.ident )
        , ( "width", Json.Encode.int x.width )
        , ( "priceBuy", Json.Encode.float x.priceBuy )
        , ( "priceSell", Json.Encode.float x.priceSell )
        , ( "attributes", (Json.Encode.list << List.map Json.Encode.int) x.attributes )
        ]

type alias Report =
    { planogramUser : String
    , planogram : Planogram
    , profit : Float
    , productsSold : List ((Int, Int))
    , creationTime : Int
    }

decodeReport : Decoder Report
decodeReport =
    decode Report
        |> required "planogramUser" string
        |> required "planogram" decodePlanogram
        |> required "profit" float
        |> required "productsSold" (list (map2 (,) (index 0 int) (index 1 int)))
        |> required "creationTime" int

encodeReport : Report -> Json.Encode.Value
encodeReport x =
    Json.Encode.object
        [ ( "planogramUser", Json.Encode.string x.planogramUser )
        , ( "planogram", encodePlanogram x.planogram )
        , ( "profit", Json.Encode.float x.profit )
        , ( "productsSold", (Json.Encode.list << List.map (Exts.Json.Encode.tuple2 Json.Encode.int Json.Encode.int)) x.productsSold )
        , ( "creationTime", Json.Encode.int x.creationTime )
        ]

type alias Planogram =
    { planogramStore : String
    , productShelfLocations : List (ProductShelfLocation)
    }

decodePlanogram : Decoder Planogram
decodePlanogram =
    decode Planogram
        |> required "planogramStore" string
        |> required "productShelfLocations" (list decodeProductShelfLocation)

encodePlanogram : Planogram -> Json.Encode.Value
encodePlanogram x =
    Json.Encode.object
        [ ( "planogramStore", Json.Encode.string x.planogramStore )
        , ( "productShelfLocations", (Json.Encode.list << List.map encodeProductShelfLocation) x.productShelfLocations )
        ]

type LocationOnShelf
    = Top
    | Middle
    | Bottom

decodeLocationOnShelf : Decoder LocationOnShelf
decodeLocationOnShelf =
    string
        |> andThen
            (\x ->
                case x of
                    "Top" ->
                        decode Top

                    "Middle" ->
                        decode Middle

                    "Bottom" ->
                        decode Bottom

                    _ ->
                        fail "Constructor not matched"
            )

encodeLocationOnShelf : LocationOnShelf -> Json.Encode.Value
encodeLocationOnShelf x =
    case x of
        Top ->
            Json.Encode.string "Top"

        Middle ->
            Json.Encode.string "Middle"

        Bottom ->
            Json.Encode.string "Bottom"

type alias ProductShelfLocation =
    { productId : Int
    , shelfId : Int
    , locationOnShelf : LocationOnShelf
    }

decodeProductShelfLocation : Decoder ProductShelfLocation
decodeProductShelfLocation =
    decode ProductShelfLocation
        |> required "productId" int
        |> required "shelfId" int
        |> required "locationOnShelf" decodeLocationOnShelf

encodeProductShelfLocation : ProductShelfLocation -> Json.Encode.Value
encodeProductShelfLocation x =
    Json.Encode.object
        [ ( "productId", Json.Encode.int x.productId )
        , ( "shelfId", Json.Encode.int x.shelfId )
        , ( "locationOnShelf", encodeLocationOnShelf x.locationOnShelf )
        ]