module Main exposing (..)

import Browser
import Debug
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Json.Decode as J
import Json.Decode.Pipeline as P
import Maybe exposing (Maybe)



---- MODEL ----


type alias LoggedInUserId =
    Int


userDecoder : J.Decoder User
userDecoder =
    J.succeed User
        |> P.required "id" J.int
        |> P.optional "city" J.string ""
        |> P.required "firstName" J.string
        |> P.required "lastName" J.string
        |> P.optional "img" (J.maybe J.string) Nothing
        |> P.optional "slug" (J.maybe J.string) Nothing
        |> P.required "userid" J.string
        |> P.optional "hasOutdated" J.bool False
        |> P.optional "bids" (J.list J.int) []
        |> P.optional "buys" (J.list J.int) []
        |> P.optional "contacts" (J.list J.string) []
        |> P.optional "offers" (J.list J.int) []
        |> P.optional "sells" (J.list J.int) []


usersDecoder : J.Decoder (List User)
usersDecoder =
    J.list userDecoder


type alias User =
    { id : Int
    , city : String
    , firstName : String
    , lastName : String
    , img : Maybe String
    , slug : Maybe String
    , userid : String
    , hasOutdated : Bool
    , bids : List Int
    , buys : List Int
    , contacts : List String
    , offers : List Int
    , sells : List Int
    }


type alias Flags =
    J.Value


type alias Model =
    { tmp : String
    , users : List User
    }


init : J.Value -> ( Model, Cmd Msg )
init value =
    let
        _ =
            Debug.log "users" (J.decodeValue usersDecoder value)
    in
    ( { tmp = "tmp", users = [] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text model.tmp ]
        ]



---- PROGRAM ----


main : Program J.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
