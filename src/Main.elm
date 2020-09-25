module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, h1, img, input, label, p, span, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as J
import Json.Decode.Pipeline as P
import List exposing (map)
import Maybe exposing (Maybe)



---- MODEL ----
-- USER


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



-- DEAL


type alias Deal =
    { id : Int
    , accepted : Maybe String
    , sellerId : Int
    , buyerId : Int
    , dealgoods : List Dealgood
    , state : DealState
    }


dealDecoder : J.Decoder Deal
dealDecoder =
    J.succeed Deal
        |> P.required "id" J.int
        |> P.optional "accepted" (J.maybe J.string) Nothing
        |> P.required "buyerId" J.int
        |> P.required "sellerId" J.int
        |> P.required "dealgoods" (J.list dealgoodDecoder)
        |> P.required "state" dealstateDecoder


dealsDecoder : J.Decoder (List Deal)
dealsDecoder =
    J.list dealDecoder


type alias Dealgood =
    { id : Int
    , goodId : Int
    , price : Float
    , q : Float
    }


dealgoodDecoder : J.Decoder Dealgood
dealgoodDecoder =
    J.succeed Dealgood
        |> P.required "id" J.int
        |> P.required "goodId" J.int
        |> P.required "price" J.float
        |> P.required "q" J.float


type DealState
    = Draft
    | Offered
    | Actual
    | Outdated
    | Fulfilled
    | OtherState String


dealstateDecoder : J.Decoder DealState
dealstateDecoder =
    J.string
        |> J.andThen
            (\s ->
                case s of
                    "draft" ->
                        J.succeed Draft

                    "offered" ->
                        J.succeed Offered

                    "actual" ->
                        J.succeed Actual

                    "outdated" ->
                        J.succeed Outdated

                    "fulfilled" ->
                        J.succeed Fulfilled

                    _ ->
                        J.succeed <| OtherState s
            )


ruDealState state =
    case state of
        Draft ->
            "черновик"

        Offered ->
            "предложение"

        Actual ->
            "действующий"

        Outdated ->
            "просроченный"

        Fulfilled ->
            "закрытый"

        OtherState s ->
            s



-- GOOD


type alias Good =
    { id : Int, name : String }


goodDecoder : J.Decoder Good
goodDecoder =
    J.succeed Good
        |> P.required "id" J.int
        |> P.required "name" J.string



-- INIT DATA


type alias InitData =
    { users : List User
    , loggedInUser : Maybe Int
    , deals : List Deal
    , goods : List Good
    }


initDataDecoder : J.Decoder InitData
initDataDecoder =
    J.succeed InitData
        |> P.required "users" usersDecoder
        |> P.required "loggedInUser" (J.maybe J.int)
        |> P.optional "deals" dealsDecoder []
        |> P.required "goods" (J.list goodDecoder)



-- MODEL


type alias Model =
    { tmp : String
    , tab : Tab
    , users : List User
    , loggedUser : Maybe User
    , deals : List Deal
    , goods : List Good
    , authData : { username : String, password : String }
    }


type Tab
    = UsersTab
    | DealsTab


init : J.Value -> ( Model, Cmd Msg )
init value =
    let
        { users, loggedUser, deals, goods } =
            case J.decodeValue initDataDecoder value of
                Ok res ->
                    { users = res.users
                    , loggedUser = Maybe.andThen (\uid -> find (\u -> u.id == uid) res.users) res.loggedInUser
                    , deals = res.deals
                    , goods = res.goods
                    }

                Err _ ->
                    { users = []
                    , loggedUser = Nothing
                    , deals = []
                    , goods = []
                    }
    in
    ( { tmp = "tmp"
      , tab = DealsTab
      , users = users
      , loggedUser = loggedUser
      , deals = deals
      , goods = goods
      , authData = { username = "", password = "" }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | SetTab Tab
    | UpdateAuthUsername String
    | UpdateAuthPassword String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetTab tab ->
            ( { model | tab = tab }, Cmd.none )

        UpdateAuthUsername name ->
            let
                authData =
                    model.authData
            in
            ( { model | authData = { authData | username = name } }, Cmd.none )

        UpdateAuthPassword pass ->
            let
                authData =
                    model.authData
            in
            ( { model | authData = { authData | password = pass } }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        pageView =
            case model.tab of
                UsersTab ->
                    viewUsersList model.users

                DealsTab ->
                    case model.loggedUser of
                        Nothing ->
                            viewLogin model.authData

                        Just loggedUser ->
                            let
                                findCounterpart =
                                    dealCounterpart loggedUser model.users
                            in
                            div [ class "users-list" ]
                                (map
                                    (\deal ->
                                        case findCounterpart deal of
                                            Nothing ->
                                                div [ class "user" ] [ text "User Not Found" ]

                                            Just user ->
                                                viewDeal loggedUser user deal
                                    )
                                    model.deals
                                )
    in
    div [ class "root" ]
        [ loggedUserView model.loggedUser
        , div [ class "buttons-bar" ]
            [ button
                [ onClick (SetTab UsersTab), class "btn" ]
                [ text <| "Users (" ++ String.fromInt (List.length model.users) ++ ")" ]
            , button
                [ onClick (SetTab DealsTab), class "btn" ]
                [ text <| "Deals (" ++ String.fromInt (List.length model.deals) ++ ")" ]
            ]
        , pageView
        ]


viewLogin : { username : String, password : String } -> Html Msg
viewLogin authData =
    div [ class "login" ]
        [ div [ class "mb10" ]
            [ div [ class "input-group" ]
                [ label [ class "input-group__label" ] [ text "Username" ]
                , input
                    [ class "input-group__input"
                    , value authData.username
                    , onInput UpdateAuthUsername
                    ]
                    []
                ]
            , div [ class "input-group" ]
                [ label [ class "input-group__label" ] [ text "Password" ]
                , input
                    [ class "input-group__input"
                    , type_ "password"
                    , value authData.password
                    , onInput UpdateAuthPassword
                    ]
                    []
                ]
            ]
        , div [] [ button [ class "btn btn--blue" ] [ text "Войти" ] ]
        ]


viewDeal : User -> User -> Deal -> Html Msg
viewDeal loggedUser user deal =
    div [ class "user flex-center" ]
        [ div []
            [ viewUserImg user
            , div [ class "deal-info" ]
                [ p [ class "deal-info__p" ]
                    [ text <| "№ " ++ String.fromInt deal.id ++ " " ++ (Maybe.withDefault "" deal.accepted |> formatDate)
                    ]
                , p [ class "deal-info__p user__name" ]
                    [ text <| userFullname user ]
                ]
            ]
        , div [ class "grow deal__total" ]
            [ text <|
                iif (loggedIsSeller loggedUser deal) "+" "-"
                    ++ (String.fromFloat <| dealTotal deal)
            ]
        ]


viewUsersList : List User -> Html Msg
viewUsersList users =
    div [ class "users-list" ] (map viewUser users)


viewUser : User -> Html Msg
viewUser user =
    div [ class "user" ]
        [ viewUserImg user
        , span
            [ class "user__name" ]
            [ text (userFullname user) ]
        ]


viewUserImg : User -> Html Msg
viewUserImg user =
    case user.img of
        Nothing ->
            div [ class "user__img" ] []

        Just url ->
            img [ class "user__img", src url ] []


loggedUserView : Maybe User -> Html Msg
loggedUserView loggedUser =
    div [ class "users-list" ]
        (case loggedUser of
            Nothing ->
                [ button [] [ text "Войти" ] ]

            Just user ->
                [ viewUser user
                , button [] [ text "Выйти" ]
                ]
        )



---- PROGRAM ----


main : Program J.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- HELP FUNCS ----


find : (a -> Bool) -> List a -> Maybe a
find pred list =
    List.head <| List.filter pred list


dealCounterpart : User -> List User -> Deal -> Maybe User
dealCounterpart loggedUser users deal =
    if deal.sellerId == loggedUser.id then
        find (\u -> u.id == deal.buyerId) users

    else if deal.buyerId == loggedUser.id then
        find (\u -> u.id == deal.sellerId) users

    else
        Nothing


userFullname : User -> String
userFullname user =
    user.lastName ++ " " ++ user.firstName


formatDate : String -> String
formatDate date =
    String.split "-" date
        |> List.drop 1
        |> List.reverse
        |> String.join "."


dealTotal : Deal -> Float
dealTotal deal =
    List.sum <| map (\dg -> dg.q * dg.price) deal.dealgoods


loggedIsSeller : User -> Deal -> Bool
loggedIsSeller user deal =
    user.id == deal.sellerId


iif : Bool -> a -> a -> a
iif bool a b =
    if bool then
        a

    else
        b
