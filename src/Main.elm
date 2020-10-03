module Main exposing (main)

import Browser
import Debug
import Html exposing (Html, button, div, img, input, label, p, span, text)
import Html.Attributes exposing (class, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra exposing (onClickStopPropagation)
import Http
import Json.Decode as J
import Json.Decode.Pipeline as P
import Json.Encode as JE
import List exposing (map)
import Maybe exposing (Maybe)
import String.Extra exposing (stripTags)



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



-- DEAL


type alias Deal =
    { id : Int
    , accepted : Maybe String
    , sellerId : Int
    , buyerId : Int
    , dealgoods : List Dealgood
    , state : DealState
    , details : String
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
        |> P.required "details" J.string


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


dealstateColor : DealState -> String
dealstateColor state =
    case state of
        Draft ->
            "#83d32e"

        Offered ->
            "#f1c917"

        Actual ->
            "#0789d4"

        Outdated ->
            "red"

        Fulfilled ->
            "#bbb"

        _ ->
            "red"



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
    , csrftoken : String
    }


initDataDecoder : J.Decoder InitData
initDataDecoder =
    J.succeed InitData
        |> P.required "users" (J.list userDecoder)
        |> P.required "loggedInUser" (J.maybe J.int)
        |> P.optional "deals" (J.list dealDecoder) []
        |> P.required "goods" (J.list goodDecoder)
        |> P.required "csrftoken" J.string



-- MODEL


type alias Model =
    { tmp : String
    , tab : Tab
    , users : List User
    , loggedUser : Maybe User
    , deals : List Deal
    , showDeal : Maybe Deal
    , goods : List Good
    , authData : { username : String, password : String }
    , csrftoken : String
    }


type Tab
    = UsersTab
    | DealsTab


init : J.Value -> ( Model, Cmd Msg )
init value =
    ( case J.decodeValue initDataDecoder value of
        Ok res ->
            { tmp = "tmp"
            , tab = DealsTab
            , users = res.users
            , loggedUser = res.loggedInUser |> Maybe.andThen (\uid -> find (\u -> u.id == uid) res.users)
            , deals = res.deals
            , showDeal = Nothing
            , goods = res.goods
            , authData = { username = "", password = "" }
            , csrftoken = res.csrftoken
            }

        Err _ ->
            { tmp = "tmp"
            , tab = DealsTab
            , users = []
            , loggedUser = Nothing
            , deals = []
            , showDeal = Nothing
            , goods = []
            , authData = { username = "", password = "" }
            , csrftoken = ""
            }
    , Cmd.none
    )


type LoginResponseData
    = LoginFail String
    | LoginDone LoggedUserData


type LogoutResponseData
    = LogoutFail
    | LogoutDone



---- UPDATE ----


type Msg
    = NoOp
    | SetTab Tab
    | UpdateAuthUsername String
    | UpdateAuthPassword String
    | Login
    | LoginResponse (Result Http.Error LoginResponseData)
    | Logout
    | LogoutResponse (Result Http.Error LogoutResponseData)
    | ShowDeal Deal
    | HideDeal


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

        Login ->
            ( model, login model.csrftoken model.authData )

        LoginResponse res ->
            case res of
                Ok (LoginDone loggedUserData) ->
                    ( { model
                        | loggedUser = find (\u -> u.id == loggedUserData.user.id) model.users
                        , deals = loggedUserData.deals
                      }
                    , Cmd.none
                    )

                _ ->
                    let
                        _ =
                            Debug.log "Login failed with res:" res
                    in
                    ( model, Cmd.none )

        Logout ->
            ( model, logout model.csrftoken )

        LogoutResponse res ->
            case res of
                Ok _ ->
                    ( { model
                        | deals = []
                        , loggedUser = Nothing
                      }
                    , Cmd.none
                    )

                -- Ok LogoutFail ->
                Err m ->
                    let
                        _ =
                            Debug.log "err msg" m
                    in
                    ( model, Cmd.none )

        ShowDeal deal ->
            ( { model
                | showDeal = Just deal
              }
            , Cmd.none
            )

        HideDeal ->
            ( { model | showDeal = Nothing }, Cmd.none )


post : String -> String -> Http.Body -> Http.Expect Msg -> Cmd Msg
post url token body expect =
    Http.request
        { url = url
        , method = "POST"
        , headers = [ Http.header "X-CSRFToken" token ]
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


logout : String -> Cmd Msg
logout token =
    let
        logoutEncoder =
            JE.object
                [ ( "logout", JE.bool True )
                , ( "reqId", JE.int 1 )
                ]

        logoutResponseDecoder =
            J.field "logout" J.string
                |> J.andThen
                    (\res ->
                        J.succeed <|
                            case res of
                                "OK" ->
                                    LogoutDone

                                "FAIL" ->
                                    LogoutDone

                                _ ->
                                    LogoutFail
                    )
    in
    post
        "/api/logout"
        token
        (Http.jsonBody logoutEncoder)
        (Http.expectJson LogoutResponse logoutResponseDecoder)


login : String -> { username : String, password : String } -> Cmd Msg
login token authData =
    let
        loginEncoder : { username : String, password : String } -> JE.Value
        loginEncoder { username, password } =
            JE.object
                [ ( "login", JE.bool True )
                , ( "reqId", JE.int 0 )
                , ( "id", JE.string username )
                , ( "pass", JE.string password )
                ]

        loginResponseDecoder : J.Decoder LoginResponseData
        loginResponseDecoder =
            J.field "login" J.string
                |> J.andThen
                    (\status ->
                        case status of
                            "OK" ->
                                J.map LoginDone loggedUserDataDecoder

                            "FAIL" ->
                                J.succeed <| LoginFail "login failed"

                            _ ->
                                J.succeed <| LoginFail "some other error"
                    )
    in
    Http.request
        { url = "/api/login"
        , method = "POST"
        , headers = [ Http.header "X-CSRFToken" token ]
        , body = Http.jsonBody <| loginEncoder authData
        , expect = Http.expectJson LoginResponse loginResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


type alias LoggedUser =
    { id : Int }


type alias LoggedUserData =
    { user : { id : Int }
    , deals : List Deal
    }


loggedUserDataDecoder : J.Decoder LoggedUserData
loggedUserDataDecoder =
    J.succeed LoggedUserData
        |> P.required "user" (J.field "id" J.int |> J.andThen (\id -> J.succeed { id = id }))
        |> P.required "deals" (J.list dealDecoder)



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

                                viewDeal_ =
                                    viewDeal model.goods loggedUser
                            in
                            div [ class "users-list" ]
                                (map
                                    (\deal ->
                                        case findCounterpart deal of
                                            Nothing ->
                                                div [ class "user" ] [ text "User Not Found" ]

                                            Just user ->
                                                viewDeal_ user deal
                                    )
                                    model.deals
                                )
    in
    div [ class "root" ]
        [ viewLoggedUser model.loggedUser
        , div [ class "buttons-bar" ]
            [ div [ class "tabs-row" ]
                [ button
                    [ onClick (SetTab UsersTab), class "tabs-row__tab" ]
                    [ text <| "Users (" ++ String.fromInt (List.length model.users) ++ ")" ]
                , button
                    [ onClick (SetTab DealsTab), class "tabs-row__tab" ]
                    [ text <| "Deals (" ++ String.fromInt (List.length model.deals) ++ ")" ]
                ]
            ]
        , pageView
        , case model.showDeal of
            Nothing ->
                text ""

            Just deal ->
                viewDealFull model deal
        ]


viewDealFull : Model -> Deal -> Html Msg
viewDealFull model deal =
    div [ class "modal", onClick HideDeal ]
        [ div [ class "modal__content", onClickStopPropagation NoOp ]
            [ div [ class "card" ]
                [ div [ class "card__title" ]
                    [ viewDealStatusIndicator deal
                    , viewDealTitle deal
                    , div [ class "card__x", onClick HideDeal ]
                        [ text "close"
                        ]
                    ]
                , div [ class "card__content" ]
                    [ case model.loggedUser of
                        Nothing ->
                            text "No Logged In User"

                        Just loggedUser ->
                            case dealCounterpart loggedUser model.users deal of
                                Nothing ->
                                    text "No Other User"

                                Just otherUser ->
                                    viewUser otherUser
                                        [ div [ class "grow" ] []
                                        , viewDealTotal loggedUser deal
                                        ]
                    , p [] [ text deal.details ]
                    , div [ class "mb10" ]
                        (div [ class "dealgood-row dealgood-row--title" ]
                            [ span [ class "dealgood-row__name" ] [ text "name" ]
                            , span [ class "dealgood-row__price" ] [ text "price" ]
                            , span [ class "dealgood-row__q" ] [ text "q" ]
                            , span [ class "dealgood-row__sum" ] [ text "sum" ]
                            ]
                            :: (let
                                    viewDealgood_ =
                                        viewDealgood model.goods
                                in
                                map
                                    viewDealgood_
                                    deal.dealgoods
                               )
                        )
                    , div [ class "buttons-row" ]
                        [ button [ class "btn btn--blue", onClick HideDeal ] [ text "close" ]
                        , button [ class "btn btn--blue" ] [ text "done" ]
                        ]
                    ]
                ]
            ]
        ]


viewDealgood : List Good -> Dealgood -> Html Msg
viewDealgood goods dealgood =
    let
        goodname =
            case
                goods |> find (\g -> g.id == dealgood.goodId)
            of
                Nothing ->
                    "<not found>"

                Just good ->
                    good.name
    in
    div [ class "dealgood-row" ]
        [ span [ class "dealgood-row__name" ] [ text goodname ]
        , span [ class "dealgood-row__price" ] [ text <| String.fromFloat dealgood.price ]
        , span [ class "dealgood-row__q" ] [ text <| String.fromFloat dealgood.q ]
        , span [ class "dealgood-row__sum" ] [ text <| String.fromFloat <| dealgood.q * dealgood.price ]
        ]


viewLogin : { username : String, password : String } -> Html Msg
viewLogin authData =
    div [ class "login" ]
        [ div [ class "mb10" ]
            [ div []
                [ div [ class "input-group" ]
                    [ label [ class "input-group__label" ] [ text "Username" ]
                    , input
                        [ class "input-group__input"
                        , value authData.username
                        , onInput UpdateAuthUsername
                        ]
                        []
                    ]
                ]
            , div []
                [ div [ class "input-group" ]
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
            ]
        , div [] [ button [ class "btn btn--blue", onClick Login ] [ text "Войти" ] ]
        ]


viewDeal : List Good -> User -> User -> Deal -> Html Msg
viewDeal goods loggedUser user deal =
    div [ class "user flex-center flex-center--nowrap", onClick <| ShowDeal deal ]
        [ div [ class "nowrap" ]
            [ viewUserImg user
            , div [ class "deal-info" ]
                [ p [ class "deal-info__p" ]
                    [ viewDealStatusIndicator deal
                    , viewDealTitle deal
                    ]
                , p [ class "deal-info__p user__name" ]
                    [ text <| userFullname user ]
                ]
            ]
        , div [ class "grow deal__short" ] [ text <| dealShort goods deal ]
        , viewDealTotal loggedUser deal
        ]


viewDealTotal : User -> Deal -> Html Msg
viewDealTotal loggedUser deal =
    let
        ( cls, txt ) =
            iif (loggedIsSeller loggedUser deal)
                ( " deal__total--income", "+" )
                ( "", "" )
    in
    div [ class <| "deal__total" ++ cls ]
        [ text <| txt ++ (String.fromFloat <| dealTotal deal) ]


viewDealStatusIndicator : Deal -> Html Msg
viewDealStatusIndicator deal =
    div
        [ class "deal-info__state-color"
        , style "background-color" <| dealstateColor deal.state
        ]
        []


viewDealTitle : Deal -> Html Msg
viewDealTitle deal =
    text <| String.trim <| "№ " ++ String.fromInt deal.id ++ " " ++ (Maybe.withDefault "" deal.accepted |> formatDate)


viewUsersList : List User -> Html Msg
viewUsersList users =
    div [ class "users-list" ] (map (\u -> viewUser u []) users)


viewUser : User -> List (Html Msg) -> Html Msg
viewUser user content =
    div [ class "user" ]
        ([ viewUserImg user
         , span
            [ class "user__name" ]
            [ text (userFullname user) ]
         ]
            ++ content
        )


viewUserImg : User -> Html Msg
viewUserImg user =
    case user.img of
        Nothing ->
            div [ class "user__img" ] []

        Just url ->
            img [ class "user__img", src url ] []


viewLoggedUser : Maybe User -> Html Msg
viewLoggedUser loggedUser =
    div [ class "users-list" ]
        (case loggedUser of
            Nothing ->
                [ button [] [ text "Войти" ] ]

            Just user ->
                [ viewUser user
                    [ div [ class "grow" ] []
                    , button [ onClick Logout ] [ text "Выйти" ]
                    ]
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
    user.lastName ++ " " ++ user.firstName |> String.trim


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


dealShort : List Good -> Deal -> String
dealShort goods deal =
    String.trim <|
        stripTags deal.details
            ++ " "
            ++ (String.join ", " <|
                    map
                        (\dg ->
                            let
                                goodname =
                                    find (\g -> g.id == dg.goodId) goods
                                        |> Maybe.map .name
                                        |> Maybe.withDefault "<not found>"
                            in
                            goodname ++ " (" ++ String.fromFloat dg.q ++ ")"
                        )
                        deal.dealgoods
               )
