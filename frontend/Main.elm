module Main exposing (..)

import Html exposing (Html, h1, text)


-- MAIN


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { status : String }


init : ( Model, Cmd Msg )
init =
    { status = "Hello world" } ! []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view model =
    h1 [] [ text "hello" ]



-- SUBSCRIPTIONS


subscriptions _ =
    Sub.none
