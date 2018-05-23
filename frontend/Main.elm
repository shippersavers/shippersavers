module Main exposing (..)

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Html exposing (Html)
import Layout exposing (..)


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
    viewport stylesheet
        (column Clean
            []
            [ mainContent MainContent
                [ maxWidth (px 1280) ]
                (column Clean
                    [ center, width (percent 100) ]
                    [ h1 Title [ paddingXY 0 30 ] (text "Shipper Savers ")
                    , h3 Subtitle [ paddingXY 0 20 ] (text "We compare sea freight from shipping lines and help save money")
                    , searchFormView
                    ]
                )
            ]
        )


searchFormView : Element Styles variation Msg
searchFormView =
    row SearchForm
        [ paddingXY 50 20
        , spacing 30
        , verticalCenter
        , spread
        ]
        [ Input.search TextSearch
            [ padding 15 ]
            { onChange = \_ -> NoOp
            , value = "test"
            , label = Input.hiddenLabel ""
            , options = []
            }
        , Input.search TextSearch
            [ padding 15 ]
            { onChange = \_ -> NoOp
            , value = "test"
            , label = Input.hiddenLabel ""
            , options = []
            }
        , button ButtonSearch [ padding 15 ] (text "Search")
        ]



-- SUBSCRIPTIONS


subscriptions _ =
    Sub.none
