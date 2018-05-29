module Main exposing (..)

import AccessibleExample
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
    { accessibleAutocomplete : AccessibleExample.Model
    , status : String
    , currentFocus : Focused
    }


init : ( Model, Cmd Msg )
init =
    { accessibleAutocomplete = AccessibleExample.init
    , status = "Hello world"
    , currentFocus = Simple
    }
        ! []



-- UPDATE


type Msg
    = NoOp
    | AccessibleExample AccessibleExample.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                AccessibleExample autoMsg ->
                    let
                        toggleFocus autoMsg model =
                            case autoMsg of
                                AccessibleExample.OnFocus ->
                                    { model | currentFocus = Simple }

                                _ ->
                                    model
                    in
                    { model | accessibleAutocomplete = Tuple.first <| AccessibleExample.update autoMsg model.accessibleAutocomplete }
                        |> toggleFocus autoMsg

                NoOp ->
                    model
    in
    ( newModel, Cmd.none )



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
                    , html (viewSimpleExample model.accessibleAutocomplete)
                    ]
                )
            ]
        )


viewSimpleExample : AccessibleExample.Model -> Html Msg
viewSimpleExample autocomplete =
    Html.map AccessibleExample (AccessibleExample.view autocomplete)


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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.currentFocus of
        Simple ->
            Sub.map AccessibleExample (AccessibleExample.subscriptions model.accessibleAutocomplete)

        None ->
            Sub.none


type Focused
    = Simple
    | None
