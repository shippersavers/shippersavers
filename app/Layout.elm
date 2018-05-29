module Layout exposing (..)

import Color exposing (..)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


type Styles
    = Clean
    | Title
    | Subtitle
    | ButtonSearch
    | SearchForm
    | MainContent
    | TextSearch


stylesheet =
    Style.styleSheet
        [ style Clean []
        , style Title
            [ Font.size 36
            ]
        , style Subtitle
            [ Font.size 18
            , Color.text yellow
            ]
        , style SearchForm
            [ Font.size 24
            ]
        , style TextSearch
            [ Font.size 24
            , Border.rounded 5
            ]
        , style ButtonSearch
            [ Color.background blue
            , Color.text white
            , Border.rounded 5
            ]
        , style MainContent
            [ Font.center ]
        ]
