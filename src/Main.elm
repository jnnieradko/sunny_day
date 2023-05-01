module Main exposing (..)

import Browser
import Debug exposing (toString, todo)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


init : ( Model, Cmd Msg )
init =
    ( InitState, Cmd.none )

type alias Roof =
    { isVisible : Bool
    , id : Int
    , roofType : RoofType
    , properties : RoofProperties
    }


type RoofType
    = Hip
    | Gable
    | Mansard
    | Flat
    | Shed


type RoofProperties
    = HipRoofProp
        { gableLength : Float
        , gableHeight : Float
        , roofWidth : Float
        , roofLength : Float
        , gutterHeight : Float
        , roofColor : String
        , roofAngle : Float
        }
    | GableRoofProp
        { gableLength : Float
        , gableHeight : Float
        , roofWidth : Float
        , roofLength : Float
        , gutterHeight : Float
        , roofColor : String
        , roofAngle : Float
        }
    | MansardRoofProp
        { splitLineWidth : Float
        , splitLineHeight : Float
        , ridgeLength : Float
        , roofWidth : Float
        , roofLength : Float
        , gutterHeight : Float
        , roofColor : String
        , upperRoofAngle : Float
        , bottomRoofAngle : Float
        , roofAngle : Float
        }
    | FlatRoofProp
        { roofWidth : Float
        , roofLength : Float
        , gutterHeight : Float
        , roofColor : String
        , roofAngle : Float
        }
    | ShedRoofProp
        { roofWidth : Float
        , roofLength : Float
        , gutterHeight : Float
        , roofColor : String
        , roofAngle : Float
        }



---- UPDATE ----


type Model
    = InitState
    | DisplayAllRoofsModal (List RoofType) (List Roof) (List Int)
    | SelectedRoof Roof (List Roof) (List Int)


type Msg
    = AddRoof
    | ChooseRoofType RoofType (List Roof) Int
    | SaveRoof Roof (List Roof) (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRoof ->
            ( DisplayAllRoofsModal
                [ Hip
                , Gable
                , Mansard
                , Flat
                , Shed
                ] [] []
            , Cmd.none
            )

        ChooseRoofType rt listroof id ->
            let
                newRoof =
                    createRoof rt id
            in
            case model of
                DisplayAllRoofsModal lrt lr li ->
                    ( SelectedRoof newRoof lr (li)
                    , Cmd.none
                    )

                _ ->
                    todo ""

        SaveRoof r lr li ->
            ( DisplayAllRoofsModal
                [ Hip
                , Gable
                , Mansard
                , Flat
                , Shed
                ]
                ( r :: lr) (r.id ::li)
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "This is my solar panel planning app" ]
        , case model of
            InitState ->
                div []
                    [ text "hdhdhhd"
                    , button [ onClick AddRoof ] [ text "Add roof" ]
                    ]

            DisplayAllRoofsModal lrt lr lid ->
                div []
                    [ div [] [ text "init - lista licznikow: " ]
                    , div [] [ text (String.fromInt (List.length lid)) ]
                    , div [] [ text "Wybrane dachy: " ]
                    , div [] [ text (String.fromInt (countInitListOfRoofs lr)) ]
                    , div [] [ displayRoofs lr ]
                    , div []
                        [ text "Wybierz i edytuj dach"
                        , div [] (roofTypeButton lrt lr ( List.length  lid))
                        ]
                    ]

            SelectedRoof r lr li ->
                if r.isVisible then
                    div roofpropertiesContainerStyle
                        [ div [] [ text ("Dach typu " ++ toString r.roofType) ]
                        , div roofPropertiesStyle [ text ("Właściwości: " ++ toString r.id ++ toString r.properties) ]
                        , button [ onClick (SaveRoof r lr li) ] [ text "Zatwierdź dach" ]
                        ]

                else
                    div [] [ text "Roof not visible" ]

        {- _ ->
           div [] [ text "xxxxxxxxx" ]
        -}
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



---- Functions ----


countInitListOfRoofs : List Roof -> Int
countInitListOfRoofs lr =
    List.length lr


roofTypeButton : List RoofType -> List Roof -> Int -> List (Html Msg)
roofTypeButton lrt lr id =
    List.map (\rt -> button [ onClick (ChooseRoofType rt lr id) ] [ text (toString rt) ]) lrt


initStateRoofs : List Roof -> List (Html Msg)
initStateRoofs lr =
    List.map (\r -> text (toString r)) lr


createRoof : RoofType -> Int -> Roof
createRoof rt id =
    { isVisible = True
    , id = id + 1
    , roofType = rt
    , properties =
        case rt of
            Hip ->
                HipRoofProp
                    { gableLength = 0
                    , gableHeight = 0
                    , roofWidth = 0
                    , roofLength = 0
                    , gutterHeight = 0
                    , roofColor = ""
                    , roofAngle = 0
                    }

            Gable ->
                GableRoofProp
                    { gableLength = 0
                    , gableHeight = 0
                    , roofWidth = 0
                    , roofLength = 0
                    , gutterHeight = 0
                    , roofColor = ""
                    , roofAngle = 0
                    }

            Mansard ->
                MansardRoofProp
                    { splitLineWidth = 0
                    , splitLineHeight = 0
                    , ridgeLength = 0
                    , roofWidth = 0
                    , roofLength = 0
                    , gutterHeight = 0
                    , roofColor = ""
                    , upperRoofAngle = 0
                    , bottomRoofAngle = 0
                    , roofAngle = 0
                    }

            Flat ->
                FlatRoofProp
                    { roofWidth = 0
                    , roofLength = 0
                    , gutterHeight = 0
                    , roofColor = ""
                    , roofAngle = 0
                    }

            Shed ->
                ShedRoofProp
                    { roofWidth = 0
                    , roofLength = 0
                    , gutterHeight = 0
                    , roofColor = ""
                    , roofAngle = 0
                    }
    }


roofpropertiesContainerStyle : List (Attribute Msg)
roofpropertiesContainerStyle =
    [ style "position" "absolute"
    , style "top" "0"
    , style "bottom" "0"
    , style "right" "0"
    , style "left" "0"
    , style "display" "flex"
    , style "align-items" "center"
    , style "justify-content" "center"
    , style "background-color" "rgba(33, 43, 54, 0.4)"
    ]


roofPropertiesStyle : List (Attribute Msg)
roofPropertiesStyle =
    [ style "border-style" "solid"
    , style "border-radius" "3px"
    , style "border-color" "white"
    , style "background-color" "white"
    , style "height" "120px"
    , style "width" "440px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "align-items" "center"
    , style "justify-content" "center"
    ]


displayRoofs : List Roof -> Html msg
displayRoofs lr =
    div []
        (List.map displayRoof lr)


displayRoof : Roof -> Html msg
displayRoof r =
    let
        roofTypeText =
            case r.roofType of
                Hip ->
                    "Hip"

                Gable ->
                    "Gable"

                Mansard ->
                    "Mansard"

                Flat ->
                    "Flat"

                Shed ->
                    "Shed"
    in
    div []
        [ text ("ID: " ++ String.fromInt r.id)
        , br [] []
        , text ("Roof type: " ++ roofTypeText)
        ]
