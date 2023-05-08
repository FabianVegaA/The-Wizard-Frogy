module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import String exposing (toInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg
    = OnAnimationFrame Float
    | OnMouseDown Int Int
    | OnMouseUp


type alias Model =
    { frogy : Frogy
    , flies : List Fly
    }


type alias Fly =
    { x : Int
    , y : Int
    , direction : Float
    , speed : Int
    }


type alias Frogy =
    { x : Int
    , y : Int
    , tongue : Tongue
    }


type alias Tongue =
    { x : Int
    , y : Int
    , speed : Int
    , status : TongueStatus
    }


type TongueStatus
    = Moving Int Int
    | Retracting
    | Retracted


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { frogy = initFrogy
      , flies = []
      }
    , Cmd.none
    )


initFrogy : Frogy
initFrogy =
    { x = 650
    , y = 450
    , tongue =
        { x = 650
        , y = 450
        , speed = 0
        , status = Retracted
        }
    }


initFly : Fly
initFly =
    { x = 0
    , y = 0
    , direction = 0
    , speed = 10
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Svg.Svg Msg
view { frogy } =
    svg
        [ width "800"
        , height "450"
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewFrogy frogy
        ]


viewFrogy : Frogy -> Svg.Svg Msg
viewFrogy frogy =
    let
        tongue =
            frogy.tongue
    in
    g
        [ fill "white"
        , stroke "black"
        , strokeWidth "5"
        ]
        [ circle
            [ cx <| String.fromInt tongue.x
            , cy <| String.fromInt tongue.y
            , r "15"
            , fill "red"
            , stroke "red"
            ]
            []
        , line
            [ x1 <| String.fromInt frogy.x
            , y1 <| String.fromInt frogy.y
            , x2 <| String.fromInt tongue.x
            , y2 <| String.fromInt tongue.y
            , stroke "red"
            , strokeWidth "20"
            ]
            []
        , circle
            [ cx <| String.fromInt frogy.x
            , cy <| String.fromInt frogy.y
            , r "80"
            , fill "green"
            ]
            []
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMouseDown x y ->
            ( { model | frogy = updateTongueStatus model.frogy (Moving x y) }, Cmd.none )

        OnMouseUp ->
            ( { model | frogy = updateTongueStatus model.frogy Retracting }, Cmd.none )

        OnAnimationFrame timeDelta ->
            ( { model | frogy = updateFrogy model.frogy }, Cmd.none )


updateTongueStatus : Frogy -> TongueStatus -> Frogy
updateTongueStatus frogy status =
    let
        tongue =
            frogy.tongue
    in
    { frogy | tongue = { tongue | status = status } }


updateFrogy : Frogy -> Frogy
updateFrogy frogy =
    let
        tongue =
            frogy.tongue

        updateTongue =
            case frogy.tongue.status of
                Moving x y ->
                    { tongue
                        | x = x
                        , y = y
                    }

                Retracting ->
                    { tongue
                        | x = frogy.x
                        , y = frogy.y
                    }

                _ ->
                    tongue
    in
    { frogy | tongue = updateTongue }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
        , Browser.Events.onMouseDown (Decode.map (\( x, y ) -> OnMouseDown x y) keyDecoder)
        , Browser.Events.onMouseUp (Decode.succeed OnMouseUp)
        ]


keyDecoder : Decode.Decoder ( Int, Int )
keyDecoder =
    Decode.map2
        (\x y -> ( x, y ))
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)
