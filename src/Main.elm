module Main exposing (main)

import Browser
import Browser.Events
import Json.Decode as Decode
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)


type Msg
    = OnAnimationFrame Float
    | OnMouseDown Int Int
    | OnMouseUp
    | SpawnFlies (List Fly)


type alias Model =
    { frogy : Frogy
    , flies : List Fly
    , dim : ( Int, Int )
    , seed : Random.Seed
    }


type alias Fly =
    { x : Int
    , y : Int
    , vertSpeed : Int
    , horizSpeed : Int
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
      , flies = [ Fly 100 100 1 1 ] -- TODO: randomize
      , dim = ( 800, 450 )
      , seed = Random.initialSeed 42
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
        , speed = 5
        , status = Retracted
        }
    }


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Svg.Svg Msg
view { frogy, flies, dim } =
    let
        ( x, y ) =
            dim
    in
    svg
        [ width <| String.fromInt x
        , height <| String.fromInt y
        , viewBox "0 0 500 500"
        , Svg.Attributes.style "background: #efefef"
        ]
        [ viewFlies flies
        , viewFrogy frogy
        ]


viewFlies : List Fly -> Svg.Svg Msg
viewFlies flies =
    g [] (List.map viewFly flies)


viewFly : Fly -> Svg.Svg Msg
viewFly { x, y } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "5"
        , fill "black"
        ]
        []


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
            let
                flies =
                    model.flies
                        |> List.filter (isOutside model.dim)

                ( updatedFlies, newSeed ) =
                    updateRandomFlies model.seed model.flies
            in
            ( { model
                | frogy = updateFrogy model.frogy
                , flies = updatedFlies
                , seed = newSeed
              }
            , Cmd.none
            )

        SpawnFlies flies ->
            ( { model | flies = flies }, Cmd.none )


isOutside : ( Int, Int ) -> Fly -> Bool
isOutside ( sizeX, sizeY ) { x, y } =
    x < 0 || x > sizeX || y < 0 || y > sizeY


updateRandomFly : ( Int, Int ) -> Fly -> Fly
updateRandomFly ( newXSpeed, newYSpeed ) fly =
    let
        { x, y, vertSpeed, horizSpeed } =
            fly
    in
    { fly
        | x = x + horizSpeed
        , y = y + vertSpeed
        , vertSpeed = newYSpeed
        , horizSpeed = newXSpeed
    }


updateRandomFlies : Random.Seed -> List Fly -> ( List Fly, Random.Seed )
updateRandomFlies seed flies =
    case flies of
        [] ->
            ( [], seed )

        fly :: rest ->
            let
                ( newXSpeed, s1 ) =
                    randomSpeed seed

                ( newYSpeed, s2 ) =
                    randomSpeed s1

                fliesGenerator =
                    updateRandomFly ( newXSpeed, newYSpeed )

                ( updatedFlies, lastSeed ) =
                    updateRandomFlies s2 rest
            in
            ( fliesGenerator fly :: updatedFlies, lastSeed )


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
        (\x y -> ( x - 140, y + 35 ))
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)



-- fliesGenerator : Int -> ( Int, Int ) -> Random.Generator (List Fly)
-- fliesGenerator n dim =
--     flyGenerator dim
--         |> Random.list n


flyGenerator : ( Int, Int ) -> Random.Generator Fly
flyGenerator ( sizeX, sizeY ) =
    Random.map3
        (Fly 0)
        (Random.int 0 sizeY)
        (Random.int -10 10)
        (Random.int -10 10)


randomSpeed : Random.Seed -> ( Int, Random.Seed )
randomSpeed seed =
    Random.step (Random.int -10 10) seed
