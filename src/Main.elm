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
    | SpawnFlies Int (List Fly)


type alias Model =
    { frogy : Frogy
    , flies : List Fly
    , dim : ( Int, Int )
    , maxSpeed : Int
    , maxPopulation : Int
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
      , flies = initFlies
      , dim = ( 800, 450 )
      , seed = Random.initialSeed 42
      , maxSpeed = 7
      , maxPopulation = 10
      }
    , Cmd.none
    )


initFrogy : Frogy
initFrogy =
    { x = 750
    , y = 400
    , tongue =
        { x = 750
        , y = 650
        , speed = 5
        , status = Retracted
        }
    }


initFlies : List Fly
initFlies =
    [ Fly 0 100 10 1
    , Fly 0 200 10 2
    , Fly 0 300 13 -5
    ]


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
        , viewBox ("0 0 " ++ String.fromInt x ++ " " ++ String.fromInt y)
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
                ( updatedFlies, s1 ) =
                    model.flies
                        |> List.filter (isOutside 100 model.dim)
                        |> instanceNewFlies model
                        |> (\( flies, seed ) -> updateFlies { model | seed = seed } flies)

                ( hasSpawn, s2 ) =
                    Random.step (Random.weighted ( 10, True ) [ ( 90, False ) ]) s1
            in
            ( { model
                | frogy = updateFrogy model.frogy
                , flies = updatedFlies
                , seed = s2
              }
            , if hasSpawn && List.length updatedFlies <= model.maxPopulation then
                Random.generate (\n -> SpawnFlies n updatedFlies) (Random.int 1 5)

              else
                Cmd.none
            )

        SpawnFlies n flies ->
            let
                ( newFlies, newSeed ) =
                    Random.step (fliesGenerator n model) model.seed
            in
            ( { model
                | flies = flies ++ newFlies
                , seed = newSeed
              }
            , Cmd.none
            )


instanceNewFlies : Model -> List Fly -> ( List Fly, Random.Seed )
instanceNewFlies model flies =
    case flies of
        [] ->
            Random.step (fliesGenerator 3 model) model.seed

        _ ->
            ( flies, model.seed )


fliesGenerator : Int -> Model -> Random.Generator (List Fly)
fliesGenerator n model =
    flyGenerator model
        |> Random.list n


flyGenerator : Model -> Random.Generator Fly
flyGenerator { dim, maxSpeed } =
    let
        ( _, sizeY ) =
            dim
    in
    Random.map3
        (Fly 0)
        (Random.int 0 sizeY)
        (Random.int -maxSpeed maxSpeed)
        (Random.int -maxSpeed maxSpeed)


isOutside : Int -> ( Int, Int ) -> Fly -> Bool
isOutside tol ( sizeX, sizeY ) { x, y } =
    x > -tol && x < sizeX + tol && y > -tol && y < sizeY + tol


updateFlies : Model -> List Fly -> ( List Fly, Random.Seed )
updateFlies model flies =
    case flies of
        [] ->
            ( [], model.seed )

        fly :: rest ->
            let
                ( newFly, s1 ) =
                    motionFly model fly

                ( updatedRest, newSeed ) =
                    updateFlies { model | seed = s1 } rest
            in
            ( newFly :: updatedRest, newSeed )


generatorSpeedFly : Model -> Int -> Random.Generator Int
generatorSpeedFly { maxSpeed } oldSpeed =
    Random.weighted ( 80, True ) [ ( 20, False ) ]
        |> Random.andThen
            (\hasInertia ->
                if hasInertia then
                    Random.constant oldSpeed

                else
                    Random.int -maxSpeed maxSpeed
            )


motionFly : Model -> Fly -> ( Fly, Random.Seed )
motionFly model { x, y, vertSpeed, horizSpeed } =
    let
        ( frogyX, frogyY ) =
            ( model.frogy.x, model.frogy.y )

        ( newVertSpeed, s1 ) =
            Random.step (generatorSpeedFly model vertSpeed) model.seed

        ( newHorizSpeed, s2 ) =
            Random.step (generatorSpeedFly model horizSpeed) s1

        inertia s =
            s // 10

        horizInertia =
            inertia horizSpeed

        vertInertia =
            inertia vertSpeed

        repultion origin coord =
            if origin - coord == 0 then
                0

            else
                (origin - coord) // 10000

        horizRepulsion =
            repultion frogyX x

        vertRepulsion =
            repultion frogyY y
    in
    ( { x = x + newHorizSpeed + horizInertia + horizRepulsion
      , y = y + newVertSpeed + vertInertia + vertRepulsion
      , vertSpeed = newVertSpeed
      , horizSpeed = newHorizSpeed
      }
    , s2
    )


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
                    if tongue.x == x && tongue.y == y then
                        { tongue | status = Retracting }

                    else
                        { tongue
                            | x = tongue.x - abs (x - tongue.x) // tongue.speed
                            , y = tongue.y - abs (y - tongue.y) // tongue.speed
                        }

                Retracting ->
                    if tongue.x == frogy.x && tongue.y == frogy.y then
                        { tongue | status = Retracted }

                    else
                        { tongue
                            | x = tongue.x + abs (frogy.x - tongue.x) // tongue.speed
                            , y = tongue.y + abs (frogy.y - tongue.y) // tongue.speed
                        }

                Retracted ->
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
