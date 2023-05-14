module Pages.Home_ exposing (Model, Msg, page)

import Browser.Events
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Page exposing (Page)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import View exposing (View)


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = OnAnimationFrame Float
    | OnMouseDown Int Int
    | OnMouseUp
    | SpawnFlies Int (List Fly)
    | OnTick
    | OnInputNickname String
    | GameStatusChange GameStatus


type alias Model =
    { frogy : Frogy
    , flies : List Fly
    , dim : ( Int, Int )
    , maxSpeed : Int
    , maxPopulation : Int
    , seed : Random.Seed
    , score : Int
    , timer : Timer
    , status : GameStatus
    , gamer : String
    }


type GameStatus
    = Initializing
    | Start
    | Playing
    | Paused
    | Finished
    | Reset


type alias Timer =
    { start : Float
    , elapsed : Float
    , stop : Float
    }


type alias Fly =
    { x : Int
    , y : Int
    , vertSpeed : Int
    , horizSpeed : Int
    , status : FlyStatus
    }


type FlyStatus
    = Free
    | Caught
    | Dead


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


init : ( Model, Cmd Msg )
init =
    ( { frogy = initFrogy
      , flies = initFlies
      , dim = ( 800, 450 )
      , seed = Random.initialSeed 42
      , maxSpeed = 7
      , maxPopulation = 10
      , score = 0
      , timer =
            { start = 0
            , elapsed = 0
            , stop = 60
            }
      , status = Initializing
      , gamer = ""
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
    [ Fly 0 100 10 1 Free
    , Fly 0 200 10 2 Free
    , Fly 0 300 13 -5 Free
    ]


view : Model -> View Msg
view model =
    let
        ( x_, y_ ) =
            model.dim
    in
    { title = "The Wizard Frogs"
    , body =
        [ Html.div [ class "game" ]
            [ viewInializingModal model
            , viewModalFinished model
            , Html.div [ class "game__score" ] [ Html.pre [] [ text <| "Score: " ++ String.fromInt model.score ] ]
            , svg
                [ width <| String.fromInt x_
                , height <| String.fromInt y_
                , Svg.Attributes.style "background: #efefef"
                ]
                [ viewFrogy model.frogy
                , viewFlies model.flies
                ]
            , Html.div
                [ class "game__controls" ]
                [ Html.button
                    [ class "play"
                    , Html.Events.onClick (clicker model.status)
                    ]
                    []
                , Html.div [ class "progress__bar" ]
                    [ Html.div
                        [ class "progress__bar__fill"
                        , Attr.style "width" <| String.fromFloat (Basics.min 100 (model.timer.elapsed / model.timer.stop * 100)) ++ "%"
                        ]
                        []
                    ]
                , Html.span [ class "timer" ]
                    [ model.timer.elapsed |> formatTimer |> text
                    ]
                ]
            ]
        ]
    }


viewInializingModal : Model -> Html.Html Msg
viewInializingModal { status, timer } =
    Html.div
        [ class ("modal" ++ " " ++ modalStatus Initializing status) ]
        [ Html.div
            [ class "modal__content" ]
            [ Html.h1 [] [ text "The Wizard Frogs" ]
            , Html.p [] [ text "You are a wizard frog. You have a magic tongue. You can catch flies with it." ]
            , Html.p [] [ text ("Catch as many flies as you can in " ++ String.fromFloat timer.stop ++ " seconds.") ]
            , Html.p [] [ text "Add your Nickname to the Hall of Fame." ]
            , Html.input
                [ class "modal__input"
                , Attr.placeholder "Nickname"
                , Html.Events.onInput OnInputNickname
                ]
                []
            , Html.p [ class "click__to__start" ] [ text "Click to start the game." ]
            ]
        ]


viewModalFinished : Model -> Html.Html Msg
viewModalFinished { status, gamer, score } =
    let
        genLink g s =
            "https://wizard-frogs.netlify.com/?gamer=" ++ g ++ "&score=" ++ String.fromInt s

        gamer_ =
            if gamer == "" then
                "Anonymous"

            else
                gamer
    in
    Html.div
        [ class ("modal" ++ " " ++ modalStatus Finished status) ]
        [ Html.div
            [ class "modal__content" ]
            [ Html.h1 [] [ text "It's over!" ]
            , Html.p [] [ text <| "Your score is " ++ String.fromInt score ++ "." ]
            , Html.p [] [ text "Challenge your friends using this link:" ]
            , Html.p [ class "share__link" ] [ text <| genLink gamer_ score ]
            ]
        ]


modalStatus : GameStatus -> GameStatus -> String
modalStatus currentStatus status =
    if currentStatus == status then
        "modal__active"

    else
        "modal__inactive"


formatTimer : Float -> String
formatTimer time =
    let
        minutes =
            floor (time / 60)

        seconds =
            floor (time - (toFloat minutes * 60))

        fillZero n =
            if n < 10 then
                "0" ++ String.fromInt n

            else
                String.fromInt n
    in
    fillZero minutes ++ ":" ++ fillZero seconds


viewFlies : List Fly -> Svg.Svg Msg
viewFlies flies =
    g [] (List.map viewFly flies)


viewFly : Fly -> Svg.Svg Msg
viewFly { x, y, status } =
    circle
        [ cx <| String.fromInt x
        , cy <| String.fromInt y
        , r "5"
        , fill <|
            case status of
                Free ->
                    "black"

                Caught ->
                    "blue"

                Dead ->
                    "red"
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
        OnInputNickname nickname ->
            ( { model | gamer = nickname }, Cmd.none )

        OnMouseDown x y ->
            ( { model | frogy = updateTongueStatus model.frogy (Moving x y) }, Cmd.none )

        OnMouseUp ->
            ( { model | frogy = updateTongueStatus model.frogy Retracting }, Cmd.none )

        OnAnimationFrame timeDelta ->
            let
                ( updatedFlies, score ) =
                    model.flies
                        |> List.filter (\{ x, y } -> isOutside 100 model.dim ( x, y ))
                        |> caughtFlies model.frogy

                ( newFlies, s1 ) =
                    updatedFlies
                        |> instanceNewFlies model
                        |> (\( flies, seed ) -> updateFlies { model | seed = seed } flies)

                ( hasSpawn, s2 ) =
                    Random.step (Random.weighted ( 10, True ) [ ( 90, False ) ]) s1
            in
            ( { model
                | frogy = updateFrogy model.frogy
                , flies = newFlies
                , seed = s2
                , score = model.score + score
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

        OnTick ->
            let
                timer =
                    model.timer

                newTimer =
                    { timer
                        | elapsed = timer.elapsed + 1
                    }

                newStatus =
                    if timer.elapsed >= timer.stop then
                        Finished

                    else
                        model.status
            in
            ( { model | timer = newTimer, status = newStatus }, Cmd.none )

        GameStatusChange status ->
            let
                timer =
                    model.timer

                newTimer =
                    case status of
                        Start ->
                            { timer | elapsed = 0 }

                        _ ->
                            model.timer
            in
            ( if status == Reset then
                let
                    newModel =
                        Tuple.first init
                in
                { newModel | status = Playing }

              else
                { model | timer = newTimer, status = status }
            , Cmd.none
            )


caughtFlies : Frogy -> List Fly -> ( List Fly, Int )
caughtFlies frogy flies =
    let
        isCaught : Fly -> Frogy -> Bool
        isCaught { x, y, status } { tongue } =
            List.all identity
                [ status == Free
                , x > tongue.x - 15
                , x < tongue.x + 15
                , y > tongue.y - 15
                , y < tongue.y + 15
                ]

        caught fly =
            case ( fly.status, isCaught fly frogy, frogy.tongue.status ) of
                ( Free, True, Moving _ _ ) ->
                    { fly | status = Caught }

                ( Caught, True, Retracted ) ->
                    { fly | status = Dead }

                _ ->
                    fly

        ( aliveFlies, deadFlies ) =
            flies
                |> List.map caught
                |> List.partition (\{ status } -> status /= Caught)
    in
    ( aliveFlies, List.length deadFlies )


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
    Random.map4
        (Fly 0)
        (Random.int 0 sizeY)
        (Random.int -maxSpeed maxSpeed)
        (Random.int -maxSpeed maxSpeed)
        (Random.constant Free)


isOutside : Int -> ( Int, Int ) -> ( Int, Int ) -> Bool
isOutside tol ( sizeX, sizeY ) ( x, y ) =
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
motionFly model { x, y, vertSpeed, horizSpeed, status } =
    case status of
        Caught ->
            ( Fly model.frogy.x model.frogy.y vertSpeed horizSpeed Free, model.seed )

        Free ->
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
              , status = Free
              }
            , s2
            )

        _ ->
            ( Fly x y vertSpeed horizSpeed status, model.seed )


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
subscriptions model =
    if model.status == Playing then
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
            , Browser.Events.onMouseDown (Decode.map (\( x, y ) -> OnMouseDown x y) keyDecoder)
            , Browser.Events.onMouseUp (Decode.succeed OnMouseUp)
            , Time.every 1000 (\_ -> OnTick)
            ]

    else
        Sub.none


keyDecoder : Decode.Decoder ( Int, Int )
keyDecoder =
    Decode.map2
        (\x y -> ( x, y ))
        (Decode.field "clientX" Decode.int)
        (Decode.field "clientY" Decode.int)


clicker : GameStatus -> Msg
clicker status =
    let
        newStatus =
            case status of
                Playing ->
                    Paused

                Finished ->
                    Reset

                _ ->
                    Playing
    in
    GameStatusChange newStatus
