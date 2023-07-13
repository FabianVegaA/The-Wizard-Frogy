module Game exposing (Model, Msg, game)

import Browser.Events
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Lib.Challenger exposing (Challenger)
import Lib.Encrypt exposing (encrypt)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time
import View exposing (View)


type alias GameElement a msg =
    { init : ( a, Cmd msg )
    , update : msg -> a -> ( a, Cmd msg )
    , subscriptions : a -> Sub msg
    , view : a -> View msg
    }


type alias Model =
    { frogy : Frogy
    , flies : List Fly
    , board : Board
    , maxSpeed : Int
    , maxPopulation : Int
    , seed : Random.Seed
    , score : Int
    , timer : Timer
    , status : GameStatus
    , gamer : String
    , challenger : Maybe Challenger
    }


type Msg
    = OnAnimationFrame Float
    | OnMouseDown Int Int
    | SpawnFlies Int (List Fly)
    | OnTick
    | OnInputNickname String
    | GameStatusChange GameStatus


type alias Board =
    { width : Int
    , height : Int
    , left : Int
    , top : Int
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
    , tol : Int
    , status : TongueStatus
    }


type TongueStatus
    = Moving Int Int
    | Retracting
    | Retracted


game : { challenger : Maybe Challenger } -> GameElement Model Msg
game params =
    { init = init params
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : { challenger : Maybe Challenger } -> ( Model, Cmd Msg )
init params =
    ( { frogy = initFrogy
      , flies = initFlies
      , board = { width = 800, height = 450, left = 55, top = 65 }
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
      , challenger = params.challenger
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
        , tol = 10
        , status = Retracted
        }
    }


initFlies : List Fly
initFlies =
    let
        initFly x y sx sy =
            Fly x y sx sy Free
    in
    [ initFly 0 100 10 1
    , initFly 0 200 10 2
    , initFly 0 300 13 -5
    ]


view : Model -> View Msg
view model =
    { title = "The Wizard Frogs"
    , body =
        [ Html.div [ class "game" ]
            [ viewBackground
            , Html.div
                []
                [ viewInializingModal model
                , viewModalFinished model
                , Html.div [ class "game__score" ] [ Html.pre [] [ text <| "Score: " ++ String.fromInt model.score ] ]
                , svg
                    [ width <| String.fromInt model.board.width
                    , height <| String.fromInt model.board.height
                    , class "game__board"
                    ]
                    [ Svg.defs []
                        [ Svg.filter [ id "blur" ]
                            [ Svg.feGaussianBlur [ stdDeviation "0.4" ] [] ]
                        ]
                    , viewFrogy model.frogy
                    , case model.status of
                        Initializing ->
                            Html.div [] []

                        _ ->
                            viewFlies model.flies
                    ]
                , Html.div
                    [ class "game__controls" ]
                    [ Html.button
                        [ class "play_button"
                        , Html.Events.onClick (clicker model)
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
        , viewFooter model
        ]
    }


viewBackground : Html.Html Msg
viewBackground =
    Html.div
        [ class "game__board__background"
        ]
        [ viewRain
        , viewRain
        , viewRain
        , viewRain
        , viewRain
        ]


viewRain : Html.Html Msg
viewRain =
    Html.div [ class "rain" ]
        [ Html.div [ class "drop" ] []
        , Html.div [ class "waves" ]
            [ Html.div [] []
            , Html.div [] []
            , Html.div [] []
            ]
        , Html.div [ class "splash" ] []
        , Html.div [ class "particles" ]
            [ Html.div [] []
            , Html.div [] []
            , Html.div [] []
            , Html.div [] []
            , Html.div [] []
            , Html.div [] []
            , Html.div [] []
            ]
        ]


viewFooter : Model -> Html.Html Msg
viewFooter _ =
    Html.footer
        [ class "_footer w-100" ]
        [ Html.div
            [ class "footer__media" ]
            [ Html.a
                [ Attr.href "https://www.linkedin.com/in/fabian-vega-alcota/"
                , class "footer__media__link"
                ]
                [ Html.i [ class "fa fa-linkedin" ] [] ]
            , Html.a
                [ Attr.href "https://twitter.com/fabianmativeal"
                , class "footer__media__link"
                ]
                [ Html.i [ class "fa fa-twitter" ] [] ]
            , Html.a
                [ Attr.href "https://github.com/FabianVegaA"
                , class "footer__media__link"
                ]
                [ Html.i [ class "fa fa-github" ] [] ]
            ]
        , Html.div
            [ class "footer__open__source" ]
            [ Html.span [] [ text "The source code is available on " ]
            , Html.a
                [ Attr.href "https://github.com/FabianVegaA/The-Wizard-Frogy"
                , class "footer__open__source__link"
                ]
                [ text "Repository" ]
            , Html.span [] [ text "." ]
            ]
        ]


viewInializingModal : Model -> Html.Html Msg
viewInializingModal { status, timer } =
    Html.div
        [ class ("_modal" ++ " " ++ modalStatus Initializing status) ]
        [ Html.div
            [ class "modal__content" ]
            [ Html.h1 [] [ text "The Wizard Frogs" ]
            , Html.p [] [ text "You are a wizard frog. You have a magic tongue. You can catch flies with it." ]
            , Html.p [] [ text ("Catch as many stars as you can in " ++ String.fromFloat timer.stop ++ " seconds.") ]
            , Html.p [] [ text "Add your Nickname to the Hall of Fame." ]
            , Html.input
                [ class "modal__input"
                , Attr.placeholder "Nickname"
                , Html.Events.onInput OnInputNickname
                , Attr.pattern "[a-zA-Z0-9-_.!?]+"
                ]
                []
            , Html.p [ class "click__to__start" ] [ text "Click to start the game." ]
            ]
        ]


viewModalFinished : Model -> Html.Html Msg
viewModalFinished model =
    let
        ( shift, _ ) =
            Random.step (Random.int 0 1000000) model.seed

        encrypted =
            encrypt shift (model.gamer ++ "," ++ String.fromInt model.score)
                |> Maybe.withDefault ""

        link =
            "https://the-wizard-frogy.netlify.com/game?shift=" ++ String.fromInt shift ++ "&challenger=" ++ encrypted

        ( winnerMsg, classModal ) =
            case model.challenger of
                Just { name, score } ->
                    if model.score > score then
                        ( "You won against " ++ name ++ "!", "modal__won" )

                    else if model.score < score then
                        ( "You lost against " ++ name ++ ".", "modal__lost" )

                    else
                        ( "You tied with " ++ name ++ ".", "modal__tied" )

                Nothing ->
                    ( "", "" )
    in
    Html.div
        [ class ("_modal" ++ " " ++ modalStatus Finished model.status) ]
        [ Html.div
            [ class "modal__content" ]
            [ Html.h1 [] [ text "It's over!" ]
            , Html.p [ class (classModal ++ " " ++ "game_result") ] [ text winnerMsg ]
            , Html.p [] [ text "Challenge your friends using this link:" ]
            , Html.p [ class "share__link" ] [ text <| link ]
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
viewFly { x, y } =
    Svg.text_
        [ Svg.Attributes.x <| String.fromInt x
        , Svg.Attributes.y <| String.fromInt y
        , Svg.Attributes.fontSize "30"
        , Svg.Attributes.rotate "30"
        , Svg.Attributes.fill "#b7dade"
        , Svg.Attributes.filter "url(#blur)"
        , id "fly"
        ]
        [ text "★" ]


viewFrogy : Frogy -> Svg.Svg Msg
viewFrogy frogy =
    let
        tongue =
            frogy.tongue

        viewTongue =
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
            ]
    in
    g
        [ fill "white"
        , stroke "black"
        , strokeWidth "5"
        ]
        (viewTongue
            ++ [ circle
                    [ cx <| String.fromInt frogy.x
                    , cy <| String.fromInt frogy.y
                    , r "80"
                    , fill "green"
                    ]
                    []
               ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInputNickname nickname ->
            ( { model | gamer = nickname }, Cmd.none )

        OnMouseDown x y ->
            if x < 0 || x > model.board.width || y < 0 || y > model.board.height then
                ( model, Cmd.none )

            else
                ( { model | frogy = updateTongueStatus model.frogy (Moving x y) }, Cmd.none )

        OnAnimationFrame _ ->
            let
                ( updatedFlies, score ) =
                    model.flies
                        |> List.filter (\{ x, y } -> isOutside 100 model.board ( x, y ))
                        |> caughtFlies model.frogy

                ( newFlies, s1 ) =
                    updatedFlies
                        |> instanceNewFlies model
                        |> (\( flies, seed ) -> updateFlies { model | seed = seed } flies)

                ( hasSpawn, s2 ) =
                    Random.step (Random.weighted ( 10, True ) [ ( 90, False ) ]) s1
            in
            ( { model
                | frogy = updateFrogy model
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
                        reset model
                in
                { newModel | status = Playing }

              else
                { model | timer = newTimer, status = status }
            , Cmd.none
            )


reset : Model -> Model
reset _ =
    -- FIXME: get params from url
    init { challenger = Nothing } |> Tuple.first


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
flyGenerator { board, maxSpeed } =
    Random.map4
        (Fly 0)
        (Random.int 0 board.height)
        (Random.int -maxSpeed maxSpeed)
        (Random.int -maxSpeed maxSpeed)
        (Random.constant Free)


isOutside : Int -> Board -> ( Int, Int ) -> Bool
isOutside tol { width, height } ( x, y ) =
    x > -tol && x < width + tol && y > -tol && y < height + tol


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


updateFrogy : Model -> Frogy
updateFrogy { frogy, board } =
    let
        tongue =
            frogy.tongue

        updateTongue =
            case frogy.tongue.status of
                Moving x y ->
                    let
                        ( maxX, maxY ) =
                            ( board.width - 10, board.height - 10 )

                        movX =
                            tongue.x + (x - tongue.x) // tongue.speed

                        movY =
                            tongue.y + (y - tongue.y) // tongue.speed

                        hasArrived tol =
                            tongue.x - x < tol && tongue.y - y < tol
                    in
                    if hasArrived 10 then
                        { tongue | status = Retracting }

                    else
                        { tongue
                            | x = movX |> Basics.max 0 |> Basics.min maxX
                            , y = movY |> Basics.max 0 |> Basics.min maxY
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
subscriptions { status, board } =
    if status == Playing then
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta OnAnimationFrame
            , Browser.Events.onClick (Decode.map (\( x, y ) -> OnMouseDown (x - board.left) (y - board.top)) keyDecoder)
            , Time.every 1000 (\_ -> OnTick)
            ]

    else
        Sub.none


keyDecoder : Decode.Decoder ( Int, Int )
keyDecoder =
    let
        clientX =
            Decode.field "clientX" Decode.int

        clientY =
            Decode.field "clientY" Decode.int
    in
    Decode.map2 Tuple.pair clientX clientY


clicker : Model -> Msg
clicker { status, gamer } =
    let
        newStatus =
            case status of
                Playing ->
                    Paused

                Finished ->
                    Reset

                Initializing ->
                    if gamer == "" then
                        Initializing

                    else
                        Playing

                _ ->
                    Playing
    in
    GameStatusChange newStatus
