module Game.View exposing (view)

import Game.Model exposing (..)
import Html
import Html.Attributes as Attr
import Html.Events
import Lib.Encrypt exposing (encrypt)
import Random
import Route.Path exposing (Path(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Url
import View exposing (View)


view : Model -> View Msg
view model =
    { title = "The Wizard Frogs"
    , body =
        [ Html.div [ class "game" ]
            [ viewBackground model
            , Html.div
                []
                [ viewInializingModal model
                , case model.status of
                    Finished ->
                        viewModalFinished model

                    _ ->
                        Html.div [] []
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
                        [ class <| "play_button " ++ useGameStatusClass model.status
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


useGameStatusClass : GameStatus -> String
useGameStatusClass status =
    case status of
        Playing ->
            "pause_button__icon"

        Finished ->
            "reset_button__icon"

        _ ->
            "play_button__icon"


viewBackground : Model -> Html.Html Msg
viewBackground model =
    let
        rain =
            List.repeat 10 (viewRain model)
    in
    Html.div
        [ class "game__board__background"
        ]
        rain


viewRain : Model -> Html.Html Msg
viewRain model =
    let
        waves =
            Random.step (Random.int 1 5) model.seed
                |> Tuple.first
                |> (\n -> List.repeat n (Html.div [] []))

        particles =
            Random.step (Random.int 3 20) model.seed
                |> Tuple.first
                |> (\n -> List.repeat n (Html.div [] []))
    in
    Html.div [ class "rain" ]
        [ Html.div [ class "drop" ] []
        , Html.div [ class "waves" ] waves
        , Html.div [ class "splash" ] []
        , Html.div [ class "particles" ] particles
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
viewInializingModal { status, timer, challenger } =
    let
        challengerMsg =
            case challenger of
                Just c ->
                    [ Html.p []
                        [ Html.b [ class "challenger__property" ] [ text c.name ]
                        , text " is challenging you to beat his score of "
                        , Html.b [ class "challenger__property" ] [ text <| String.fromInt c.score ++ " ★" ]
                        ]
                    ]

                Nothing ->
                    []
    in
    Html.div
        [ class ("_modal" ++ " " ++ modalStatus Initializing status) ]
        [ Html.div
            [ class "modal__content" ]
            ([ Html.h1 [] [ text "The Wizard Frogs" ]
             , Html.p [] [ text "You are a wizard frog. You have a magic tongue. You can catch stars with it." ]
             , Html.p [] [ text ("Catch as many stars as you can in " ++ String.fromFloat timer.stop ++ " seconds.") ]
             , Html.br [] []
             ]
                ++ challengerMsg
                ++ [ Html.p [] [ text "Add your Nickname to the Hall of Fame." ]
                   , Html.input
                        [ class "modal__input"
                        , Attr.placeholder "Nickname"
                        , Html.Events.onInput OnInputNickname
                        , Attr.required True
                        , Attr.pattern "^[a-zA-Z0-9\\-_\\.@]{1,20}$"
                        ]
                        []
                   , Html.p [ class "click__to__start" ] [ text "Click to start the game." ]
                   ]
            )
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
            let
                query =
                    "shift=" ++ String.fromInt shift ++ "&challenger=" ++ encrypted
            in
            model.url
                |> Maybe.map (\url -> { url | query = Just query })
                |> Maybe.map Url.toString
                |> Maybe.withDefault ""

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
viewFly { x, y, angle } =
    Svg.text_
        [ Svg.Attributes.x <| String.fromInt x
        , Svg.Attributes.y <| String.fromInt y
        , Svg.Attributes.fontSize "50"
        , Svg.Attributes.rotate <| String.fromFloat angle
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

        tongue_ =
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

        frogy_ =
            [ ellipse
                [ cx <| String.fromInt <| frogy.x - 40
                , cy <| String.fromInt <| frogy.y + 40
                , rx "60"
                , ry "40"
                , rotate "90"
                , fill "#005823"
                ]
                []
            , ellipse
                [ cx <| String.fromInt <| frogy.x + 40
                , cy <| String.fromInt <| frogy.y - 40
                , rx "40"
                , ry "60"
                , rotate "90"
                , fill "#005823"
                ]
                []
            , circle
                [ cx <| String.fromInt frogy.x
                , cy <| String.fromInt frogy.y
                , r "80"
                , fill "#005823"
                ]
                []
            , circle
                [ cx <| String.fromInt <| frogy.x - 90
                , cy <| String.fromInt <| frogy.y - 30
                , r "30"
                , fill "#005823"
                ]
                []
            , circle
                [ cx <| String.fromInt <| frogy.x - 40
                , cy <| String.fromInt <| frogy.y - 80
                , r "30"
                , fill "#005823"
                ]
                []
            , ellipse
                [ cx <| String.fromInt <| frogy.x - 20
                , cy <| String.fromInt <| frogy.y + 30
                , rx "20"
                , ry "15"
                , rotate "90"
                , fill "#00b047"
                ]
                []
            , ellipse
                [ cx <| String.fromInt <| frogy.x + 10
                , cy <| String.fromInt <| frogy.y - 30
                , rx "15"
                , ry "20"
                , rotate "90"
                , fill "#00b047"
                ]
                []
            , ellipse
                [ cx <| String.fromInt <| frogy.x + 40
                , cy <| String.fromInt <| frogy.y + 20
                , rx "15"
                , ry "17"
                , rotate "90"
                , fill "#00b047"
                ]
                []
            ]
    in
    g
        [ fill "white"
        , stroke "black"
        , strokeWidth "5"
        ]
        (tongue_ ++ frogy_)


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
