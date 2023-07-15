module Game.View exposing (view)

import Game.Model exposing (..)
import Html
import Html.Attributes as Attr
import Html.Events
import Lib.Encrypt exposing (encrypt)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Url
import View exposing (View)


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
