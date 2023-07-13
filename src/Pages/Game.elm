module Pages.Game exposing (Model, Msg, page)

import Dict
import Effect exposing (sendCmd)
import Game exposing (game)
import Lib.Challenger exposing (parseChallenger)
import Lib.Encrypt exposing (decrypt)
import Page exposing (Page)
import Route exposing (Route)
import Shared


type alias Model =
    Game.Model


type alias Msg =
    Game.Msg


page : Shared.Model -> Route () -> Page Model Msg
page _ { query } =
    let
        challenger =
            Dict.get "challenger" query
                |> Maybe.map2 decrypt (Dict.get "shift" query |> Maybe.andThen String.toInt)
                |> Maybe.withDefault Nothing
                |> Maybe.andThen parseChallenger

        { init, update, subscriptions, view } =
            game { challenger = challenger }

        toEffect ( m, cmd ) =
            ( m, sendCmd cmd )

        initEffect _ =
            init |> toEffect

        updateEffect msg_ model_ =
            update msg_ model_ |> toEffect
    in
    Page.new
        { init = initEffect
        , update = updateEffect
        , subscriptions = subscriptions
        , view = view
        }
