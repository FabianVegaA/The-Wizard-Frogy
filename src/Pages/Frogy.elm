module Pages.Frogy exposing (Model, Msg, page)

import Game exposing (Model, Msg, game)
import Page exposing (Page)


type alias Model =
    Game.Model


type alias Msg =
    Game.Msg


page : Page Model Msg
page =
    Page.element (game Nothing)
