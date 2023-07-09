module Pages.Frogy.Challenger_ exposing (Model, Msg, page)

import Game exposing (game)
import Page exposing (Page)


type alias Model =
    Game.Model


type alias Msg =
    Game.Msg


page : { challenger : String } -> Page Model Msg
page params =
    Page.element (game (Just params))
