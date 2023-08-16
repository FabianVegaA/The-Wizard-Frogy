module Game exposing (game)

import Game.Init as Init
import Game.Model exposing (..)
import Game.Subcriptions as Subscriptions
import Game.Update as Update
import Game.View as View
import Lib.Challenger exposing (Challenger)
import Url exposing (Url)


game : { challenger : Maybe Challenger, url : Maybe Url } -> GameElement Model Msg
game params =
    { init = Init.init params
    , view = View.view
    , update = Update.update
    , subscriptions = Subscriptions.subscriptions
    }
