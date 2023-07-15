module Game.Init exposing (init)

import Game.Model exposing (..)
import Lib.Challenger exposing (Challenger)
import Random
import Url


init : { challenger : Maybe Challenger, url : Maybe Url.Url } -> ( Model, Cmd Msg )
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
      , url = params.url
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
