module Game.Model exposing
    ( Board
    , Fly
    , FlyStatus(..)
    , Frogy
    , GameElement
    , GameStatus(..)
    , Model
    , Msg(..)
    , TongueStatus(..)
    )

import Lib.Challenger exposing (Challenger)
import Random
import Url
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
    , url : Maybe Url.Url
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
