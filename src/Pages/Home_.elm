module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )


type Msg
    = Clicked


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Clicked ->
            ( model
            , Effect.loadExternalUrl "/frogy"
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> View Msg
view _ =
    { title = "Home - The Wizard Frogy"
    , body =
        [ Html.div
            [ Attr.class "container bg-dark text-white text-center vh-100 d-flex flex-column justify-content-center mw-100" ]
            [ Html.div
                [ Attr.class "row" ]
                [ Html.div
                    [ Attr.class "col-12" ]
                    [ Html.i
                        [ Attr.id "frog"
                        , Attr.class "fa-solid fa-frog fa-bounce fa-2xl text-white"
                        ]
                        []
                    , Html.h1
                        [ Attr.class "mt-3" ]
                        [ Html.text "Welcome to The Wizard Frogy" ]
                    , Html.p
                        [ Attr.class "lead mt-3" ]
                        [ Html.text "This is a game made using the Elm programming language. You can play it on your computer or on your phone." ]
                    ]
                ]
            , Html.div
                [ Attr.class "row" ]
                [ Html.div
                    [ Attr.class "col-12" ]
                    [ Html.button
                        [ Attr.class "btn btn-primary bg-success border border-white"
                        , onClick Clicked
                        ]
                        [ Html.text "Start the game"
                        ]
                    ]
                ]
            ]
        ]
    }
