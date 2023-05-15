module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (Html, button, h1, p, a, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Page exposing (Page)
import View exposing (View)


page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { repository : String
    }


init : ( Model, Cmd Msg )
init =
    ( { repository = "https://github.com/FabianVegaA/The-Wizard-Frogy"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GoToGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , body = viewHome model
    }


viewHome : Model -> List (Html Msg)
viewHome { repository } =
    [ h1 [] [ text "Welcome to \"The Wizard Frogy\"!" ]
    , p [] [ text "This is a game made with Elm. It's a work in progress." ]
    , p []
        [ text "You can find the source code on "
        , a [ class "link", href repository ] [ text "GitHub" ]
        , text ". You can also contribute to the game by making a pull request or giving feedback in the issues section."
        ]
    , p [] [ text "Have fun! Press the button to start." ]
    , button [
        onClick GoToGame
    ] [ text "Start" ]
    ]
