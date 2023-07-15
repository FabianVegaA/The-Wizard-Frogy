module Game.Subcriptions exposing (subscriptions)

import Browser.Events exposing (onAnimationFrameDelta, onClick)
import Game.Model exposing (..)
import Json.Decode as Decode
import Time


subscriptions : Model -> Sub Msg
subscriptions { status, board } =
    if status == Playing then
        Sub.batch
            [ onAnimationFrameDelta OnAnimationFrame
            , onClick (Decode.map (\( x, y ) -> OnMouseDown (x - board.left) (y - board.top)) keyDecoder)
            , Time.every 1000 (\_ -> OnTick)
            ]

    else
        Sub.none


keyDecoder : Decode.Decoder ( Int, Int )
keyDecoder =
    let
        clientX =
            Decode.field "clientX" Decode.int

        clientY =
            Decode.field "clientY" Decode.int
    in
    Decode.map2 Tuple.pair clientX clientY
