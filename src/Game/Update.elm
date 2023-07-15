module Game.Update exposing (update)

import Game.Init as Init
import Game.Model exposing (..)
import Random


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnInputNickname nickname ->
            ( { model | gamer = nickname }, Cmd.none )

        OnMouseDown x y ->
            if x < 0 || x > model.board.width || y < 0 || y > model.board.height then
                ( model, Cmd.none )

            else
                ( { model | frogy = updateTongueStatus model.frogy (Moving x y) }, Cmd.none )

        OnAnimationFrame _ ->
            let
                ( updatedFlies, score ) =
                    model.flies
                        |> List.filter (\{ x, y } -> isOutside 100 model.board ( x, y ))
                        |> caughtFlies model.frogy

                ( newFlies, s1 ) =
                    updatedFlies
                        |> instanceNewFlies model
                        |> (\( flies, seed ) -> updateFlies { model | seed = seed } flies)

                ( hasSpawn, s2 ) =
                    Random.step (Random.weighted ( 10, True ) [ ( 90, False ) ]) s1
            in
            ( { model
                | frogy = updateFrogy model
                , flies = newFlies
                , seed = s2
                , score = model.score + score
              }
            , if hasSpawn && List.length updatedFlies <= model.maxPopulation then
                Random.generate (\n -> SpawnFlies n updatedFlies) (Random.int 1 5)

              else
                Cmd.none
            )

        SpawnFlies n flies ->
            let
                ( newFlies, newSeed ) =
                    Random.step (fliesGenerator n model) model.seed
            in
            ( { model
                | flies = flies ++ newFlies
                , seed = newSeed
              }
            , Cmd.none
            )

        OnTick ->
            let
                timer =
                    model.timer

                newTimer =
                    { timer
                        | elapsed = timer.elapsed + 1
                    }

                newStatus =
                    if timer.elapsed >= timer.stop then
                        Finished

                    else
                        model.status
            in
            ( { model | timer = newTimer, status = newStatus }, Cmd.none )

        GameStatusChange status ->
            let
                timer =
                    model.timer

                newTimer =
                    case status of
                        Start ->
                            { timer | elapsed = 0 }

                        _ ->
                            model.timer
            in
            ( if status == Reset then
                let
                    newModel =
                        reset model
                in
                { newModel | status = Playing }

              else
                { model | timer = newTimer, status = status }
            , Cmd.none
            )


reset : Model -> Model
reset model =
    Init.init { challenger = model.challenger, url = model.url } |> Tuple.first


caughtFlies : Frogy -> List Fly -> ( List Fly, Int )
caughtFlies frogy flies =
    let
        isCaught : Fly -> Frogy -> Bool
        isCaught { x, y, status } { tongue } =
            List.all identity
                [ status == Free
                , x > tongue.x - 15
                , x < tongue.x + 15
                , y > tongue.y - 15
                , y < tongue.y + 15
                ]

        caught fly =
            case ( fly.status, isCaught fly frogy, frogy.tongue.status ) of
                ( Free, True, Moving _ _ ) ->
                    { fly | status = Caught }

                ( Caught, True, Retracted ) ->
                    { fly | status = Dead }

                _ ->
                    fly

        ( aliveFlies, deadFlies ) =
            flies
                |> List.map caught
                |> List.partition (\{ status } -> status /= Caught)
    in
    ( aliveFlies, List.length deadFlies )


instanceNewFlies : Model -> List Fly -> ( List Fly, Random.Seed )
instanceNewFlies model flies =
    case flies of
        [] ->
            Random.step (fliesGenerator 3 model) model.seed

        _ ->
            ( flies, model.seed )


fliesGenerator : Int -> Model -> Random.Generator (List Fly)
fliesGenerator n model =
    flyGenerator model
        |> Random.list n


flyGenerator : Model -> Random.Generator Fly
flyGenerator { board, maxSpeed } =
    Random.map4
        (Fly 0)
        (Random.int 0 board.height)
        (Random.int -maxSpeed maxSpeed)
        (Random.int -maxSpeed maxSpeed)
        (Random.constant Free)


isOutside : Int -> Board -> ( Int, Int ) -> Bool
isOutside tol { width, height } ( x, y ) =
    x > -tol && x < width + tol && y > -tol && y < height + tol


updateFlies : Model -> List Fly -> ( List Fly, Random.Seed )
updateFlies model flies =
    case flies of
        [] ->
            ( [], model.seed )

        fly :: rest ->
            let
                ( newFly, s1 ) =
                    motionFly model fly

                ( updatedRest, newSeed ) =
                    updateFlies { model | seed = s1 } rest
            in
            ( newFly :: updatedRest, newSeed )


generatorSpeedFly : Model -> Int -> Random.Generator Int
generatorSpeedFly { maxSpeed } oldSpeed =
    Random.weighted ( 80, True ) [ ( 20, False ) ]
        |> Random.andThen
            (\hasInertia ->
                if hasInertia then
                    Random.constant oldSpeed

                else
                    Random.int -maxSpeed maxSpeed
            )


motionFly : Model -> Fly -> ( Fly, Random.Seed )
motionFly model { x, y, vertSpeed, horizSpeed, status } =
    case status of
        Caught ->
            ( Fly model.frogy.x model.frogy.y vertSpeed horizSpeed Free, model.seed )

        Free ->
            let
                ( frogyX, frogyY ) =
                    ( model.frogy.x, model.frogy.y )

                ( newVertSpeed, s1 ) =
                    Random.step (generatorSpeedFly model vertSpeed) model.seed

                ( newHorizSpeed, s2 ) =
                    Random.step (generatorSpeedFly model horizSpeed) s1

                inertia s =
                    s // 10

                horizInertia =
                    inertia horizSpeed

                vertInertia =
                    inertia vertSpeed

                repultion origin coord =
                    if origin - coord == 0 then
                        0

                    else
                        (origin - coord) // 10000

                horizRepulsion =
                    repultion frogyX x

                vertRepulsion =
                    repultion frogyY y
            in
            ( { x = x + newHorizSpeed + horizInertia + horizRepulsion
              , y = y + newVertSpeed + vertInertia + vertRepulsion
              , vertSpeed = newVertSpeed
              , horizSpeed = newHorizSpeed
              , status = Free
              }
            , s2
            )

        _ ->
            ( Fly x y vertSpeed horizSpeed status, model.seed )


updateTongueStatus : Frogy -> TongueStatus -> Frogy
updateTongueStatus frogy status =
    let
        tongue =
            frogy.tongue
    in
    { frogy | tongue = { tongue | status = status } }


updateFrogy : Model -> Frogy
updateFrogy { frogy, board } =
    let
        tongue =
            frogy.tongue

        updateTongue =
            case frogy.tongue.status of
                Moving x y ->
                    let
                        ( maxX, maxY ) =
                            ( board.width - 10, board.height - 10 )

                        movX =
                            tongue.x + (x - tongue.x) // tongue.speed

                        movY =
                            tongue.y + (y - tongue.y) // tongue.speed

                        hasArrived tol =
                            tongue.x - x < tol && tongue.y - y < tol
                    in
                    if hasArrived 10 then
                        { tongue | status = Retracting }

                    else
                        { tongue
                            | x = movX |> Basics.max 0 |> Basics.min maxX
                            , y = movY |> Basics.max 0 |> Basics.min maxY
                        }

                Retracting ->
                    if tongue.x == frogy.x && tongue.y == frogy.y then
                        { tongue | status = Retracted }

                    else
                        { tongue
                            | x = tongue.x + abs (frogy.x - tongue.x) // tongue.speed
                            , y = tongue.y + abs (frogy.y - tongue.y) // tongue.speed
                        }

                Retracted ->
                    tongue
    in
    { frogy | tongue = updateTongue }
