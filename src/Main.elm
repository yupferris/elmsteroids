module Main exposing (..)

import Html exposing (Html)
import Random exposing (..)
import Time exposing (..)
import AnimationFrame exposing (..)
import Keyboard exposing (..)
import Collage exposing (Form, collage, group, rect, filled, text, moveY, scale, alpha, scale)
import Element
import Color exposing (..)
import State exposing (..)
import DefaultText exposing (..)
import Bounds exposing (..)
import Stars exposing (Star)
import Player exposing (Player)
import Asteroids exposing (Asteroid)
import Bullets exposing (Bullet)
import SegmentParticles exposing (SegmentParticle)
import KeyStates exposing (KeyStates)
import Ship
import Collisions exposing (..)
import Hud


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


type Model
    = Uninitialized
    | Title TitleState
    | PreGame PreGameState
    | Game GameState
    | PostGame PostGameState
    | GameOver GameOverState


type alias TitleState =
    { stars : List Star
    , asteroids : List Asteroid
    , randomSeed : Seed
    , stateTime : Float
    }


type alias PreGameState =
    { sector : Int
    , score : Int
    , lives : Int
    , stars : List Star
    , asteroids : List Asteroid
    , bullets : List Bullet
    , segmentParticles : List SegmentParticle
    , randomSeed : Seed
    , stateTime : Float
    }


preGameLength : Float
preGameLength =
    3


type alias GameState =
    { sector : Int
    , score : Int
    , lives : Int
    , stars : List Star
    , player : Player
    , asteroids : List Asteroid
    , bullets : List Bullet
    , segmentParticles : List SegmentParticle
    , keys : KeyStates
    , randomSeed : Seed
    , fireTime : Float
    , stateTime : Float
    }


invinciblePeriod : Float
invinciblePeriod =
    3


type alias PostGameState =
    { sector : Int
    , score : Int
    , lives : Int
    , stars : List Star
    , player : Player
    , bullets : List Bullet
    , segmentParticles : List SegmentParticle
    , keys : KeyStates
    , randomSeed : Seed
    , stateTime : Float
    }


postGameLength : Float
postGameLength =
    5


type alias GameOverState =
    { sector : Int
    , score : Int
    , stars : List Star
    , asteroids : List Asteroid
    , bullets : List Bullet
    , segmentParticles : List SegmentParticle
    , randomSeed : Seed
    , stateTime : Float
    }


init : Model
init =
    Uninitialized


type Msg
    = Init Time
    | Tick Time
    | KeyPressed KeyCode
    | KeyReleased KeyCode


update : Msg -> Model -> Model
update msg model =
    let
        enter =
            13
    in
        case model of
            Uninitialized ->
                case msg of
                    Init time ->
                        let
                            randomSeed =
                                inMilliseconds time |> floor |> initialSeed
                        in
                            Title (initTitle randomSeed)

                    _ ->
                        model

            Title titleState ->
                case msg of
                    Tick timeDelta ->
                        Title (tickTitle (inSeconds timeDelta) titleState)

                    KeyPressed key ->
                        if key == enter then
                            PreGame (initPreGame 1 0 3 titleState.stars titleState.asteroids [] [] titleState.randomSeed)
                        else
                            model

                    _ ->
                        model

            PreGame preGameState ->
                case msg of
                    Tick timeDelta ->
                        tickPreGame (inSeconds timeDelta) preGameState

                    _ ->
                        model

            Game gameState ->
                case msg of
                    Tick timeDelta ->
                        tickGame (inSeconds timeDelta) gameState

                    KeyPressed key ->
                        Game { gameState | keys = KeyStates.pressed key gameState.keys }

                    KeyReleased key ->
                        Game { gameState | keys = KeyStates.released key gameState.keys }

                    _ ->
                        model

            PostGame postGameState ->
                case msg of
                    Tick timeDelta ->
                        tickPostGame (inSeconds timeDelta) postGameState

                    _ ->
                        model

            GameOver gameOverState ->
                case msg of
                    Tick timeDelta ->
                        tickGameOver (inSeconds timeDelta) gameOverState

                    KeyPressed key ->
                        if key == enter then
                            Title (initTitle gameOverState.randomSeed)
                        else
                            model

                    _ ->
                        model


initTitle : Seed -> TitleState
initTitle randomSeed =
    let
        ( ( stars, asteroids ), randomSeed_ ) =
            initStarsAndAsteroids randomSeed
    in
        { stars = stars
        , asteroids = asteroids
        , randomSeed = randomSeed_
        , stateTime = 0
        }


initStarsAndAsteroids : State Seed ( List Star, List Asteroid )
initStarsAndAsteroids =
    Stars.init
        >>= \stars ->
                Asteroids.init
                    >>= \asteroids ->
                            return ( stars, asteroids )


tickTitle : Float -> TitleState -> TitleState
tickTitle timeDelta titleState =
    { titleState
        | stars = Stars.tick timeDelta titleState.stars
        , asteroids = Asteroids.tick timeDelta titleState.asteroids
        , stateTime = titleState.stateTime + timeDelta
    }


initPreGame : Int -> Int -> Int -> List Star -> List Asteroid -> List Bullet -> List SegmentParticle -> Seed -> PreGameState
initPreGame sector score lives stars asteroids bullets segmentParticles randomSeed =
    { sector = sector
    , score = score
    , stars = stars
    , lives = lives
    , asteroids = asteroids
    , bullets = bullets
    , segmentParticles = segmentParticles
    , randomSeed = randomSeed
    , stateTime = 0
    }


tickPreGame : Float -> PreGameState -> Model
tickPreGame timeDelta preGameState =
    let
        stars =
            Stars.tick timeDelta preGameState.stars

        asteroids =
            Asteroids.tick timeDelta preGameState.asteroids

        bullets =
            Bullets.tick timeDelta preGameState.bullets

        ( ( asteroids_, bullets_, segmentParticles, _, _ ), randomSeed ) =
            collide
                Nothing
                asteroids
                bullets
                preGameState.randomSeed

        segmentParticles_ =
            SegmentParticles.tick timeDelta preGameState.segmentParticles ++ segmentParticles
    in
        if preGameState.stateTime >= preGameLength then
            Game
                (initGame
                    preGameState.sector
                    preGameState.score
                    preGameState.lives
                    stars
                    asteroids_
                    bullets_
                    segmentParticles_
                    randomSeed
                )
        else
            PreGame
                { preGameState
                    | stars = stars
                    , asteroids = asteroids_
                    , bullets = bullets_
                    , segmentParticles = segmentParticles_
                    , randomSeed = randomSeed
                    , stateTime = preGameState.stateTime + timeDelta
                }


initGame : Int -> Int -> Int -> List Star -> List Asteroid -> List Bullet -> List SegmentParticle -> Seed -> GameState
initGame sector score lives stars asteroids bullets segmentParticles randomSeed =
    { sector = sector
    , score = score
    , lives = lives
    , stars = stars
    , player =
        { position = ( 0, 0 )
        , velocity = ( 0, 0 )
        , rotation = 0
        }
    , asteroids = asteroids
    , bullets = bullets
    , segmentParticles = segmentParticles
    , keys =
        { left = False
        , right = False
        , up = False
        , down = False
        , space = False
        }
    , randomSeed = randomSeed
    , fireTime = 0
    , stateTime = 0
    }


tickGame : Float -> GameState -> Model
tickGame timeDelta gameState =
    let
        stars =
            Stars.tick timeDelta gameState.stars

        player =
            Player.tick timeDelta gameState.keys gameState.player

        asteroids =
            Asteroids.tick timeDelta gameState.asteroids

        bullets =
            Bullets.tick timeDelta gameState.bullets

        ( bullets_, fireTime ) =
            if gameState.keys.space && gameState.fireTime >= 0 then
                ( Bullets.fire gameState.player bullets, -0.3 )
            else
                ( bullets, gameState.fireTime + timeDelta )

        ( ( asteroids_, bullets__, segmentParticles, score, hitPlayer ), randomSeed ) =
            collide
                (if gameState.stateTime < invinciblePeriod then
                    Nothing
                 else
                    Just player
                )
                asteroids
                bullets_
                gameState.randomSeed

        score_ =
            gameState.score + score

        segmentParticles_ =
            SegmentParticles.tick timeDelta gameState.segmentParticles ++ segmentParticles
    in
        if hitPlayer then
            let
                lives =
                    gameState.lives - 1
            in
                if lives > 0 then
                    PreGame
                        (initPreGame
                            gameState.sector
                            score_
                            lives
                            stars
                            asteroids_
                            bullets__
                            segmentParticles_
                            randomSeed
                        )
                else
                    GameOver
                        (initGameOver
                            gameState.sector
                            score_
                            stars
                            asteroids_
                            bullets__
                            segmentParticles_
                            randomSeed
                        )
        else
            case asteroids_ of
                [] ->
                    PostGame
                        (initPostGame
                            gameState.sector
                            score_
                            gameState.lives
                            stars
                            player
                            bullets__
                            segmentParticles_
                            randomSeed
                        )

                _ ->
                    Game
                        { gameState
                            | score = score_
                            , stars = stars
                            , player = player
                            , asteroids = asteroids_
                            , bullets = bullets__
                            , segmentParticles = segmentParticles_
                            , randomSeed = randomSeed
                            , fireTime = fireTime
                            , stateTime = gameState.stateTime + timeDelta
                        }


initPostGame : Int -> Int -> Int -> List Star -> Player -> List Bullet -> List SegmentParticle -> Seed -> PostGameState
initPostGame sector score lives stars player bullets segmentParticles randomSeed =
    { sector = sector
    , score = score
    , stars = stars
    , lives = lives
    , player = player
    , bullets = bullets
    , segmentParticles = segmentParticles
    , keys =
        { left = False
        , right = False
        , up = False
        , down = False
        , space = False
        }
    , randomSeed = randomSeed
    , stateTime = 0
    }


tickPostGame : Float -> PostGameState -> Model
tickPostGame timeDelta postGameState =
    let
        stars =
            Stars.tick timeDelta postGameState.stars

        player =
            Player.tick timeDelta postGameState.keys postGameState.player

        bullets =
            Bullets.tick timeDelta postGameState.bullets

        segmentParticles =
            SegmentParticles.tick timeDelta postGameState.segmentParticles
    in
        if postGameState.stateTime >= postGameLength then
            let
                ( ( stars_, asteroids ), randomSeed ) =
                    initStarsAndAsteroids postGameState.randomSeed
            in
                PreGame
                    (initPreGame
                        (postGameState.sector + 1)
                        postGameState.score
                        postGameState.lives
                        stars_
                        asteroids
                        []
                        []
                        randomSeed
                    )
        else
            PostGame
                { postGameState
                    | stars = stars
                    , player = player
                    , bullets = bullets
                    , segmentParticles = segmentParticles
                    , stateTime = postGameState.stateTime + timeDelta
                }


initGameOver : Int -> Int -> List Star -> List Asteroid -> List Bullet -> List SegmentParticle -> Seed -> GameOverState
initGameOver sector score stars asteroids bullets segmentParticles randomSeed =
    { sector = sector
    , score = score
    , stars = stars
    , asteroids = asteroids
    , bullets = bullets
    , segmentParticles = segmentParticles
    , randomSeed = randomSeed
    , stateTime = 0
    }


tickGameOver : Float -> GameOverState -> Model
tickGameOver timeDelta gameOverState =
    let
        stars =
            Stars.tick timeDelta gameOverState.stars

        asteroids =
            Asteroids.tick timeDelta gameOverState.asteroids

        bullets =
            Bullets.tick timeDelta gameOverState.bullets

        ( ( asteroids_, bullets_, segmentParticles, _, _ ), randomSeed ) =
            collide
                Nothing
                asteroids
                bullets
                gameOverState.randomSeed

        segmentParticles_ =
            SegmentParticles.tick timeDelta gameOverState.segmentParticles ++ segmentParticles
    in
        GameOver
            { gameOverState
                | stars = stars
                , asteroids = asteroids_
                , bullets = bullets_
                , segmentParticles = segmentParticles_
                , randomSeed = randomSeed
                , stateTime = gameOverState.stateTime + timeDelta
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Uninitialized ->
            times Init

        _ ->
            Sub.batch
                [ diffs Tick
                , downs KeyPressed
                , ups KeyReleased
                ]


view : Model -> Html Msg
view model =
    let
        scene =
            case model of
                Uninitialized ->
                    group []

                Title titleState ->
                    group
                        [ Stars.draw titleState.stars
                        , Asteroids.draw titleState.asteroids
                        , group
                            [ defaultText 40 "elmsteroids" |> moveY 50
                            , defaultText 16 "ferris // 2016" |> moveY -30
                            , defaultText 14 "press enter/return to begin" |> moveY -50
                            ]
                            |> alpha (min titleState.stateTime 1)
                        ]

                PreGame preGameState ->
                    let
                        animAmt =
                            preGameState.stateTime / preGameLength

                        animAmt_ =
                            1 - animAmt
                    in
                        group
                            [ Stars.draw preGameState.stars
                            , Asteroids.draw preGameState.asteroids

                            -- Seems there are rendering bugs when drawing the ship with alpha = 0
                            , Ship.draw ( 0, 0 ) ((animAmt_ ^ 3) * 8) |> scale (1 + (animAmt_ ^ 2) * 2) |> alpha (animAmt |> max 0.00001)
                            , Bullets.draw preGameState.bullets
                            , SegmentParticles.draw preGameState.segmentParticles
                            , group
                                [ defaultText 26 ("warping to sector " ++ toString preGameState.sector) |> moveY 50
                                , defaultText 18 ("score: " ++ toString preGameState.score ++ " // " ++ Hud.livesText preGameState.lives) |> moveY -30
                                ]
                                |> alpha (min preGameState.stateTime (preGameLength - preGameState.stateTime |> min 1 |> max 0))
                                |> scale (1 + (animAmt_ * 0.2))
                            ]

                Game gameState ->
                    group
                        [ Stars.draw gameState.stars
                        , Asteroids.draw gameState.asteroids
                        , let
                            a =
                                if gameState.stateTime < invinciblePeriod then
                                    cos (gameState.stateTime * 50) * 0.4 + 0.6
                                else
                                    1
                          in
                            Player.draw gameState.player |> alpha a
                        , Bullets.draw gameState.bullets
                        , SegmentParticles.draw gameState.segmentParticles
                        , Hud.draw gameState.sector gameState.score gameState.lives |> alpha (min gameState.stateTime 1)
                        ]

                PostGame postGameState ->
                    group
                        [ Stars.draw postGameState.stars
                        , Player.draw postGameState.player
                        , Bullets.draw postGameState.bullets
                        , SegmentParticles.draw postGameState.segmentParticles
                        , group
                            [ defaultText 26 ("sector " ++ toString postGameState.sector ++ " cleared") |> moveY 50
                            , defaultText 18 ("score: " ++ toString postGameState.score ++ " // " ++ Hud.livesText postGameState.lives) |> moveY -30
                            ]
                            |> alpha (min postGameState.stateTime 1)
                        ]

                GameOver gameOverState ->
                    group
                        [ Stars.draw gameOverState.stars
                        , Asteroids.draw gameOverState.asteroids
                        , Bullets.draw gameOverState.bullets
                        , SegmentParticles.draw gameOverState.segmentParticles
                        , group
                            [ defaultText 36 "GAME OVER" |> moveY 30
                            , defaultText 18 ("sector " ++ toString gameOverState.sector ++ " // score: " ++ toString gameOverState.score) |> moveY -30
                            ]
                            |> alpha (min gameOverState.stateTime 1)
                        ]
    in
        collage
            (floor width)
            (floor height)
            [ rect width height |> filled black
            , scene
            ]
            |> Element.toHtml
