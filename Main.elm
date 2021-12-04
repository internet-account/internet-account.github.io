module Main exposing (main)

import Browser
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }

type alias Model = Rubik
type alias Msg = Move

init : () -> (Model, Cmd Msg)
init _ =
    ( { top = Side White White White White White White White White White
      , back = Side Red Red Red Red Red Red Red Red Red
      , right = Side Blue Blue Blue Blue Blue Blue Blue Blue Blue
      , front = Side Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow Yellow
      , left = Side Green Green Green Green Green Green Green Green Green
      , bottom = Side Black Black Black Black Black Black Black Black Black
      }
    , Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    ( turn msg model, Cmd.none )

view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ div [ id "grid" ]
              ([ div
                    [ id "ccw-button"
                    , class "button"
                    ]
                    [ button [ onClick CCW ] [ text "CCW" ] ]
              , div
                    [ id "cw-button"
                    , class "button"
                    ]
                    [ button [ onClick CW ] [ text "CW" ] ]
              ] ++ viewRubik model)
        ]

viewRubik : Rubik -> List (Html Msg)
viewRubik r =
    List.map viewSide [ (r.top, "top", Just Up)
                      , (r.left, "left", Just Left)
                      , (r.front, "front", Nothing)
                      , (r.right, "right", Just Right)
                      , (r.bottom, "bottom", Just Down)
                      , (r.back |> rotateSide SideCW |> rotateSide SideCW, "back", Nothing)
                      ]

viewSide : (Side, String, Maybe Msg) -> Html Msg
viewSide (s, t, mmsg) =
    div
        ([ id t
        , class "side"
        ] ++ case mmsg of
                 Just msg -> [ onClick msg ]
                 Nothing -> []
        )
        [ div [ class <| toString s.topleft ] []
        , div [ class <| toString s.top ] []
        , div [ class <| toString s.topright ] []
        , div [ class <| toString s.left ] []
        , div [ class <| toString s.center ] []
        , div [ class <| toString s.right ] []
        , div [ class <| toString s.bottomleft ] []
        , div [ class <| toString s.bottom ] []
        , div [ class <| toString s.bottomright ] []
        ]

toString : Color -> String
toString color =
    case color of
        White -> "white"
        Red -> "red"
        Blue -> "blue"
        Yellow -> "yellow"
        Green -> "green"
        Black -> "black"
        
type alias Rubik =
    { top : Side
    , back : Side
    , right : Side
    , front : Side
    , left : Side
    , bottom : Side
    }

type alias Side =
    { center : Color
    , topleft : Color
    , top : Color
    , topright : Color
    , right : Color
    , bottomright : Color
    , bottom : Color
    , bottomleft : Color
    , left : Color
    }

type Color
    = White
    | Red
    | Blue
    | Yellow
    | Green
    | Black

type Move
    = Left
    | Right
    | Up
    | Down
    | CW
    | CCW

type SideRotation
    = SideCW
    | SideCCW

turn : Move -> Rubik -> Rubik
turn move {top, back, right, front, left, bottom} =
    case move of
        Left -> { top = top |> rotateSide SideCW
                , back = left
                , right = back
                , front = right
                , left = front
                , bottom = bottom |> rotateSide SideCCW
                }

        Right -> { top = top |> rotateSide SideCCW
                 , back = right
                 , right = front
                 , front = left
                 , left = back
                 , bottom = bottom |> rotateSide SideCW
                 }

        Up -> { top = front
              , back = top |> rotateSide SideCW |> rotateSide SideCW
              , right = right |> rotateSide SideCW
              , front = bottom
              , left = left |> rotateSide SideCCW
              , bottom = back |> rotateSide SideCW |> rotateSide SideCW
              }

        Down -> { top = back |> rotateSide SideCW |> rotateSide SideCW
                , back = bottom |> rotateSide SideCW |> rotateSide SideCW
                , right = right |> rotateSide SideCCW
                , front = top
                , left = left |> rotateSide SideCW
                , bottom = front
                }

        CW -> { top = { top | bottomleft = left.bottomright
                            , bottom = left.right
                            , bottomright = left.topright
                      }
              , back = back
              , right = { right | topleft = top.bottomleft
                                , left = top.bottom
                                , bottomleft = top.bottomright
                        }
              , front = front |> rotateSide SideCW
              , left = { left | topright = bottom.topleft
                              , right = bottom.top
                              , bottomright = bottom.topright
                       }
              , bottom = { bottom | topleft = right.bottomleft
                                  , top = right.left
                                  , topright = right.topleft
                         }
              }

        CCW -> { top = { top | bottomleft = right.topleft
                             , bottom = right.left
                             , bottomright = right.bottomleft
                       }
               , back = back
               , right = { right | topleft = bottom.topright
                                 , left = bottom.top
                                 , bottomleft = bottom.topleft
                         }
               , front = front |> rotateSide SideCCW
               , left = { left | topright = top.bottomright
                               , right = top.bottom
                               , bottomright = top.bottomleft
                        }
               , bottom = { bottom | topleft = left.topright
                                   , top = left.right
                                   , topright = left.bottomright
                          }
               }

rotateSide : SideRotation -> Side -> Side
rotateSide rotation s =
    case rotation of
        SideCW -> { center = s.center
                  , topleft = s.bottomleft
                  , top = s.left
                  , topright = s.topleft
                  , right = s.top
                  , bottomright = s.topright
                  , bottom = s.right
                  , bottomleft = s.bottomright
                  , left = s.bottom
                  }

        SideCCW -> { center = s.center
                   , topleft = s.topright
                   , top = s.right
                   , topright = s.bottomright
                   , right = s.bottom
                   , bottomright = s.bottomleft
                   , bottom = s.left
                   , bottomleft = s.topleft
                   , left = s.top
                   }
