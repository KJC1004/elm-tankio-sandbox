module View exposing (..)

import Messages exposing(..)
import Model exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Collage exposing (..)
import Color
import Element

view : Model -> Html Msg
view model =
  if model.paused then
    pause_view model
  else
    let
      (w, h) = model.window

      group_list =
        (draw_background model) ++
        (draw_grid model) ++
        (draw_bullets model Color.yellow  model.minion_bullets) ++ 
        (draw_bullets model Color.lightBlue model.player_bullets) ++
        (draw_minions model) ++
        (draw_player model)
    in
      Element.toHtml 
        ( collage (floor w) (floor h) group_list )


draw_bullet : Float -> LineStyle -> Color.Color -> Bullet -> Form
draw_bullet unit myline mycolor bullet =
  let
    (x,y) = bullet.pos
    hp = bullet.hp
    r = bullet.r
    r_u = r*unit
  in
    [ circle r_u
        |> filled mycolor
    , circle r_u
        |> filled Color.black
        |> alpha (1 - hp/r^2)
    , circle r_u
        |> outlined myline
    ] |> group 
      |> move (x*unit,y*unit) 
draw_bullets : Model -> Color.Color -> List Bullet -> List Form
draw_bullets model mycolor bullets = 
  let
    unit = model.unit 
    (w,h) = model.window
    window = (w/unit, h/unit)

    blackline = 
      (solid Color.black)
        |>  (\n -> {n | width=0.3*unit}) 
  in
    bullets
      |> List.filter (is_bullet_in_window window)
      |> List.map (draw_bullet unit blackline mycolor)
is_bullet_in_window : Vector -> Bullet -> Bool
is_bullet_in_window (w,h) {pos,r} =
  let
    (x,y) = pos
    bound_x = r+w/2
    bound_y = r+h/2
  in 
    x < bound_x && x > -bound_x && 
    y < bound_y && y > -bound_y

draw_background : Model -> List Form
draw_background {window} = 
  let 
    (w, h) = window
  in
    [ rect w h 
        |> filled Color.white
    ]

draw_grid : Model -> List Form
draw_grid {window,grid} =
  let
    (w, h) = window
    (hw,hh, nhw, nhh) = (w/2, h/2, -w/2, -h/2)
    (row,col) = grid
    row_view =  
      row 
        |> List.map ( \n -> n - hh )
        |> List.map ( \n -> segment (nhw,n) (hw,n) )
        |> List.map ( traced (solid Color.darkGrey) )

    col_view = 
      col 
        |> List.map ( \n -> n - hw )
        |> List.map ( \n -> segment (n,nhh) (n,hh) )
        |> List.map ( traced (solid Color.darkGrey) )
  in
    row_view ++ col_view

draw_player : Model -> List Form
draw_player {unit, player} = 
  let
    r = player.r
    r_u = r*unit
    hp = player.hp
    cd = player.cd
    cd_max = player.cd_max

    my_linestyle = 
      (solid Color.black)
        |> (\n -> {n | width=0.3*unit}) 

    player_view = 
      [ rect r_u (r_u*1.5)
          |> filled Color.darkGrey
          |> moveY r_u
      , rect r_u (r_u*1.5)
          |> outlined my_linestyle
          |> moveY r_u
      , rect r_u (r_u*1.5)
          |> filled Color.red
          |> moveY r_u
          |> alpha (cd/cd_max)
      , circle r_u
          |> filled Color.lightBlue
      , circle r_u 
          |> filled Color.black
          |> alpha (1 - hp/r^2)
      , circle r_u
          |> outlined my_linestyle
      ] |> group
        |> rotate (radians -player.angle)
  in
    [player_view]

draw_minion : Float -> GameObj -> Form
draw_minion unit minion = 
  let
    r = minion.r
    r_u = r*unit
    (x,y) = minion.pos
    cd = minion.cd
    cd_max = minion.cd_max
    hp = minion.hp
  
    my_linestyle = 
      (solid Color.black)
        |> (\n -> {n | width=0.3*unit}) 
  in
    [ rect r_u (r_u*1.5)
        |> filled Color.darkGrey
        |> moveY r_u
    , rect r_u (r_u*1.5)
        |> outlined my_linestyle
        |> moveY r_u
    , rect r_u (r_u*1.5)
        |> filled Color.red
        |> moveY r_u
        |> alpha (cd/cd_max)
    , circle r_u
        |> filled Color.yellow
    , circle r_u 
        |> filled Color.black
        |> alpha (1 - hp/r^2)
    , circle r_u
        |> outlined my_linestyle
    ] |> group
      |> rotate (radians -minion.angle)
      |> move (x*unit, y*unit)
draw_minions : Model -> List Form
draw_minions model =
  let
    minions = model.minions
    unit = model.unit
  in
    minions
      |> List.map (draw_minion unit)

tr_input : Model -> String -> (GameObj -> a) -> (String -> Msg) -> (String -> Msg) -> Html Msg
tr_input model str func msg0 msg1 = 
  let 
    player = model.player
    minion = model.minion
  in
    tr [] 
      [ td [] [input [onInput msg0] []]
      , td [] [player |> func |> toString |> Html.text]
      , td [] [Html.text str]
      , td [] [minion |> func |> toString |> Html.text]
      , td [] [input [onInput msg1] []]
      ]
tableStyles : List (Attribute msg)
tableStyles = 
  [ align "center"
  , style
      [ ("width", "50%")
      , ("height", "100%") 
      , ("text-align", "center")
      , ("backgroundColor", "lightgrey")
      ]
  ]
pause_view : Model -> Html Msg
pause_view model = 
  let 
    player = model.player
    minion = model.minion
  in 
    table tableStyles 
    [ thead [] 
      [ tr [] [ th [colspan 5] [Html.text <| "TANK.IO SANDBOX"] ] 
      , tr []
        [ th [] []
        , th [] [Html.text "PLAYER"]
        , th [] []
        , th [] [Html.text "MINION"]
        , th [] []
        ]
      ]
    , tbody []
      [ tr [] 
        [ td [] [input [onInput PlayerAMax] []]
        , td [] [Html.text <| toString <| player.a_max]
        , td [] [Html.text "Acceleration | Number"]
        , td [] [Html.text <| toString <| model.minion_max]
        , td [] [input [onInput MinionMax] []]
        ]
      , tr_input model "Radius" .r PlayerR MinionR  
      , tr_input model "Velocity" .v_max PlayerVMax MinionVMax
      , tr_input model "Cooldown (sec)" .cd_max PlayerCDMax MinionCDMax
      , tr_input model "Bullet radius" .bullet_r PlayerBulletR MinionBulletR
      , tr_input model "Bullet velocity" .bullet_v PlayerBulletV MinionBulletV
      , tr_input model "Bullet Num" .bullet_max PlayerBulletMax MinionBulletMax
      , tr_input model "Bullet lifespan (sec)" .bullet_lifespan PlayerBulletLifespan MinionBulletLifespan
      , tr_input model "Bullet Spread Ratio (0~1)" .bullet_spread_ratio PlayerBulletSpreadRatio MinionBulletSpreadRatio
      , tr_input model "Bullet Inertia Ratio " .bullet_inertia_ratio PlayerBulletInertiaRatio MinionBulletInertiaRatio
      , tr_input model "Bullet Invincibility (0~1)" .bullet_invincibility PlayerBulletInvincibility MinionBulletInvincibility
      , tr_input model "Invincibility (0~1)" .invincibility PlayerInvincibility MinionInvincibility
      , tr [] []
      , tr [] 
        [ td [colspan 2] [Html.text <| "W,A,S,D(move)"]
        , td [colspan 1] [Html.text <| "MouseMove (aim)"]
        , td [colspan 2] [Html.text <| "MousePress (fire)"]
        ]
      , tr [] 
        [ td [colspan 5] [Html.text <| "Press P to switch between window"]
        ] 
      ] 
    ]
