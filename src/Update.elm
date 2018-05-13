module Update exposing (..)

import Messages exposing (..)
import Model exposing (..)
import Messages exposing(..)

import Char
import String
import Random

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    PlayerR str -> 
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> 
              let v=max 1 val in
                {player | r = v, hp = v^2}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerVMax str -> 
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | v_max = max 0.001 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerAMax str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | a_max = max 0.001 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerCDMax str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | cd_max = max 0.001 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletR str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | bullet_r = max 0 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletV str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | bullet_v= max 0.001 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletMax str ->
      let 
        player = model.player
        new_player =
          case String.toInt str of
            Ok val -> {player | bullet_max = max 0 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletLifespan str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | bullet_lifespan = max 0 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletSpreadRatio str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | bullet_spread_ratio = max 0 <| min 1 val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletInertiaRatio str ->
      let 
        player = model.player
        new_player =
          case String.toFloat str of
            Ok val -> {player | bullet_inertia_ratio=val}
            Err error -> player
      in
        ({model | player=new_player}, Cmd.none)
    PlayerBulletPenetrate str ->
      let 
        player = model.player
        new_player = {player | bullet_penetrate =  not player.bullet_penetrate}
      in
        ({model | player=new_player}, Cmd.none)
    PlayerInvincible str ->
      let 
        player = model.player
        new_player = {player | invincible = not player.invincible}
      in
        ({model | player=new_player}, Cmd.none)
    
    MinionMax str -> 
      case String.toInt str of
        Ok val -> ({model | minion_max = max 0 val}, Cmd.none)
        Err error -> (model, Cmd.none)
    MinionR str -> 
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> 
              let v = max 1 val in
                {minion | r = v, hp = v^2}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionVMax str -> 
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | v_max= max 0.001 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionCDMax str ->
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | cd_max = max 0.001 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletR str ->
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | bullet_r = max 0 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletV str ->
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | bullet_v = max 0.001 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletMax str ->
      let 
        minion = model.minion
        new_minion =
          case String.toInt str of
            Ok val -> {minion | bullet_max = max 0 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletLifespan str ->
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | bullet_lifespan = max 0 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletSpreadRatio str ->
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | bullet_spread_ratio = max 0 <| min 1 val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletInertiaRatio str ->
      let 
        minion = model.minion
        new_minion =
          case String.toFloat str of
            Ok val -> {minion | bullet_inertia_ratio = val}
            Err error -> minion
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionBulletPenetrate str ->
      let 
        minion = model.minion
        new_minion = {minion | bullet_penetrate =  not minion.bullet_penetrate}
      in
        ({model | minion=new_minion}, Cmd.none)
    MinionInvincible str ->
      let 
        minion = model.minion
        new_minion = {minion | invincible = not minion.invincible}
      in
        ({model | minion=new_minion}, Cmd.none)
    
    
    MouseDown mouse ->
      ({model | fire=True}, Cmd.none)
    
    MouseUp mouse ->
      ({model | fire=False}, Cmd.none)

    MouseMove mouse ->
      let 
        (w,h) = model.window
        player = model.player
        pos = (toFloat mouse.x - w/2, toFloat mouse.y - h/2)
        new_player = { player | pos=pos }
      in
        ( {model | player=new_player}, Cmd.none)

    KeyDown code ->
      let
        player = model.player
        (ax, ay) = player.a
        a_max = player.a_max
        ch = Char.fromCode code
        new_a = 
          case ch of
            'W' -> (ax, if ay==0 then  a_max else ay)
            'S' -> (ax, if ay==0 then -a_max else ay)
            'D' -> (if ax==0 then  a_max else ax, ay)
            'A' -> (if ax==0 then -a_max else ax, ay)
            _ -> (ax, ay)

        new_paused = 
          case ch of
            'P' -> not model.paused
            _ -> model.paused  

        new_player = {player | a=new_a}
      in
        ( if new_paused then 
            { model 
                | minions = []
                , player_bullets = []
                , minion_bullets = []
                , paused = new_paused
            }
          else 
            { model 
                | player=new_player
                , paused=new_paused
            }
        , Cmd.none
        )

    KeyUp code ->
      let 
        player = model.player
        (ax, ay) = player.a
        new_a = 
          case (Char.fromCode code) of
            'W' -> (ax, if ay>0 then 0 else ay)
            'S' -> (ax, if ay<0 then 0 else ay)
            'D' -> (if ax>0 then 0 else ax, ay)
            'A' -> (if ax<0 then 0 else ax, ay)
            _ -> (ax, ay)
        new_player = {player | a=new_a}
      in
        ( {model | player=new_player}, Cmd.none)

    WindowResize size ->
      let 
        (w, h) = (toFloat size.width, toFloat size.height)
        switch = if h < w then True else False 
        ratio = if switch then (w / h) else (h / w)
        seg_h = h / if     switch then 10 else ((10 * ratio) |> floor |> toFloat)
        seg_w = w / if not switch then 10 else ((10 * ratio) |> floor |> toFloat)
        row = List.map (\n -> toFloat n * seg_h) (List.range 0 (floor (h/seg_h) - 1))
        col = List.map (\n -> toFloat n * seg_w) (List.range 0 (floor (w/seg_w) - 1))

        unit = (if switch then h else w) / 100
      in
        ( { model | window=(w,h), grid=(row,col), unit=unit}, Cmd.none)

    GameUpdate dt ->
      let
        new_dt = dt/1000

        player = model.player
        minion = model.minion

        (temp_player,offset) = update_player model
        temp_player_bullets = update_player_bullets offset model
        temp_minion_bullets = update_minion_bullets offset model
        temp_minions = update_minions offset model 

        new_player_bullets = 
          if player.bullet_penetrate then
            temp_player_bullets
          else
            temp_player_bullets
              |> List.map (handle_bullet_collision temp_minion_bullets temp_minions)
              |> List.filter(\{lifespan,hp} -> lifespan>0 && hp>0)

        new_minion_bullets =   
          if minion.bullet_penetrate then
            temp_minion_bullets
          else 
            temp_minion_bullets 
              |> List.map (handle_bullet_collision temp_player_bullets [{player | pos=(0,0)}])
              |> List.filter (\{lifespan,hp} -> lifespan>0 && hp > 0)

        new_minions = 
          if minion.invincible then 
            temp_minions
          else 
            temp_minions
              |> List.map (handle_object_collision temp_player_bullets [{player | pos=(0,0)}])
              |> List.filter (\{hp} -> hp>0)

        new_player = 
          if player.invincible then
            temp_player
          else 
            {temp_player | pos=(0,0)}
              |>  handle_object_collision temp_minion_bullets temp_minions
              |> (\n -> {n | pos = player.pos})


        new_paused = 
          new_player |> (\{hp} -> hp <= 0)

        new_grid = 
          update_grid model offset

      in
        if new_paused then 
          ( { model 
                | player = {new_player | hp=new_player.r^2}
                , minions = []
                , player_bullets = []
                , minion_bullets = []
                , paused = new_paused
            }
          , Cmd.none
          )
        else
          ( { model 
                | grid=new_grid
                , player=new_player
                , player_bullets = new_player_bullets
                , minions = new_minions
                , minion_bullets = new_minion_bullets
                , dt=new_dt
            } 
          , Random.generate RandomRadian (Random.float -pi pi)
          )

    RandomRadian rad ->
      ( {model | rand_rad=rad} , Cmd.none)
 
update_grid : Model -> Vector -> Grid
update_grid model (dx,dy) =
  let
    (w,h) = model.window
    (row,col) = model.grid
    unit = model.unit
    (dx_u, dy_u) = (dx*unit, dy*unit)
    new_row = 
      row 
        |> List.map(\n -> n + dy_u)
        |> List.map(\n -> if n>h then n-h else if n<0 then n+h else n)
    new_col = 
      col 
        |> List.map(\n -> n + dx_u)
        |> List.map(\n -> if n>w then n-w else if n<0 then n+w else n)
  in
    (new_row,new_col)

update_player : Model -> (GameObj, Vector)
update_player model = 
  let
    dt = model.dt
    player = model.player
    (ax,ay) = player.a
    (vx,vy) = player.v
    (x,y) = player.pos
    v_max = player.v_max
    a_max = player.a_max
    cd = player.cd
    cd_max = player.cd_max
    fire = model.fire

    new_angle = (atan2 y x) + pi/2 
    
    (dx,dy) = (-vx * dt, -vy * dt)
    decay = 1-dt

    (new_ax,new_ay) = 
      ( max a_max (sqrt (ax^2+ay^2)) ) / a_max
        |> (\n -> (ax/n, ay/n))

    new_v = 
      ( max v_max (sqrt (vx^2+vy^2)) ) / v_max
        |> (\n -> ((vx*decay+ax*dt)/n, (vy*decay+ay*dt)/n))

    temp_cd = cd-dt
    new_cd = 
      if fire && temp_cd<0 
      then cd_max 
      else max temp_cd 0 

  in
    ( {player | v=new_v, angle=new_angle, cd=new_cd}
    , (dx,dy)
    )

update_minions : Vector -> Model -> List GameObj
update_minions offset model =
  let 
    dt = model.dt
  in
    (model.minions ++ create_minions model)
      |> List.map (update_minion dt offset model)
update_minion : Float -> Vector -> Model -> GameObj -> GameObj
update_minion dt (dx,dy) model minion =
  let
    unit = model.unit
    (w,h) = model.window
    (x,y) = minion.pos
    (vx,vy) = minion.v
    v_max = minion.v_max
    cd = minion.cd
    cd_max = minion.cd_max
    angle = minion.angle
    rand_rad = model.rand_rad

    new_angle = -( (atan2 y x) + pi/2 )
    rad = rand_rad / 2
    boundary = sqrt ( (w/2)^2 + (h/2)^2 ) / unit 
    distance = sqrt ( x^2 + y^2 )

    new_v = 
      if distance > boundary then
        (v_max * sin(angle+rad), v_max * cos(angle+rad))
      else
        (vx,vy)

    new_pos =
      ( max distance boundary ) / boundary
        |> (\n -> ((x+vx*dt+dx)/n, (y+vy*dt+dy)/n))
      
    temp_cd = cd-dt
    new_cd = 
      if temp_cd<0 
      then cd_max 
      else temp_cd
  in
    { minion
        | pos = new_pos
        , v = new_v
        , angle = new_angle
        , cd = new_cd
    }
create_minions : Model -> List GameObj
create_minions model = 
  let 
    num = model.minion_max - List.length model.minions
    deg_offset = model.rand_rad
    (w,h) = model.window
    unit = model.unit
    template = model.minion
  
    d = sqrt ((w/2)^2+(h/2)^2) / unit * 1.1
  in
    List.range 1 num
      |> List.map (\n -> (toFloat n-1) / (toFloat num) * 2*pi + deg_offset)
      |> List.map (\n -> (d*cos n, d*sin n))
      |> List.map (\pos -> {template | pos=pos})

update_bullet : Float -> Vector -> Bullet -> Bullet
update_bullet dt (dx,dy) bullet =
  let
    (x,y) = bullet.pos
    (vx,vy) = bullet.v
    lifespan = bullet.lifespan

    new_pos = (x+vx*dt + dx, y+vy*dt + dy)
    new_lifespan = lifespan - dt
  in
    {bullet | pos=new_pos, lifespan=new_lifespan}
create_bullet : Model -> GameObj -> Bullet
create_bullet model obj = 
  let
    (x,y) = obj.pos
    inertia_ratio = obj.bullet_inertia_ratio
    angle = obj.angle + pi/2
    (vx,vy) = obj.v
    bullet_v = obj.bullet_v
    d = obj.r + obj.bullet_r
    spread_ratio = obj.bullet_spread_ratio
    rand_rad = model.rand_rad 

    new_pos =
      ( x - d*cos angle , y + d*sin angle) 

    v_angle = angle + rand_rad * spread_ratio
    new_v = 
      ( (-bullet_v * cos v_angle) + vx * inertia_ratio
      , ( bullet_v * sin v_angle) + vy * inertia_ratio
      )
  in
    { pos=new_pos
    , v=new_v
    , r=obj.bullet_r
    , lifespan=obj.bullet_lifespan
    , hp= obj.bullet_r^2
    }

update_player_bullets : Vector -> Model -> List Bullet
update_player_bullets offset model = 
  let 
    player = model.player
    bullets = model.player_bullets
    bullet_max = player.bullet_max
    dt = model.dt
    minion_bullets = model.minion_bullets

    new_bullet = 
      if player |> (\{cd, cd_max} -> cd==cd_max)
      then [create_bullet model {player | pos=(0,0)}] 
      else []

    restrict_func = 
      ( \n -> 
        if List.length n > bullet_max then
          List.take bullet_max n
        else
          n
      )
  in
    (new_bullet ++ bullets)
      |> restrict_func
      |> List.map (update_bullet dt offset)
      |> List.filter (\{lifespan} -> lifespan>0)
 
update_minion_bullets : Vector -> Model -> List Bullet
update_minion_bullets offset model =
  let 
    minions = model.minions
    bullets = model.minion_bullets
    bullet_max = model.minion.bullet_max
    dt = model.dt
    player_bullets = model.player_bullets

    new_bullets = 
      minions 
        |> List.filter (\{cd, cd_max} -> cd==cd_max)
        |> List.map (\x -> create_bullet model x)

    restrict_func = 
      ( \n -> 
        if List.length n > bullet_max then
          List.take bullet_max n
        else
          n
      )
  in
    (new_bullets ++ bullets)
      |> restrict_func
      |> List.map (update_bullet dt offset)
      |> List.filter (\{lifespan} -> lifespan>0)

is_player_collided : GameObj -> List GameObj -> List Bullet -> Bool
is_player_collided player minions bullets =
  not 
    ( not_collided_o_bs player bullets &&
      not_collided_o_os player minions
    )

distance : Vector -> Vector -> Float
distance (x0,y0) (x1,y1) =
  sqrt ((x0-x1)^2 + (y0-y1)^2)

not_collided_b_bs : Bullet -> List Bullet -> Bool
not_collided_b_bs bullet bullets =
  List.all (not_collided_b_b bullet) bullets
not_collided_b_b : Bullet -> Bullet -> Bool
not_collided_b_b a b =
  distance a.pos b.pos > a.r + b.r

not_collided_o_bs : GameObj -> List Bullet -> Bool
not_collided_o_bs obj bullets =
  List.all (not_collided_o_b obj) bullets
not_collided_o_b : GameObj -> Bullet -> Bool
not_collided_o_b a b =
  distance a.pos b.pos > a.r + b.r

not_collided_b_os : Bullet -> List GameObj -> Bool
not_collided_b_os bullet objs = 
  List.all (not_collided_b_o bullet) objs
not_collided_b_o : Bullet -> GameObj -> Bool
not_collided_b_o a b =
  distance a.pos b.pos > a.r + b.r

not_collided_o_os : GameObj -> List GameObj -> Bool
not_collided_o_os obj objs =
  List.all (not_collided_o_o obj) objs
not_collided_o_o : GameObj -> GameObj -> Bool
not_collided_o_o a b =
  distance a.pos b.pos > a.r + b.r


handle_bullet_collision : List Bullet -> List GameObj -> Bullet -> Bullet 
handle_bullet_collision bullets objs bullet =
  let 
    pos = bullet.pos
    r = bullet.r
    hp_offset =
      ( bullets 
          |> List.filter (\n -> distance n.pos pos < n.r + r)
          |> List.map .hp
          |> List.sum
      ) +
      ( objs 
          |> List.filter (\n -> distance n.pos pos < n.r + r)
          |> List.map .hp
          |> List.sum
      )
  in 
    { bullet | hp = bullet.hp - hp_offset}

handle_object_collision : List Bullet -> List GameObj -> GameObj -> GameObj
handle_object_collision bullets objs obj =
  let
    pos = obj.pos
    r = obj.r
    hp_offset =
      ( bullets 
          |> List.filter (\n -> distance n.pos pos < n.r + r)
          |> List.map .hp
          |> List.sum
      ) +
      ( objs 
          |> List.filter (\n -> distance n.pos pos < n.r + r)
          |> List.map .hp
          |> List.sum
      )
  in 
    { obj | hp = obj.hp - hp_offset}