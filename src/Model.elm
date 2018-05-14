module Model exposing (..)

import Messages exposing(..)

import Window
import Task

type alias Model = 
  { player : GameObj
  , minion : GameObj
  , minion_max : Int
  , player_bullets : List Bullet
  , minions : List GameObj
  , minion_bullets : List Bullet
  , window : Vector
  , grid : Grid
  , unit : Float
  , dt : Float
  , fire : Bool
  , paused : Bool
  , rand_rad : Float
  }

init : (Model, Cmd Msg)
init =
  ( { player = { init_gameobj | bullet_inertia_ratio=1, v_max=50, cd_max=0.1} 
    , minion = init_gameobj
    , minion_max = 3
    , minions = []
    , player_bullets = []
    , minion_bullets = []
    , window = (0,0)
    , grid = ([],[])
    , unit = 0
    , dt = 0
    , fire = False
    , paused = True
    , rand_rad = 0
    }
  , Task.perform WindowResize Window.size)
  
type alias GameObj = 
  { pos : Vector
  , v : Vector
  , a : Vector
  , angle : Float
  , cd : Float
  , r : Float 
  , hp : Float
  , v_max : Float
  , a_max : Float
  , cd_max : Float
  , bullet_r : Float
  , bullet_v : Float
  , bullet_max : Int
  , bullet_lifespan : Float
  , bullet_spread_ratio : Float
  , bullet_inertia_ratio : Float
  , bullet_invincibility : Float
  , invincibility : Float
  }
init_gameobj : GameObj
init_gameobj = 
  { pos = (0,0)
  , v = (0,0)
  , a = (0,0)
  , r = 3
  , hp = 9
  , angle = 0
  , cd = 0
  , v_max = 30
  , a_max = 100
  , cd_max = 0.5
  , bullet_r = 1.5
  , bullet_v = 50
  , bullet_max = 100
  , bullet_lifespan = 5
  , bullet_spread_ratio = 0
  , bullet_inertia_ratio = 0
  , bullet_invincibility = 0
  , invincibility = 0
  }

type alias Bullet = 
  { pos : Vector
  , v : Vector
  , r : Float
  , lifespan : Float
  , hp : Float
  , invincibility : Float
  }

type alias Vector = 
  (Float, Float)

type alias Grid =
  (List Float, List Float)