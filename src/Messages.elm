module Messages exposing (..)

import Time
import Window
import Keyboard
import Mouse

type Msg
  = WindowResize Window.Size
  | GameUpdate Time.Time
  | KeyUp Keyboard.KeyCode
  | KeyDown Keyboard.KeyCode
  | MouseMove Mouse.Position
  | MouseDown Mouse.Position
  | MouseUp Mouse.Position
  | RandomRadian Float

  | PlayerAMax String
  | PlayerR String
  | PlayerVMax String
  | PlayerCDMax String
  | PlayerBulletR String
  | PlayerBulletV String
  | PlayerBulletMax String
  | PlayerBulletLifespan String
  | PlayerBulletSpreadRatio String
  | PlayerBulletInertiaRatio String
  | PlayerBulletInvincibility String
  | PlayerInvincibility String
  
  | MinionMax String
  | MinionR String
  | MinionVMax String
  | MinionCDMax String
  | MinionBulletR String
  | MinionBulletV String
  | MinionBulletMax String
  | MinionBulletLifespan String
  | MinionBulletSpreadRatio String
  | MinionBulletInertiaRatio String
  | MinionBulletInvincibility String
  | MinionInvincibility String

