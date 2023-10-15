import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Operations
import System.Exit (exitWith, ExitCode(ExitSuccess))

main = xmonad $ def
  { terminal    = "kitty"
  , modMask     = mod4Mask
  , borderWidth = 3
  , focusedBorderColor = "#CAD3F5"
  , normalBorderColor = "#25273A"
  } `additionalKeysP`
  [ ("M-S-q", io (exitWith ExitSuccess))
  , ("M-d", spawn "rofi -show drun")
  , ("M-w", kill)
  , ("C-M1-l", spawn "i3lock-fancy-rapid 5 5")
  ]
