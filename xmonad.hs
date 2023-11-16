{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

import Graphics.X11.ExtraTypes.XF86
import System.Exit (exitSuccess)
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.DwmPromote (dwmpromote)
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll (killAll)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Hooks.WindowSwallowing
import XMonad.Layout.Accordion
import XMonad.Layout.CenteredMaster (centerMaster)
import XMonad.Layout.HintedGrid
import XMonad.Layout.LayoutHints
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL, NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Prelude
import XMonad.StackSet qualified as W
import XMonad.Util.ClickableWorkspaces (clickablePP)
import XMonad.Util.EZConfig
import XMonad.Util.Hacks
import XMonad.Util.Loggers (logTitles)
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab

myLayout = smartBorders . avoidStruts . spacer . mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6 / 7) ||| Grid False ||| threeCols ||| Accordion ||| Full
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes
    threeCols = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    spacer = spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True

myLayoutPrinter :: String -> String
myLayoutPrinter "Full" = "<icon=full.xbm/>"
myLayoutPrinter "Tall" = "<icon=tall.xbm/>"
myLayoutPrinter "Mirror Tall" = "<icon=mtall.xbm/>"
myLayoutPrinter "Spiral" = "<icon=spiral.xbm/>"
myLayoutPrinter "Magnifier NoMaster ThreeCol" = "<icon=threeCol.xbm/>"
myLayoutPrinter "Accordion" = "<icon=accordion.xbm/>"
myLayoutPrinter "Grid False" = "<icon=grid.xbm/>"
myLayoutPrinter "Grid" = "<icon=grid.xbm/>"
myLayoutPrinter x
  | "Spacing" `isPrefixOf` x = myLayoutPrinter $ stripPrefix "Spacing " x
  | otherwise = x
  where
    stripPrefix :: String -> String -> String
    stripPrefix [] s = s
    stripPrefix _ [] = []
    stripPrefix (p : ps) (s : ss) = if p == s then stripPrefix ps ss else s : ss

myHandleEventHook =
  composeAll
    [ handleEventHook def,
      swallowEventHook (className =? "kitty" <||> className =? "Alacritty") (return True),
      trayerAboveXmobarEventHook,
      trayerPaddingXmobarEventHook
    ]

scratchpads = [NS "scratchpad" "kitty --name scratchpad --class scratchpad" (className =? "scratchpad") defaultFloating]

myManageHook =
  composeAll
    [ manageHook def,
      insertPosition Below Newer,
      isDialog --> doCenterFloat,
      isFullscreen --> doFullFloat,
      -- manageDocks,
      (className =? "leagueclientux.exe") --> doCenterFloat,
      (className =? "riotclientux.exe") --> doCenterFloat,
      (className =? "Pavucontrol") --> doCenterFloat,
      -- (className =? "league of legends.exe") --> doFullFloat,
      namedScratchpadManageHook scratchpads
    ]

myStartupHook = restoreBackground -- <> trayer
  where
    restoreBackground = spawnOnce "~/.fehbg"

-- trayer =
--   spawnOnce $
--     "trayer --edge top --align right --widthtype request --expand true --SetDockType true --SetPartialStrut true --monitor primary --height "
--       ++ show trayerHeight
--       ++ " --transparent true --alpha 0 --tint 0x25273A --padding 1 --distance 1 --distancefrom right"
--   where
--     trayerHeight = 40

-- https://github.com/xmonad/xmonad/blob/master/src/XMonad/Config.hs
myKeymap =
  [ ("M-S-q", io exitSuccess),
    ("M-d", spawn "rofi -show"),
    ("M-w", kill1),
    ("M-S-w", kill),
    ("M-C-S-w", killAll),
    ("C-M1-l", spawn "i3lock-fancy-rapid 5 5"),
    -- ("M-b", sendMessage ToggleStruts),
    ("M-f", sendMessage $ Toggle NBFULL),
    ("M-<Return>", spawn term),
    ("M-S-<Return>", dwmpromote),
    ("M-C-t", withFocused $ windows . W.sink),
    ("M-s", namedScratchpadAction scratchpads "scratchpad"),
    ("M-n", runOrRaiseNext (term ++ " nvim") (isSuffixOf "NVIM" <$> title <||> isSuffixOf "- NVIM\" " <$> title)),
    ("M-b", runOrRaiseNext "firefox" (className =? "firefox")),
    ("M-v", runOrRaiseNext term (className =? term)),
    ("M-u", focusUrgent),
    -- Volume
    ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 5%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 5%+"),
    ("<XF86AudioMute>", spawn "amixer -q sset Master toggle"),
    -- Brightness
    ("<XF86MonBrightnessUp>", spawn "light -A 1"),
    ("<XF86MonBrightnessDown>", spawn "light -U 1"),
    -- Media
    ("<XF86AudioPlay>", spawn "playerctl play-pause"),
    ("<XF86AudioNext>", spawn "playerctl next"),
    ("<XF86AudioPrev>", spawn "playerctl previous"),
    ("<XF86AudioStop>", spawn "playerctl stop"),
    ("M-<KP_5>", spawn "hass-cli state toggle light.desk_lamp")
  ]
    -- Move to other screens
    ++ [ ("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
         | (k, sc) <- zip ["e", "r", "t"] [0 ..],
           (f, m) <- [(W.view, ""), (W.shift, "S-")]
       ]
    -- Copy client to other workspaces
    ++ [ ("M-C-S-" ++ ws, windows $ copy ws)
         | ws <- map show [1 .. 9]
       ]
  where
    term = XMonad.terminal myConfig

myWorkspaces = ["web", "code"] ++ map show [3 .. 9]

myLogHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ def

-- myXmobarPP :: PP
myXmobarPP = do
  copies <- wsContainingCopies
  let check ws
        | ws `elem` copies = pad . xmobarColor "red" "black" $ ws
        | otherwise = white . pad $ ws

  clickablePP . filterOutWsPP [scratchpadWorkspaceTag] $
    def
      { ppSep = magenta " | ",
        ppTitleSanitize = xmobarStrip,
        ppCurrent = pad . xmobarBorder "Top" "#8BD5CA" 2,
        ppVisible = wrap "(" ")",
        ppHidden = check,
        ppHiddenNoWindows = lowWhite . pad,
        ppLayout = white . myLayoutPrinter,
        ppUrgent = red . wrap (yellow "!") (yellow "!"),
        ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
        ppExtras = [logTitles formatFocused formatUnfocused]
      }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#C6A0F6" ""
    blue = xmobarColor "#8AADF4" ""
    white = xmobarColor "#CAD3F5" ""
    yellow = xmobarColor "#EED49F" ""
    red = xmobarColor "#ED8796" ""
    lowWhite = xmobarColor "#5B6078" ""

-- isFullscreen --> doFullFloat
-- color0 = "#25273A"; # base
-- color1 = "#1E2030"; # mantle
-- color2 = "#363A4F"; # surface0
-- color3 = "#494D64"; # surface1
-- color4 = "#5B6078"; # surface2
-- color5 = "#CAD3F5"; # text
-- color6 = "#F4DBD6"; # rosewater
-- color7 = "#B7BDF8"; # lavender
-- color8 = "#ED8796"; # red
-- color9 = "#F5A97F"; # peach
-- color10 = "#EED49F"; # yellow
-- color11 = "#A6DA95"; # green
-- color12 = "#8BD5CA"; # teal
-- color13 = "#8AADF4"; # blue
-- color14 = "#C6A0F6"; # mauve
-- color15 = "#F0C6C6"; # flamingo

-- For some reason, the league clients do not work with ewmh fullscreen,
-- despite being floating, they get back into tiling mode when some action
-- if performed. By skipping ewmh fullscreen for these clients, we can
-- avoid this issue:
myEwmhFullscreen :: XConfig a -> XConfig a
myEwmhFullscreen c =
  c
    { startupHook = startupHook c <+> fullscreenStartup,
      handleEventHook = handleEventHook c <+> fullscreenEventHookNoLeagueClient
    }
  where
    fullscreenEventHookNoLeagueClient :: Event -> X All
    fullscreenEventHookNoLeagueClient ev@(ClientMessageEvent {ev_event_display = dpy, ev_window = win}) =
      liftIO ((`elem` ["leagueclientux.exe", "riotclientux.exe"]) . resClass <$> getClassHint dpy win) >>= \case
        True -> return $ All True
        False -> fullscreenEventHook ev

main =
  xmonad
    . docks
    . myEwmhFullscreen
    . ewmh
    . withSB (statusBarProp "xmobar" myXmobarPP)
    $ myConfig

myConfig =
  def
    { terminal = "kitty",
      modMask = mod4Mask,
      borderWidth = 3,
      focusedBorderColor = "#CAD3F5",
      normalBorderColor = "#25273A",
      layoutHook = myLayout,
      handleEventHook = myHandleEventHook,
      -- logHook = dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ myXmobarPP,
      manageHook = myManageHook,
      startupHook = do
        return () >> checkKeymap myConfig myKeymap -- WARN: return needed to avoid infinite recursion
        myStartupHook,
      workspaces = myWorkspaces
    }
    `additionalKeysP` myKeymap
