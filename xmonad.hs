{-# LANGUAGE OverloadedStrings #-}

-- TODO: navigate window layouts with gridselect

-- Base
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Tree
import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad
-- Actions
import XMonad.Actions.CycleWS ()
import XMonad.Actions.DynamicProjects
import XMonad.Actions.GridSelect
import XMonad.Actions.TreeSelect
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory ()
-- Layouts
import XMonad.Layout.Circle (Circle (..))
import XMonad.Layout.Column
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Roledex
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
-- Prompts
import XMonad.Prompt (XPConfig (..))
import XMonad.Prompt.Input
import XMonad.Prompt.Pass
import XMonad.Prompt.Unicode
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

myWorkspaces =
  [ "build",
    "chat",
    "play",
    "music",
    "feed",
    "twitter",
    "work",
    "browse",
    "chaos"
  ]

-- PulseAudio Headset
-- headsetSink = "alsa_input.usb-Logitech_G533_Gaming_Headset-00.mono-fallback"
headsetSink :: String
headsetSink = "alsa_input.pci-0000_0b_00.4.analog-stereo"

-- Scratchpads
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "mixer" spawnMixer findMixer manageMixer,
    NS "clock" spawnClock findClock manageClock,
    NS "twitter" spawnTwitter findTwitter manageTwitter
  ]
  where
    spawnMixer = "urxvt -title Pulsemixer -name Pulsemixer -e pulsemixer" -- launch pulsemixer
    findMixer = resource =? "Pulsemixer" -- its window will be named "pulsemixer"
    manageMixer = customFloating $ W.RationalRect l t w h -- and the geometry:
      where
        h = 0.2
        w = 0.8
        t = 0.5 - (h / 2)
        l = 0.5 - (w / 2)

    spawnClock = "urxvt -title peaclock -name peaclock -e peaclock"
    findClock = resource =? "peaclock"
    manageClock = customFloating $ W.RationalRect l t w h -- and the geometry:
      where
        h = 0.5
        w = 0.5
        t = 0.5 - (h / 2)
        l = 0.5 - (w / 2)

    spawnTwitter = "urxvt -title twitter -name twitter -e nix run -f '<nixpkgs>' -I ~/repos python38Packages.rainbowstream -c rainbowstream -to 120"
    findTwitter = resource =? "twitter"
    manageTwitter = customFloating $ W.RationalRect l t w h -- and the geometry:
      where
        h = 0.8
        w = 0.8
        t = 0.5 - (h / 2)
        l = 0.5 - (w / 2)

-- Keybindings
extraKeys :: [((KeyMask, KeySym), X ())]
extraKeys =
  foldr1
    (++)
    [ [ -- Workspace Navigation
        ((mod1Mask, xK_f), workspacePrompt def (windows . W.greedyView)),
        ((mod1Mask .|. shiftMask, xK_f), workspacePrompt def (windows . W.shift))
        -- ((mod1Mask, xK_f), gridselectWorkspace def W.greedyView),
        -- ((mod1Mask .|. shiftMask, xK_f), bringSelected def)
      ],
      [ -- Spawn Programs
        ((mod1Mask .|. shiftMask, xK_Return), namedTerminal),
        ((mod1Mask, xK_p), spawn "rofi -show run -theme 'Arc-Dark'"),
        ((mod1Mask .|. shiftMask, xK_p), spawn "rofi -show window -theme 'Arc-Dark'"),
        ((mod1Mask, xK_g), spawn "eidolon list | tail -n +3 | rofi -dmenu -theme darker_than_black | cut -d '-' -f 2 | xargs eidolon run")
      ],
      [ -- XMonad Config
        ((mod1Mask .|. shiftMask, xK_t), sendMessage ToggleStruts)
      ],
      [ -- Screenshots
        ((mod1Mask, xK_s), screenshot),
        ((mod1Mask .|. shiftMask, xK_s), screenshotWindow)
      ],
      [ -- Headset Controls
        ((mod1Mask, xF86XK_Tools), spawn $ "pactl set-source-mute" ++ headsetSink ++ " 1"),
        ((mod1Mask, xF86XK_Launch5), spawn $ "pactl set-source-mute " ++ headsetSink ++ " 0"),
        ((mod1Mask, xF86XK_AudioPlay), spawn $ "pactl set-source-mute " ++ headsetSink ++ " toggle")
      ],
      [ -- Scratchpad
        ((mod1Mask, xK_m), namedScratchpadAction myScratchPads "mixer"),
        ((mod1Mask, xK_c), namedScratchpadAction myScratchPads "clock"),
        ((mod1Mask .|. controlMask, xK_t), namedScratchpadAction myScratchPads "twitter")
      ],
      [ -- XMonad.Prompt
        ((mod1Mask .|. controlMask, xK_o), unicodePrompt "/home/djanatyn/UnicodeData.txt" (def {font = "xft:Noto Color Emoji"})),
        ((mod1Mask .|. controlMask, xK_p), passPrompt def),
        ((mod1Mask, xK_c), namedScratchpadAction myScratchPads "clock")
      ]
    ]

screenshot :: X ()
screenshot = inputPrompt def "screenshot name?" >>= launch
  where
    launch (Just name) = spawn $ "maim -s " ++ name
    launch Nothing = return ()

screenshotWindow :: X ()
screenshotWindow = inputPrompt def "screenshot name?" >>= launch
  where
    padding = "20"
    delay = "3"

    launch (Just name) =
      spawn $
        "maim -s -p " ++ padding ++ " -d " ++ delay ++ " -B " ++ name
    launch Nothing = return ()

-- Prompt for terminal names
namedTerminal :: X ()
namedTerminal = inputPrompt def "terminal name?" >>= launch
  where
    launch (Just name) = spawn $ "urxvt -title " ++ name
    launch Nothing = return ()

-- Layouts
spacing = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True

tall = Tall 1 (3 / 100) (1 / 2)

column = Column (10 / 7)

mediaLayout = Column 3

defaultLayout = tall ||| Full ||| column

myLayout =
  avoidStruts $
    spacing $
      onWorkspace "music" mediaLayout $
        onWorkspace "twitter" Full $
          defaultLayout

-- Fade
myFadeHook :: FadeHook
myFadeHook =
  composeAll
    [ opaque,
      -- unfocused windows are no longer transparent
      isUnfocused --> opacity 1,
      -- Leave emacs mostly opaque
      className =? "Emacs-27.1" --> opacity 0.9
    ]

-- Projects
myProjects :: [Project]
myProjects =
  [ Project
      { projectName = "chat",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "Discord"
      },
    Project
      { projectName =
          "work",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "firefox -P work"
          spawn "urxvt -title 'work terminal'"
      },
    Project
      { projectName = "music",
        projectDirectory = "~/music",
        projectStartHook = Just $ do
          spawn "urxvt -e mpdas"
          spawn "urxvt -e ncmpcpp"
      },
    Project
      { projectName = "browse",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "firefox -P default"
      },
    Project
      { projectName = "twitter",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "surf tweetdeck.twitter.com"
      },
    Project
      { projectName = "feed",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "surf -z 1.35 10.100.0.1:8080"
      }
  ]

startupProcs :: [String]
startupProcs =
  [ "nitrogen --restore",
    "compton",
    "~/.screenlayouts/default.sh",
    "fluxgui"
  ]

-- Main
main :: IO ()
main = do
  xmobarProc <- spawnPipe "xmobar -o"
  spawn `traverse` startupProcs
  xmonad $
    ewmh $
      dynamicProjects myProjects $
        (defaults xmobarProc)
          `additionalKeys` extraKeys

defaults xmobarProc =
  def
    { logHook =
        composeAll
          [ fadeWindowsLogHook myFadeHook,
            dynamicLogWithPP $
              xmobarPP
                { ppOutput = hPutStrLn xmobarProc,
                  ppExtras = [loadAvg, battery],
                  ppSort = getSortByXineramaRule
                }
          ],
      manageHook = manageDocks <+> manageHook def <+> namedScratchpadManageHook myScratchPads,
      workspaces = myWorkspaces,
      layoutHook = myLayout,
      handleEventHook =
        composeAll
          [ handleEventHook def,
            docksEventHook,
            fadeWindowsEventHook
          ],
      borderWidth = 1,
      terminal = "urxvt",
      normalBorderColor = "#264653",
      focusedBorderColor = "#2a9d8f",
      focusFollowsMouse = False
    }
