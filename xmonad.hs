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
import XMonad.Prompt.Input
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
  [ "chaos",
    "browse",
    "chat",
    "play",
    "build",
    "work",
    "music",
    "feed",
    "twitter"
  ]

-- PulseAudio Headset
headsetSink = "alsa_input.usb-Logitech_G533_Gaming_Headset-00.mono-fallback"

-- Scratchpads
myScratchPads =
  [ NS "mixer" spawnMixer findMixer manageMixer,
    NS "clock" spawnClock findClock manageClock
  ]
  where
    spawnMixer = "urxvt -title Pulsemixer -e pulsemixer" -- launch pulsemixer
    findMixer = resource =? "Pulsemixer" -- its window will be named "pulsemixer"
    manageMixer = customFloating $ W.RationalRect l t w h -- and the geometry:
      where
        h = 0.2
        w = 0.8
        t = 0.5 - (h / 2)
        l = 0.5 - (w / 2)

    spawnClock = "urxvt -name peaclock -e peaclock"
    findClock = resource =? "peaclock"
    manageClock = customFloating $ W.RationalRect l t w h -- and the geometry:
      where
        h = 0.5
        w = 0.5
        t = 0.5 - (h / 2)
        l = 0.5 - (w / 2)

-- Keybindings
extraKeys :: [((KeyMask, KeySym), X ())]
extraKeys =
  foldr1
    (++)
    [ [ -- Workspace Navigation
        ((mod1Mask, xK_f), workspacePrompt def (windows . W.greedyView)),
        -- ((mod1Mask, xK_f), gridselectWorkspace def W.greedyView),
        ((mod1Mask .|. shiftMask, xK_f), bringSelected def)
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
      [ -- Headset Controls
        ((mod1Mask, xF86XK_Tools), spawn $ "pactl set-source-mute" ++ headsetSink ++ " 1"),
        ((mod1Mask, xF86XK_Launch5), spawn $ "pactl set-source-mute " ++ headsetSink ++ " 0")
      ],
      [ -- Scratchpad
        ((mod1Mask, xK_m), namedScratchpadAction myScratchPads "mixer"),
        ((mod1Mask, xK_c), namedScratchpadAction myScratchPads "clock")
      ]
    ]

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
myFadeHook =
  composeAll
    [ opaque,
      -- Unfocused windows are transparent
      isUnfocused --> opacity 0.75,
      -- Leave Firefox mostly opaque
      (className =? "Firefox") <&&> (isUnfocused) --> opacity 0.9,
      -- Increased opacity for floating Discord windows
      (className =? "discord") <&&> (not <$> isFloating) --> opacity 0.95,
      (className =? "discord") <&&> (isFloating) --> opacity 0.75
    ]

-- Projects
myProjects =
  [ Project
      { projectName = "chat",
        projectDirectory = "~/",
        projectStartHook = Just $ do
          spawn "Discord"
      },
    Project
      { projectName = "work",
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
          spawn "surf 10.100.0.1:8080"
      }
  ]

startupProcs =
  [ "nitrogen --restore",
    "compton",
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
