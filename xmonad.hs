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
import XMonad.Layout.Roledex
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
-- Prompts
import XMonad.Prompt.Input
import XMonad.Prompt.XMonad
import qualified XMonad.StackSet as W
-- Util
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.Themes
import XMonad.Util.WorkspaceCompare

myWorkspaces :: Forest String
myWorkspaces =
  [ Node "home" [],
    Node "browse" [],
    Node "chat" [],
    Node "code" [],
    Node "game" [],
    Node "work" [],
    Node "music" []
  ]

extraKeys :: [((KeyMask, KeySym), X ())]
extraKeys =
  [ ((mod1Mask, xK_f), gridselectWorkspace def W.greedyView),
    ((mod1Mask .|. shiftMask, xK_f), bringSelected def),
    ((mod1Mask .|. shiftMask, xK_Return), namedTerminal),
    ((mod1Mask, xK_p), spawn "rofi -show run -theme 'Arc-Dark'"),
    ((mod1Mask .|. shiftMask, xK_p), spawn "rofi -show window -theme 'Arc-Dark'"),
    ((mod1Mask .|. shiftMask, xK_t), sendMessage ToggleStruts),
    ((mod1Mask, xF86XK_Tools), spawn "pactl set-source-mute alsa_input.usb-Logitech_G533_Gaming_Headset-00.mono-fallback 1"),
    ((mod1Mask, xF86XK_Launch5), spawn "pactl set-source-mute alsa_input.usb-Logitech_G533_Gaming_Headset-00.mono-fallback 0")
  ]

-- Prompt for terminal names
namedTerminal :: X ()
namedTerminal = inputPrompt def "terminal name?" >>= launch
  where
    launch (Just name) = spawn $ "urxvt -title " ++ name
    launch Nothing = return ()

main :: IO ()
main = do
  xmobarProc <- spawnPipe "xmobar -o"
  spawn "nitrogen --restore"
  spawn "compton"
  -- spawn "bash ~/.screenlayout/default.sh"
  xmonad $
    ewmh $
      def
        { logHook =
            dynamicLogWithPP $
              xmobarPP
                { ppOutput = hPutStrLn xmobarProc,
                  ppExtras = [loadAvg, battery],
                  ppSort = getSortByXineramaRule
                },
          manageHook = manageDocks <+> manageHook def,
          workspaces = toWorkspaces myWorkspaces,
          layoutHook =
            avoidStruts $
              spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $
                tabbed shrinkText (theme smallClean)
                  ||| Roledex
                  ||| Column (10 / 7)
                  ||| Tall 1 (3 / 100) (1 / 2)
                  ||| Full
                  ||| Grid,
          handleEventHook = handleEventHook def <+> docksEventHook,
          borderWidth = 1,
          terminal = "urxvt",
          normalBorderColor = "#264653",
          focusedBorderColor = "#2a9d8f",
          focusFollowsMouse = False
        }
        `additionalKeys` extraKeys
