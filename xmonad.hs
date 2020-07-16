import Data.Tree
import System.IO
import XMonad
import XMonad.Actions.CycleWS ()
import XMonad.Actions.GridSelect
import XMonad.Actions.TreeSelect
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.WorkspaceHistory ()
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.Circle (Circle (..))
import XMonad.Layout.Column
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Layout.Tabbed
import qualified XMonad.StackSet as W
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

myTreeConf :: TSConfig a
myTreeConf =
  tsDefaultConfig
    { ts_font = "xft:Terminess Powerline",
      ts_background = 0xc0c5d0da,
      ts_node = (0xff0d0931, 0xff3cbff0),
      ts_nodealt = (0xffffffff, 0xff0d0931),
      ts_highlight = (0xffffffff, 0xffE72478),
      ts_extra = 0xff000000
    }

extraKeys :: [((KeyMask, KeySym), X ())]
extraKeys =
  [ ((mod1Mask, xK_f), treeselectWorkspace myTreeConf myWorkspaces W.greedyView),
    -- ((mod1Mask, xK_f), gridselectWorkspace W.view), -- goToSelected defaultGSConfig),
    ((mod1Mask .|. shiftMask, xK_f), treeselectWorkspace myTreeConf myWorkspaces W.shift),
    ((mod1Mask .|. shiftMask, xK_f), bringSelected def),
    ((mod1Mask, xK_p), spawn "rofi -show run -theme 'Arc-Dark'"),
    ((mod1Mask .|. shiftMask, xK_p), spawn "rofi -show window -theme 'Arc-Dark'"),
    ((mod1Mask .|. shiftMask, xK_t), sendMessage ToggleStruts)
  ]

main :: IO ()
main = do
  xmobarProc <- spawnPipe "xmobar -o"
  spawn "nitrogen --restore"
  spawn "compton"
  -- spawn "bash ~/.screenlayout/default.sh"
  xmonad $
    ewmh
      def
        { manageHook = manageDocks <+> manageHook defaultConfig,
          workspaces = toWorkspaces myWorkspaces,
          logHook =
            dynamicLogWithPP $
              xmobarPP
                { ppOutput = hPutStrLn xmobarProc,
                  ppExtras = [loadAvg, battery],
                  ppSort = getSortByXineramaRule
                },
          layoutHook =
            avoidStruts $ spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $
              emptyBSP
                ||| tabbed shrinkText (theme smallClean)
                ||| Column (10 / 7)
                ||| Full,
          handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
          borderWidth = 1,
          terminal = "urxvt",
          normalBorderColor = "#053569",
          focusedBorderColor = "#0954B5",
          focusFollowsMouse = False
        }
      `additionalKeys` extraKeys
