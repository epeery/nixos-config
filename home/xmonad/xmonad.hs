import Data.Char
import Data.GI.Base
import qualified GI.Gtk as Gtk
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

main = do
  h <- spawnPipe "xmobar"
  xmonad $ docks $
    def
      { manageHook = myManageHook,
        layoutHook = myLayout,
        startupHook = myStartupHook,
        logHook = do
          dynamicLogWithPP
            . namedScratchpadFilterOutWorkspacePP
            $ def
              { ppCurrent = wrap "<fn=1>" "</fn>",
                ppLayout = const "",
                ppOutput = hPutStrLn h,
                ppTitle = const "",
                ppVisible = wrap ("<fc=" <> color5 <> ">") "</fc>",
                ppWsSep = " "
              },
        workspaces = myWorkspaces,
        modMask = myModMask,
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        normalBorderColor = color1,
        focusedBorderColor = color3
      }
      `additionalKeysP` myKeys

-- https://wiki.haskell.org/Xmonad/General_xmonad.hs_config_tips#ManageHook_examples
myManageHook :: ManageHook
myManageHook =
  manageDocks
    <+> insertPosition End Newer
    <+> namedScratchpadManageHook scratchpads

------------------------------------------------------------------------
--                             Autostart                              --
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "setbg &"
  spawnOnce "xrdb ~/.config/Xresources &"

------------------------------------------------------------------------
--                             Variables                              --
------------------------------------------------------------------------
color0 = "#000000"

color1 = "#7f7f7f"

color2 = "#CDCBCD"

color3 = "#ffffff"

color4 = "#D56162"

color5 = "#0081D5"

myBorderWidth = 3

myTerminal = "xst"

myFileManager = myTerminal <> " -e ranger"

myBrowser = "brave"

myXmobarConfig = "~/.xmonad/xmobar.hs"

myDmenuConfig =
  "-nb "
    <> show color3
    <> " -nf "
    <> show color0
    <> " -sb "
    <> show color5
    <> " -sf "
    <> show color3

myDmenu = "dmenu_run " <> myDmenuConfig

myWorkspaces = ("    " <>) . show <$> [1 .. 9]

myModMask = mod4Mask

scratchpads =
  [ NS "ncmpcpp" (myTerminal <> " -A 200 -n ncmpcpp 'ncmpcpp'") (resource =? "ncmpcpp") (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  ]

------------------------------------------------------------------------
--                            Keybindings                             --
------------------------------------------------------------------------
myKeys =
  -- XMonad
  [ ("M-S-q", io exitSuccess), -- Quit XMonad
    ("M-<Tab>", windows W.focusDown), -- Move focus to the next window
    ("M-S-<Tab>", windows W.focusUp), -- Move focus to the previous window
    ("M-j", windows W.focusDown), -- Move focus to the next window
    ("M-k", windows W.focusUp), -- Move focus to the previous window
    ("M-S-<Return>", windows W.swapMaster), -- Swap the focused window and the master window
    ("M-S-j", windows W.swapDown), -- Swap the focused window with the next window
    ("M-S-k", windows W.swapUp), -- Swap the focused window with the previous window
    ("M-<Space>", sendMessage NextLayout), -- Rotate through available layouts
    ("M-h", sendMessage Shrink), -- Shrink the master area
    ("M-l", sendMessage Expand), -- Expand the master area
    ("M-i", sendMessage (IncMasterN 1)), -- Increment the number of windows in the master area
    ("M-o", sendMessage (IncMasterN (-1))), -- Deincrement the number of windows in the master area
    ("M-t", withFocused $ windows . W.sink), -- Push window back into tiling
    -- ("M-b", sendMessage ToggleStruts), -- Toggle bar

    -- Programs
    ("M-<Return>", spawn myTerminal), -- Launch terminal
    ("M-w", spawn myBrowser), -- Launch browser
    ("M-d", spawn myDmenu), -- Launch dmenu
    ("M-r", spawn myFileManager), -- Launch file-manager
    ("M-c", spawn $ "clipmenu " <> myDmenuConfig), -- Launch clipmenu
    ("M-q", kill), -- Close the focused window
    ("M-m", namedScratchpadAction scratchpads "ncmpcpp"),
    -- Monitors
    ("M-.", nextScreen), -- Switch focus to next monitor
    ("M-,", prevScreen) -- Switch focus to prev monitor
  ]

------------------------------------------------------------------------
--                              Layouts                               --
------------------------------------------------------------------------
instance Shrinker CustomShrink where
  shrinkIt _ _ = [""]

myLayout =
  noBorders $
    ( avoidStruts $ noFrillsDeco CustomShrink topBarTheme
        $ spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
        $ Tall 1 (3 / 100) (61.8 / 100)
    )
      ||| Full
  where
    topBarTheme =
      def
        { inactiveBorderColor = color2,
          inactiveColor = color2,
          inactiveTextColor = color3,
          activeBorderColor = color3,
          activeColor = color3,
          activeTextColor = color0,
          urgentBorderColor = color4,
          urgentTextColor = color3,
          decoHeight = 20
        }
