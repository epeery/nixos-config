import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.InsertPosition
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main =
  xmonad $
    def
      { manageHook         = insertPosition End Newer
      , layoutHook         = myLayout
      , startupHook        = myStartupHook
      , modMask            = myModMask
      , terminal           = myTerminal
      , borderWidth        = myBorderWidth
      , normalBorderColor  = color2
      , focusedBorderColor = color5
      }
      `additionalKeysP` myKeys

------------------------------------------------------------------------
--                             Autostart                              --
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "nitrogen --restore &"
  spawnOnce "xrdb ~/.config/Xresources &"

------------------------------------------------------------------------
--                             Variables                              --
------------------------------------------------------------------------
color1 = "#000000"
color2 = "#7f7f7f"
color3 = "#D9D9D9"
color4 = "#ffffff"
color5 = "#D56162"

myBorderWidth = 4

myTerminal = "xst"
myFileManager = myTerminal ++ " -e ranger"
myBrowser = "brave"

myDmenu =
  "dmenu_run -nb "
    ++ show color1
    ++ " -nf "
    ++ show color3
    ++ " -sb "
    ++ show color5
    ++ " -sf "
    ++ show color4

myModMask = mod4Mask

------------------------------------------------------------------------
--                            Keybindings                             --
------------------------------------------------------------------------
myKeys =
    -- XMonad
    [ ("M-S-q",        io exitSuccess)                 -- Quit XMonad

    -- Programs
    , ("M-<Return>",   spawn myTerminal)               -- Launch terminal
    , ("M-w",          spawn myBrowser)                -- Launch browser
    , ("M-d",          spawn myDmenu)                  -- Launch dmenu
    , ("M-r",          spawn myFileManager)            -- Launch file-manager
    , ("M-q",          kill)                           -- Close the focused window
    , ("M-n",          refresh)                        -- Resize viewed windows to the correct siz

    -- Workspaces
    , ("M-<Tab>",      windows W.focusDown)            -- Move focus to the next window
    , ("M-S-<Tab>",    windows W.focusUp)              -- Move focus to the previous window
    , ("M-j",          windows W.focusDown)            -- Move focus to the next window
    , ("M-k",          windows W.focusUp)              -- Move focus to the previous window
    , ("M-m",          windows W.focusMaster)          -- Move focus to the master window
    , ("M-S-<Return>", windows W.swapMaster)           -- Swap the focused window and the master window
    , ("M-S-j",        windows W.swapDown)             -- Swap the focused window with the next window
    , ("M-S-k",        windows W.swapUp)               -- Swap the focused window with the previous window
    , ("M-<Space>",    sendMessage NextLayout)         -- Rotate through available layouts
    , ("M-h",          sendMessage Shrink)             -- Shrink the master area
    , ("M-l",          sendMessage Expand)             -- Expand the master area
    , ("M-i",          sendMessage (IncMasterN 1))     -- Increment the number of windows in the master area
    , ("M-o",          sendMessage (IncMasterN (-1)))  -- Deincrement the number of windows in the master area
    , ("M-t",          withFocused $ windows . W.sink) -- Push window back into tiling

    -- Monitors
    , ("M-.",          nextScreen)                     -- Switch focus to next monitor
    , ("M-,",          prevScreen)                     -- Switch focus to prev monitor
    ]

------------------------------------------------------------------------
--                              Layouts                               --
------------------------------------------------------------------------
myLayout = tiled ||| noBorders Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio = 0.618
    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100
