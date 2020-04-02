import qualified XMonad.StackSet as W
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.InsertPosition
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.SpawnOnce

main = do
  xmonad $
    def
      { manageHook         = insertPosition End Newer
      , layoutHook         = myLayout
      , startupHook        = myStartupHook
      , modMask            = myModMask
      , terminal           = myTerminal
      , borderWidth        = myBorderWidth
      , normalBorderColor  = color1
      , focusedBorderColor = color4
      }
      `additionalKeysP` myKeys

------------------------------------------------------------------------
--                             Autostart                              --
------------------------------------------------------------------------
myStartupHook = do
  spawnOnce "feh -B \"#CDCBCD\" --bg-center ~/.config/wallpaper.png --no-fehbg &"
  spawnOnce "xrdb ~/.config/Xresources &"

------------------------------------------------------------------------
--                             Variables                              --
------------------------------------------------------------------------
color0 = "#000000"
color1 = "#7f7f7f"
color2 = "#D9D9D9"
color3 = "#ffffff"
color4 = "#D56162"

myBorderWidth = 4

myTerminal = "xst"
myFileManager = myTerminal ++ " -e ranger"
myBrowser = "brave"

myXmobarConfig = "~/.xmonad/xmobar.hs"

myDmenu =
  "dmenu_run -nb "
    ++ show color0
    ++ " -nf "
    ++ show color2
    ++ " -sb "
    ++ show color4
    ++ " -sf "
    ++ show color3

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
myLayout = Tall 1 (3 / 100) (61.8 / 100) ||| noBorders Full
