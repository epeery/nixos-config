import Control.Monad (void)
import Data.Char
import qualified Data.Map as M
import System.Exit
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.XUtils

main :: IO ()
main = do
  nScreens <- countScreens
  hs <- sequence $ (spawnPipe . ("xmobar -x" <>) . show) <$> [0 .. nScreens - 1]
  xmonad $ docks $
    def
      { manageHook = myManageHook,
        layoutHook = myLayout,
        startupHook = myStartupHook,
        logHook = do
          (S current) <- currentScreenId
          dynamicLogWithPP
            . namedScratchpadFilterOutWorkspacePP
            $ def
              { ppCurrent = wrap "<fn=1>" "</fn>",
                ppLayout = const "",
                ppOutput = \s ->
                  mapM_
                    ( \(i, h) ->
                        if i == current
                          then hPutStrLn h s
                          else hPutStrLn h ""
                    )
                    $ zip [0 ..] hs,
                ppTitle = const "",
                ppVisible = wrap ("<fc=" <> color5 <> ">") "</fc>",
                ppWsSep = "  "
              },
        workspaces = myWorkspaces,
        modMask = mod4Mask,
        terminal = myTerminal,
        borderWidth = myBorderWidth,
        normalBorderColor = color1,
        focusedBorderColor = color3
      }
      `additionalKeysP` myKeys

myManageHook :: ManageHook
myManageHook =
  composeOne
    [ className =? "Gimp" -?> doFloat,
      isDialog -?> doCenterFloat,
      pure True -?> insertPosition End Newer
    ]
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads

myStartupHook :: X ()
myStartupHook = do
  spawnOnce "setbg &"
  spawnOnce "xrdb ~/.config/Xresources &"

currentScreenId :: X ScreenId
currentScreenId = withWindowSet $ return . W.screen . W.current

color0 :: String
color0 = "#000000"

color1 :: String
color1 = "#7f7f7f"

color2 :: String
color2 = "#CDCBCD"

color3 :: String
color3 = "#ffffff"

color4 :: String
color4 = "#F45B69"

color5 :: String
color5 = "#0087B8"

myBorderWidth :: Dimension
myBorderWidth = 3

myTerminal :: String
myTerminal = "xst"

myFileManager :: String
myFileManager = myTerminal <> " -e ranger"

myBrowser :: String
myBrowser = "brave"

myXmobarConfig :: String
myXmobarConfig = "~/.xmonad/xmobar.hs"

myDmenuConfig :: String
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

myWorkspaces :: [String]
myWorkspaces = ("    " <>) . show <$> [1 .. 9]

------------------------------------------------------------------------
--                            Keybindings                             --
------------------------------------------------------------------------
myKeys :: [(String, X ())]
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
    -- Programs
    ("M-<Return>", spawn myTerminal), -- Launch terminal
    ("M-w", spawn myBrowser), -- Launch browser
    ("M-d", spawn myDmenu), -- Launch dmenu
    ("M-r", spawn myFileManager), -- Launch file-manager
    ("M-c", spawn $ "clipmenu " <> myDmenuConfig), -- Launch clipmenu
    ("M-q", kill), -- Close the focused window
    ("M-m", namedScratchpadAction scratchpads "ncmpcpp"),
    ("M-p", namedScratchpadAction scratchpads "transmission-gtk"),
    -- Monitors
    ("M-.", nextScreen), -- Switch focus to next monitor
    ("M-,", prevScreen) -- Switch focus to prev monitor
  ]

------------------------------------------------------------------------
--                              Layouts                               --
------------------------------------------------------------------------

data AllFloats = AllFloats deriving (Read, Show)

instance SetsAmbiguous AllFloats where
  hiddens _ wset _ _ _ = M.keys $ W.floating wset

instance Shrinker CustomShrink where
  shrinkIt _ _ = [""]

myLayout =
  lessBorders AllFloats
    . noBorders
    $ ( avoidStruts $ noFrillsDeco CustomShrink topBarTheme
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

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS
      "ncmpcpp"
      (myTerminal <> " -n ncmpcpp 'ncmpcpp'")
      (resource =? "ncmpcpp")
      (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)),
    NS
      "transmission-gtk"
      "transmission-gtk"
      (resource =? "transmission-gtk")
      (customFloating $ W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2))
  ]
