{-# LANGUAGE AllowAmbiguousTypes, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

-- Main
import XMonad
import System.IO (hPutStrLn)
import System.Exit
import qualified XMonad.StackSet as W
import XMonad.Hooks.FloatNext

-- Actions
import XMonad.Actions.CycleWS (Direction1D(..), moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CopyWindow (kill1, wsContainingCopies, copyToAll, killAllOtherCopies)
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.FloatKeys (keysResizeWindow)

-- Data
import Data.Semigroup
import Data.Monoid
import Data.Maybe (fromJust, isJust)
import qualified Data.Map as M

-- Hooks
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, isDialog, doCenterFloat, doRectFloat)

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

-- Utilities
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Scratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce
import Graphics.X11.ExtraTypes.XF86

------------------------------------------------------------------------
-- My Strings
------------------------------------------------------------------------
myTerminal :: String
myTerminal = "alacritty"          -- Default terminal

myModMask :: KeyMask
myModMask = mod4Mask          -- Super Key (--mod4Mask= super key --mod1Mask= alt key --controlMask= ctrl key --shiftMask= shift key)

myBorderWidth :: Dimension
myBorderWidth = 0             -- Window border

myClickJustFocuses :: Bool
myClickJustFocuses = False

------------------------------------------------------------------------
-- Space between Tiling Windows
------------------------------------------------------------------------
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 20 5 5 5) True (Border 20 5 5 5) True

------------------------------------------------------------------------
-- Layout Hook
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts full
               $ mkToggle (NBFULL ?? NOBORDERS ?? MIRROR ?? EOT) myDefaultLayout
             where
               myDefaultLayout =      withBorder myBorderWidth threeCol
                                ||| tall
                                ||| full
                                ||| grid
                                ||| mirror

------------------------------------------------------------------------
-- Tiling Layouts
------------------------------------------------------------------------
tall     = renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Tall</fc>"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 8
           $ mySpacing 5
           $ ResizableTall 1 (3/100) (1/2) []               
grid     = renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Grid</fc>"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 12
           $ mySpacing 5
           $ mkToggle (single MIRROR)
          $ Grid (16/10)   
mirror     = renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Mirror</fc>"]
           $ smartBorders
           $ windowNavigation
           $ subLayout [] (smartBorders Simplest)
           $ limitWindows 6
           $ mySpacing 5
           $ Mirror  
           $ ResizableTall 1 (3/100) (1/2) []            
full     = renamed [Replace " <fc=#95e6cb><fn=2> \61449 </fn>Full</fc>"]
           $ Full                     

threeCol = renamed [Replace "Three"]
         $ avoidStruts
         $ mySpacing 5
         $ ThreeColMid 1 (1/10) (1/2)

------------------------------------------------------------------------
-- Workspaces
------------------------------------------------------------------------

myWorkspaces :: [String]
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

------------------------------------------------------------------------
-- Scratch Pads
------------------------------------------------------------------------
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [
      NS "osu!.exe"              "osu!.exe"              (appName =? "osu!.exe")                   (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "nautilus"             "nautilus"             (className =? "Org.gnome.Nautilus")      (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "ncmpcpp"              launchMocp             (title =? "ncmpcpp")                     (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "whatsapp-for-linux"   "whatsapp-for-linux"   (appName =? "whatsapp-for-linux")        (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
    , NS "ranger"               launchRang             (title =? "rangersp")                    (customFloating $ W.RationalRect 0.15 0.15 0.7 0.7)
  ]
  where
    launchMocp = myTerminal ++ " -T ncmpcpp -e ncmpcpp"
    launchRang = myTerminal ++ " -T rangersp -e ranger"

-- idk help

------------------------------------------------------------------------
-- Custom Keys
------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =

    [
    -- Xmonad
        ("M-S-r", spawn "xmonad --recompile && xmonad --restart")                   -- Recompile & Restarts xmonad
      , ("M-S-q", io exitSuccess)                                                   -- Quits xmonad

    -- System
      , ("<XF86AudioMute>",         spawn "pamixer -t")                             -- Mute
      , ("<XF86AudioRaiseVolume>",  spawn "pamixer -i 5")                           -- Volume Up
      , ("<XF86AudioLowerVolume>",  spawn "pamixer -d 5")                           -- Volume Down
      , ("<XF86AudioPlay>",         spawn "mpc toggle")                             -- ncmpcpp play/pause
      , ("<XF86AudioStop>",         spawn "mpc pause")                              -- ncmpcpp stop
      , ("<XF86AudioPrev>",         spawn "mpc prev")                               -- ncmpcpp previous
      , ("<XF86AudioNext>",         spawn "mpc next")                               -- ncmpcpp next
      , ("<Print>",                 spawn "scrot")                                  -- Screenshot

    -- System but with super key instead of Fn
      , ("M-<F1>", 		    spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
      , ("M-<F2>",		    spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
      , ("M-<F3>",		    spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
      , ("M-<F4>",        spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
      , ("M-<F11>",		    spawn "light -U 5")
      , ("M-<F12>",		    spawn "light -A 5")

    -- Run Prompt
      , ("M-p", spawn "dmenu_run")                                                  -- Run Dmenu (rofi -show drun but with icons :D )
      , ("M-<Space>", spawn "rofi -show drun -show-icons")                          -- Rofi Launcher

    -- Apps
      , ("M-c", spawn myTerminal)                                                   -- Terminal
      , ("M-w", spawn "firefox")                                                    -- web
      , ("M-f", spawn "thunar")                                                     -- File Manager
      , ("M-e", spawn "flatpak run com.spotify.Client")				                      -- Music player (Spotify)
      , ("M-s", spawn "flameshot gui")

    -- Window navigation
      , ("M-S-<Space>", withFocused $ windows . W.sink)                             -- Toggle floating
      , ("M-r",                       toggleFloatAllNew)
      , ("M-d", sendMessage NextLayout)                                             -- Rotate through the available layout algorithms
      , ("M-<Left>", windows W.swapMaster)                                          -- Swap the focused window and the master window
      , ("M-<Up>", windows W.swapUp)                                                -- Swap the focused window with the previous windo
      , ("M-<Down>", windows W.swapDown)                                            -- Swap the focused window with the next window   
      , ("M-h", sendMessage (IncMasterN 1))
      , ("M-l", sendMessage (IncMasterN (-1)))
      , ("M-<Tab>", rotAllDown)                                                     -- Rotate all windows
      , ("M-S-f", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts)       -- Toggles full width
      , ("M-l", toggleCopyToAll)                                                    -- Copy window to all workspaces
      , ("M-q", kill1)                                                              -- Quit the currently focused client
      , ("M-C-q", killAll)                                                          -- Quit all windows on current workspace

    -- Increase/decrease spacing (gaps)
      , ("M-C-j", decWindowSpacing 4)                                               -- Decrease window spacing
      , ("M-C-k", incWindowSpacing 4)                                               -- Increase window spacing
      , ("M-C-h", decScreenSpacing 4)                                               -- Decrease screen spacing
      , ("M-C-l", incScreenSpacing 4)                                               -- Increase screen spacing

   ]  

------------------------------------------------------------------------
-- Moving between WS
------------------------------------------------------------------------
      where toggleCopyToAll = wsContainingCopies >>= \ws -> case ws of
							[] -> windows copyToAll
							_ -> killAllOtherCopies
            nonNSP          = WSIs (return (\ws -> W.tag ws /= "NSP"))
            nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "NSP"))
------------------------------------------------------------------------
-- Floats
------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "confirm"                           --> doFloat
     , className =? "file_progress"                     --> doFloat
     , className =? "mpv"                               --> doCenterFloat
     , className =? "dialog"                            --> doFloat
     , className =? "download"                          --> doFloat
     , className =? "error"                             --> doFloat
     , className =? "notification"                      --> doFloat
     , className =? "toolbar"                           --> doFloat
     --, isFullscreen -->  doFullFloat
     , isDialog --> doCenterFloat
     ] <+> namedScratchpadManageHook myScratchPads


------------------------------------------------------------------------
-- Startup Hooks -------------------------------------------------------
------------------------------------------------------------------------
myStartupHook = do
    spawnOnce "xrandr --output HDMI-1 --primary --mode 1920x1080 --rate 60.00"
    spawnOnce "feh --bg-fill $HOME/Wallpapers/power-white-black.png &"
    spawnOnce "picom -b &"
    spawnOnce "dunst &"
    spawnOnce "flameshot &"
    spawnOnce "~/.config/polybar/launch.sh &"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"

-----------------------------------------------------------------------
-- Event Hook ---------------------------------------------------------
-----------------------------------------------------------------------


------------------------------------------------------------------------
-- Main Do -------------------------------------------------------------
------------------------------------------------------------------------
main :: IO ()
main = do
        xmonad . ewmhFullscreen $ ewmh def
          { manageHook = floatNextHook <+> myManageHook <+> manageDocks
                , modMask            = mod4Mask
                , layoutHook         = myLayoutHook
                , workspaces         = myWorkspaces
                , terminal           = myTerminal
                , borderWidth        = myBorderWidth
		, startupHook        = myStartupHook
                } `additionalKeysP` myKeys
