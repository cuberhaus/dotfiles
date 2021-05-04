--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
-- IMPORTS
-- Base
import System.Exit
import System.IO
import XMonad
import XMonad.Config.Desktop
import qualified XMonad.StackSet as W
-- Data
import Data.Monoid
import qualified Data.Map        as M
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ServerMode
-- Layouts
import XMonad.Layout.Accordion
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
-- Layout modifiers
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing --add gaps
-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce

-- Variables
wallpaper :: String
wallpaper = "$HOME/.local/xdg/wallpapers/doggo.jpeg"
browser :: String
browser = "google-chrome-stable"
whatsapp :: String
whatsapp = "whatsapp-nativefier"
-- Brightness
brightUp :: String
brightUp = "changeBrightness 5 && pkill -SIGRTMIN+2 i3blocks"
brightDown :: String
brightDown = "changeBrightness -5 && pkill -SIGRTMIN+2 i3blocks"
-- Media
volumeUp :: String
volumeUp = "changeVolume +5 unmute && pkill -SIGRTMIN+1 i3blocks"
volumeDown :: String
volumeDown = "changeVolume -5 unmute && pkill -SIGRTMIN+1 i3blocks"
audioMute :: String
audioMute = "pactl set-sink-mute @DEFAULT_SINK@ toggle && changeVolume && pkill -SIGRTMIN+1 i3blocks"
mediaPlay :: String
mediaPlay = "playerctl play-pause"
mediaPause :: String
mediaPause = "playerctl pause"
mediaNext :: String
mediaNext = "playerctl next"
mediaPrev :: String
mediaPrev = "playerctl previous"

screenShotFast :: String
screenShotFast = "flameshot full -p $HOME/Pictures"
screenShotOptions :: String
screenShotOptions = "flameshot gui -p $HOME/Pictures"
explorer :: String
explorer = "nemo"


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
    -- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,  xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
      , ((modm,               xK_d     ), spawn "rofi -modi window,drun,run -show drun -show-icons -terminal termite ")

    -- launch gmrun
      , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_q     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- Apply mirror transformation
      , ((modm,               xK_x     ), sendMessage $ Toggle MIRROR)

    -- Toggle fullscreen ?
      , ((modm, xK_f), sendMessage $ Toggle FULL)

    --  Reset the layouts on the current workspace to default
      , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
      , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
      , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
      , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
      , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
      , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
      , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
      , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
      , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
      , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
      , ((modm              , xK_period), sendMessage (IncMasterN (-1)))


    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
      , ((modm .|. shiftMask, xK_e     ), io (exitWith ExitSuccess))

    -- Restart xmonad
      , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
      , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
    -- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
      , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
    -- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
-- myLayout = tiled ||| Mirror tiled ||| Full
myLayout = avoidStruts $ spacing 2 $ smartBorders  $ mkToggle (NOBORDERS ?? FULL ?? EOT) $ mkToggle (single MIRROR) $ (tiled ||| Accordion ||| spiral(6/7) ||| ThreeCol 1 (3/100) (1/2) ) 
    where
        -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
    -- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "confirm"         --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "MPlayer"        --> doFloat
    , className =? "dialog"          --> doFloat
    , className =? "download"        --> doFloat
    , className =? "error"           --> doFloat
    , className =? "file_progress"   --> doFloat
    , className =? "notification"    --> doFloat
    , className =? "pinentry-gtk-2"  --> doFloat
    , className =? "splash"          --> doFloat
    , className =? "toolbar"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

------------------------------------------------------------------------
    -- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = ewmhDesktopsEventHook

------------------------------------------------------------------------
    -- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
-- myLogHook = return ()

------------------------------------------------------------------------
    -- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --alpha 0 --tint 0x2f343f --height 19 &"
    spawnOnce "exec xss-lock --transfer-sleep-lock -- betterlockscreen -l &"
    spawnOnce "betterlockscreen -u ~/.local/xdg/wallpapers/landscapes > /dev/null 2>&1 &"
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
    spawnOnce "nm-applet &"
    spawnOnce "picom &"
    spawnOnce "blueman-applet &"
    spawnOnce "udiskie &"
    spawnOnce "xfce4-clipman &"
    spawnOnce "feh --bg-scale $HOME/.local/xdg/wallpapers/doggo.jpeg &"
    spawnOnce "birdtray &"
    spawnOnce "hp-systray &"
    spawnOnce "tomighty &"
    spawnOnce "flameshot &"
    spawnOnce "conky --config=$HOME/.conky/helix/conky\\ helix\\ white & "
-- escape needs to be double escaped in haskell

------------------------------------------------------------------------
    -- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO()
main = do
    -- Execute xmobar with its config and pipe xmonad output to xmobar
    xmproc <- spawnPipe "xmobar $HOME/.xmobarrc"
    xmonad $ ewmh desktopConfig  {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        -- workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        -- mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout ,
        -- manageDocks with trayer allows tray to not be focused like a window and be on all desktops instead of only on the first
        manageHook         = myManageHook <+> manageDocks ,
        handleEventHook    = myEventHook
                            <+> fullscreenEventHook
                            <+> docksEventHook
                              ,
        logHook            = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc,
            ppTitle =xmobarColor "#bd93f9"  "" . shorten 50
            },

        startupHook        = myStartupHook
    }
        `additionalKeysP` [
        -- Multimedia Keys
        ("<XF86AudioPlay>", spawn mediaPlay)
          , ("<XF86AudioPrev>", spawn mediaPrev)
          , ("<XF86AudioNext>", spawn mediaNext)
          , ("<XF86AudioStop>", spawn mediaPause)
          , ("<XF86AudioMute>", spawn audioMute)
          , ("<XF86AudioLowerVolume>", spawn volumeDown)
          , ("<XF86AudioRaiseVolume>", spawn volumeUp)
          , ("<XF86MonBrightnessUp>", spawn brightUp)
          , ("<XF86MonBrightnessDown>", spawn brightDown)
          , ("<Print>", spawn screenShotFast)
          , ("M-<Print>", spawn screenShotOptions)
          , (("M-g"), spawn browser) -- Windows + g (meta key is windows key)
          , (("M-n"), spawn explorer)
          , (("M-C-w"), spawn whatsapp) -- Windows + ctrl + w
                          ]

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
