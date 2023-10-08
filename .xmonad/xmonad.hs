-- My take on Xmonad WM
-- WARNING: MAY NEED TO SHUT DOWN XMONAD COMPLETELY TO SEE CHANGES, TRY TO EXIT IF NOT WORKING PROPERLY WITH SOME CHANGE
-- Author: https://github.com/cuberhaus/dotfiles

-- IMPORTS
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

-- Base
import XMonad.Util.WorkspaceCompare
import Control.Monad (when)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Monoid (All, Endo)
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)
import XMonad
-- Actions
import XMonad.Actions.Navigation2D -- this treats tabs properly, each tab is not an independent window but a group
import XMonad.Actions.Promote (promote)
import XMonad.Actions.RotSlaves (rotAllDown, rotSlavesDown)
import XMonad.Actions.WithAll (killAll, sinkAll)
-- Config
import XMonad.Config.Desktop (desktopConfig)
-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeWindows ()
import XMonad.Hooks.InsertPosition
    ( insertPosition, Focus(Newer), Position(Above) )
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ()
import XMonad.Hooks.ServerMode ()
-- Layouts
import XMonad.Layout.Accordion (Accordion (Accordion))
import XMonad.Layout.GridVariants (Grid (Grid))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders ( smartBorders ) -- removes borderlines from windows
import XMonad.Layout.Reflect (REFLECTX (REFLECTX)) -- move master to the other side
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleFloat (shrinkText)
import XMonad.Layout.Simplest (Simplest (Simplest))
import XMonad.Layout.SimplestFloat ()
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral (spiral)
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeCol))
import XMonad.Layout.WindowNavigation (windowNavigation)
-- Utilities
import qualified XMonad.StackSet as W
import XMonad.Util.Dmenu (dmenu,menuArgs) -- https://bbs.archlinux.org/viewtopic.php?id=120298>
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (hPutStrLn, spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

-- This code works
powerOffWithWarning :: X ()
powerOffWithWarning = do
  let o1 = "confirm poweroff"
  let o2 = "cancel"
  s <- dmenu [o2, o1]
  when (o1 == s) (io $ spawn "poweroff")

quitWithWarning :: X ()
quitWithWarning = do
  let o1 = "confirm quit"
  let o2 = "cancel"
  s <- dmenu [o2, o1]
  when (o1 == s) (io exitSuccess)

closeAllWindows :: X ()
closeAllWindows = do
  let m = "confirm close all windows"
  s <- dmenu [m]
  when (m == s) killAll

-- Variables
myFont :: String
myFont = "xft:SauceCodePro Nerd Font Mono:regular:size=9:antialias=true:hinting=true"

wallpaper :: String
wallpaper = "$HOME/.local/xdg/wallpapers/doggo.jpeg"

helix :: String
helix = "$HOME/.conky/helix/conky\\ helix\\ white"

-- Applications
browser :: String
browser = "google-chrome-stable"

-- emacs :: String 
-- emacs = "emacs"

mail :: String
mail = "thunderbird"

emacs :: String 
emacs = "emacs"

emacsClient :: String 
emacsClient = "emacsclient -nc"

doomEmacs :: String
doomEmacs = "emacs --with-profile doom"

-- browser = "firefox"

whatsapp :: String
whatsapp = "whatsapp-nativefier"

explorer :: String
explorer = "nemo"

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

-- Screenshots
screenShotFast :: String
screenShotFast = "flameshot full -p $HOME/Pictures"

screenShotOptions :: String
screenShotOptions = "flameshot gui -p $HOME/Pictures"

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "kitty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- sizes
gap :: Int
gap = 5

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask :: KeyMask
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
-- spotifyMusicCommand = "dex /usr/share/applications/spotify.desktop"

spotifyMusicCommand :: String
spotifyMusicCommand = "spotify"

isSpotifyMusic :: Query Bool
isSpotifyMusic = className =? "Spotify"

-- whatsappCommand ="dex /usr/share/applications/whatsapp-nativefier.desktop"
whatsappCommand :: String
-- whatsappCommand = "whatsapp-nativefier" -- yay app
whatsappCommand = "whatsie" -- snap app

-- also works to write the name of the command
isWhatsapp :: Query Bool
-- isWhatsapp = className =? "whatsapp-nativefier-d40211" -- yay app
isWhatsapp = className =? "WhatSie" -- snap app

thunderbirdCommand :: String
thunderbirdCommand = "thunderbird"

isThunderbird :: Query Bool
isThunderbird = className =? "thunderbird"

discordCommand :: String
discordCommand = "discord"

isDiscord :: Query Bool
isDiscord = className =? "discord"

chatGPTCommand :: String
chatGPTCommand = "chat-gpt"

isChatGPT :: Query Bool
isChatGPT = className =? "Chat-gpt"

speedCrunchCommand :: String
speedCrunchCommand =  "speedcrunch"

isSpeedCrunch :: Query Bool
isSpeedCrunch = className =? "SpeedCrunch"

kittyCommand :: String
kittyCommand = "kitty --class='kittyPad'"

isKitty :: Query Bool
isKitty = className =? "kittyPad"

cavaCommand = "kitty --class='cava' -o font_size=1 cava"
isCava = className =? "cava"

isEmacs = title =? "EmacsScratch"
emacsCommand = "emacs --name='EmacsScratch' --title=EmacsScratch"

-- calculatorCommand :: String
-- calculatorCommand = "gnome-calculator"

calculatorCommand :: String
calculatorCommand = "kalk"

-- isCalculator :: Query Bool
-- isCalculator = className =? "gnome-calculator"

isCalculator :: Query Bool
isCalculator = className =? "kalk"

scratchpads :: [NamedScratchpad]
scratchpads =
  [ 
    NS "calculator" calculatorCommand isCalculator (customFloating $ W.RationalRect (1 / 6) (1 / 6) (4 / 6) (4 / 6)),
    NS "Emacs" emacsCommand isEmacs (customFloating $ W.RationalRect (1 / 12) (1 / 12) (5 / 6) (5 / 6)),
    NS "Spotify" spotifyMusicCommand isSpotifyMusic (customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)),
    -- NS "Spotify" cavaCommand isCava (customFloating $ W.RationalRect (1 / 12) (1 / 12) (5 / 6) (1 / 24)),
    NS "WhatsApp" whatsappCommand isWhatsapp (customFloating $ W.RationalRect (1 / 6) (1 / 6) (4 / 6) (4 / 6)),
    NS "SpeedCrunch" speedCrunchCommand isSpeedCrunch (customFloating $ W.RationalRect (1 / 6) (1 / 6) (4 / 6) (4 / 6)),
    NS "Kitty" kittyCommand isKitty (customFloating $ W.RationalRect (1 / 6) (1 / 6) (4 / 6) (4 / 6)),
    NS "ChatGPT" chatGPTCommand isChatGPT (customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)),
    NS "Thunderbird" thunderbirdCommand isThunderbird (customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)),
    NS "Discord" discordCommand isDiscord (customFloating $ W.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8))
  ]

myWorkspaces :: [String]
-- myWorkspaces = ["  1  ", "  2  ", "  3  ", "  4  ", "  5  ", "  6  ", "  7  ", "  8  ", "  9  ", "  0  "]
myWorkspaces = ["  1  ", "  2  ", "  3  ", "  4  ", "  5  ", "  6  ", "  7  ", "  8  ", "  9  "]

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..] -- zipWith (,) == \x y -> (x,y)

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

-- Border colors for unfocused and focused windows, respectively.

-- With transparency
-- myFocusedBorderColor = "#dddddd"
-- myNormalBorderColor = "#000000"

-- No transparency
myNormalBorderColor :: String
myNormalBorderColor  = "#282c34"

myFocusedBorderColor :: String
myFocusedBorderColor = "#bd93f9"
-- myFocusedBorderColor = "#000000"
-- myFocusedBorderColor = "#FF0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    [ --  Reset the layouts on the current workspace to default
      ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
      -- -- Resize viewed windows to the correct size
      -- , ((modm,               xK_r     ), refresh)

      -- Push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink)

    ]
      ++
      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++

      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_0],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

-- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1),
        \w ->
          focus w >> mouseMoveWindow w
            >> windows W.shiftMaster
      ),
      -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> focus w >> windows W.shiftMaster),
      -- mod-button3, Set the window to floating mode and resize by dragging
      ( (modm, button3),
        \w ->
          focus w >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
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
-- tiled |||  deprecated by resizableTall
mySpacing = smartSpacing gap

myNav2DConf :: Navigation2DConfig
myNav2DConf =
  def
    { defaultTiledNavigation = centerNavigation,
      floatNavigation = centerNavigation,
      screenNavigation = lineNavigation,
      layoutNavigation =
        [ ("Full", centerNavigation)
        -- line/center same results   ,("Simple Tabs", lineNavigation)
        --                            ,("Simple Tabs", centerNavigation)
        ],
      unmappedWindowRect =
        [ ("Full", singleWindowRect)
        -- works but breaks tab deco  ,("Simple Tabs", singleWindowRect)
        -- doesn't work but deco ok   ,("Simple Tabs", fullScreenRect)
        ]
    }

tall =
  renamed [Replace "tall"] $
    smartBorders $
      windowNavigation $
        addTabs shrinkText myTabTheme $
          subLayout [] Simplest $
            mySpacing $
              ResizableTall 1 (3 / 100) (1 / 2) []

threeCol =
  renamed [Replace "threeCol"] $
    smartBorders $
      windowNavigation $
        addTabs shrinkText myTabTheme $
          subLayout [] Simplest $
            mySpacing $
              ThreeCol 1 (3 / 100) (1 / 2)

spirals =
  renamed [Replace "spirals"] $
    smartBorders $
      mySpacing $
        spiral (6 / 7)

tallAccordion =
  renamed [Replace "tallAccordion"] $
    mySpacing $
      Accordion

myLayout = avoidStruts $ mkToggle (SMARTBORDERS ?? NBFULL ?? EOT) $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) (tall ||| threeCol ||| spirals ||| tallAccordion)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100

-- setting colors for tabs layout and tabs sublayout.
myTabTheme =
  def
    { fontName = myFont,
      activeColor = "#46d9ff",
      inactiveColor = "#313846",
      activeBorderColor = "#46d9ff",
      inactiveBorderColor = "#282c34",
      activeTextColor = "#282c34",
      inactiveTextColor = "#d0d0d0"
    }

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
myManageHook :: Query (Endo WindowSet)
myManageHook =
  composeAll
    [ className =? "confirm" --> doFloat,
      className =? "Gimp" --> doFloat,
      className =? "MPlayer" --> doFloat,
      className =? "dialog" --> doFloat,
      className =? "download" --> doFloat,
      className =? "error" --> doFloat,
      className =? "file_progress" --> doFloat,
      className =? "notification" --> doFloat,
      className =? "pinentry-gtk-2" --> doFloat,
      className =? "splash" --> doFloat,
      className =? "toolbar" --> doFloat,
      resource =? "desktop_window" --> doIgnore,
      resource =? "kdesktop" --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook :: Event -> X All
myEventHook =
  -- ewmhDesktopsEventHook
    dynamicPropertyChange "WM_NAME" (title =? "Spotify" --> floating)
    <+> dynamicPropertyChange "WM_NAME" (title =? "whatsapp-nativefier-d40211" --> floating2)
    <+> dynamicPropertyChange "WM_NAME" (title =? "chat-gpt" --> floating2)
    <+> dynamicPropertyChange "WM_NAME" (title =? "discord" --> floating)
    <+> dynamicPropertyChange "WM_NAME" (title =? "EmacsScratch" --> floating)
    <+> dynamicPropertyChange "WM_NAME" (title =? "Kalk" --> floating2)
    -- <+> fullscreenEventHook -- deprecated for ewmhFullscreen
    -- <+> docksEventHook -- deprecated by docs. It's now a combinator on the whole configuration object and not just a hook.
  where
    floating = customFloating $ W.RationalRect (1 / 12) (1 / 12) (5 / 6) (5 / 6)
    floating2 = customFloating $ W.RationalRect (1 / 8) (1 / 8) (5 / 6) (5 / 6)

-- whatsapp isn't really needed here just put it here for completeness and demonstration of where with multiple variables
-- To adjust rectangle had to increment first two numbers denominator to move screen upwards and change last two numbers nominator to scale up the window
-- customFloating named scratchpad not floating, had to use this instead https://github.com/xmonad/xmonad/issues/214

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
myStartupHook :: X ()
myStartupHook = do
  spawnOnce "numlockx &"
  spawnOnce "trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --alpha 0 --tint 0x2f343f --height 19 &"
  spawnOnce "exec xss-lock --transfer-sleep-lock -- i3lock &"
  -- spawnOnce "exec xss-lock --transfer-sleep-lock -- betterlockscreen -l &" --  Not working properly
  -- spawnOnce "betterlockscreen -u ~/.local/xdg/wallpapers/unsplash > /dev/null 2>&1 &"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &"
  spawnOnce "nm-applet &"
  spawnOnce "picom &"
  spawnOnce "blueman-applet &"
  spawnOnce "udiskie &"
  spawnOnce "xfce4-clipman &"
  spawnOnce ("feh --bg-scale " ++ wallpaper ++ "& ")
  spawnOnce "hp-systray &"
  spawnOnce "tomighty &"
  spawnOnce "flameshot &"
  spawnOnce "sh -c 'sleep 15; conky --config=$HOME/.conky/conkyrss & '"
  spawnOnce ("conky --config=" ++ helix ++ "& ")
  -- spawnOnOnce (myWorkspaces !! 8) "speedcrunch"
-- escape needs to be double escaped in haskell

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

-- modifier keys M1-M5 M1 is left alt key
-- Keybindings

myEmacsKeys :: [(String, X ())]
myEmacsKeys =
  [ -- Multimedia Keys
    ("<XF86Calculator>", namedScratchpadAction scratchpads "calculator"),
    ("<XF86WWW>", spawn browser),
    ("<XF86MyComputer>", spawn explorer),
    ("<XF86Mail>", namedScratchpadAction scratchpads "Thunderbird"),
    -- ("<XF86KbdBrightnessUp>", ),
    -- ("<XF86KbdBrightnessDown>", ),
    -- ("", ),

    ("<XF86AudioPlay>", spawn mediaPlay),
    ("<XF86AudioPrev>", spawn mediaPrev),
    ("<XF86AudioNext>", spawn mediaNext),
    ("<XF86AudioStop>", spawn mediaPause),
    ("<XF86AudioMute>", spawn audioMute),
    ("<XF86AudioLowerVolume>", spawn volumeDown),
    ("<XF86AudioRaiseVolume>", spawn volumeUp),

    -- Brightness keys
    ("<XF86MonBrightnessUp>", spawn brightUp),
    ("<XF86MonBrightnessDown>", spawn brightDown),

    -- Screenshots
    ("<Print>", spawn screenShotFast), -- Take screenshot
    ("M-<Print>", spawn screenShotOptions), -- Open screenshot app

    -- Open apps
    ("M-M1-e", spawn emacsClient), 
    -- ("M-M1-e", spawn emacs), 
    ("M-M1-d", spawn doomEmacs), 
    ("M-b", spawn browser), -- Windows + g (meta key is windows key)
    ("M-n", spawn explorer), -- open explorer
    ("M-<Return>", spawn myTerminal), -- Spawn terminal
    ("M-<Space>", spawn "rofi -modi window,drun,run -show drun -show-icons -terminal termite "),

    -- Kill windows
    ("M-S-q", kill), -- Kill Current window
    ("M-S-a", closeAllWindows), -- Kill all windows on current workspace
    -- ("M-S-e", quitWithWarning), -- Kill all windows on current workspace
    ("M-S-e", quitWithWarning), -- Kill all windows on current workspace
    ("M-S-r", spawn "xmonad --recompile; xmonad --restart"), -- Kill all windows on current workspace
    ("<XF86PowerDown>", powerOffWithWarning), -- Kill all windows on current workspace
    ("<XF86PowerOff>", powerOffWithWarning), -- Kill all windows on current workspace
    -- Layouts
    ("M-<Tab>", sendMessage NextLayout), -- Rotate through the available layout algorithms
    ("M-x", sendMessage $ Toggle MIRROR), -- Mirror current layout
    ("M-z", sendMessage (XMonad.Layout.MultiToggle.Toggle REFLECTX)),
    ("M-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts), -- Toggle fullscreen

    -- Window resizing
    ("M-M1-j", sendMessage MirrorShrink), -- Shrink vert window width
    ("M-M1-h", sendMessage Shrink), -- Shrink horiz window width
    ("M-M1-l", sendMessage Expand), -- Expand horiz window width
    ("M-M1-k", sendMessage MirrorExpand), -- Expand vert window width

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    ("M-S-'", spawn ("echo \"" ++ help ++ "\" | yad --list --column 'Commands'")),
    ("M-l", windowGo R False),
    ("M-h", windowGo L False),
    ("M-k", windowGo U False),
    ("M-j", windowGo D False),
    ("M-S-m", promote), -- Moves focused window to master, others maintain order

    -- Swap adjacent windows
    ("M-S-l", windowSwap R False),
    ("M-S-h", windowSwap L False),
    ("M-S-k", windowSwap U False),
    ("M-S-j", windowSwap D False),
    ("M-S-<Tab>", rotSlavesDown), -- Rotate all windows except master and keep focus in place
    ("M-C-<Tab>", rotAllDown), -- Rotate all the windows in the current stack

    -- Increase, decrease windows in stack
    ("M-S-<Up>", sendMessage (IncMasterN 1)), -- Increase # of clients master pane
    ("M-S-<Down>", sendMessage (IncMasterN (-1))), -- Decrease # of clients master pane

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
    ("M-C-h", sendMessage $ pullGroup L),
    ("M-C-l", sendMessage $ pullGroup R),
    ("M-C-k", sendMessage $ pullGroup U),
    ("M-C-j", sendMessage $ pullGroup D),
    ("M-C-m", withFocused (sendMessage . MergeAll)),
    ("M-C-u", withFocused (sendMessage . UnMerge)),
    ("M-C-7", withFocused (sendMessage . UnMergeAll)),
    ("M-.", onGroup W.focusUp'), -- Switch focus to next tab
    ("M-,", onGroup W.focusDown'), -- Switch focus to prev tab
    ("M-s", allNamedScratchpadAction scratchpads "Spotify"),
    ("M-c", namedScratchpadAction scratchpads "SpeedCrunch"),
    ("M-d", namedScratchpadAction scratchpads "Discord"),
    ("M-e", namedScratchpadAction scratchpads "Emacs"),
    ("M-g", namedScratchpadAction scratchpads "ChatGPT"),
    ("M-m", namedScratchpadAction scratchpads "Thunderbird"),
    ("M-t", namedScratchpadAction scratchpads "Kitty"),
    ("M-w", namedScratchpadAction scratchpads "WhatsApp"),
    ("M-S-t", withFocused $ windows . W.sink)

    -- Window navigation  (without2Dnav)
    -- ("M-S-m", windows W.swapMaster), -- Swap the focused window and the master window
    -- ,("M-m", windows W.focusMaster) -- Move focus to the master window
    -- ,("M-j", windows W.focusDown) -- Move focus to the next window
    -- ,("M-k", windows W.focusUp) -- Move focus to the prev window
    -- ,("M-S-j", windows W.swapDown) -- Swap focused window with next window
    -- ,("M-S-k", windows W.swapUp) -- Swap focused window with prev window
  ]

base03 :: String
base03 = "#002b36"

base02 :: String
base02 = "#073642"

base01 :: String
base01 = "#586e75"

base00 :: String
base00 = "#657b83"

base0 :: String
base0 = "#839496"

base1 :: String
base1 = "#93a1a1"

base2 :: String
base2 = "#eee8d5"

base3 :: String
base3 = "#fdf6e3"

yellow :: String
yellow = "#b58900"

orange :: String
orange = "#cb4b16"

red :: String
red = "#dc322f"

magenta :: String
magenta = "#d33682"

violet :: String
violet = "#6c71c4"

blue :: String
blue = "#268bd2"

cyan :: String
cyan = "#2aa198"

green :: String
green = "#859900"

brightgrey :: String
brightgrey = "#CCCCCC"

white :: String
white = "#FFFFFF"

barColor = "#2f343f"


main :: IO ()
main = do
  -- Execute xmobar with its config and pipe xmonad output to xmobar
  xmproc <- spawnPipe "xmobar .config/xmobar/xmobarrc"
  -- forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do
  --   safeSpawn "mkfifo" ["/tmp/" ++ file]

  xmonad .
    withNavigation2DConfig myNav2DConf .
      addEwmhWorkspaceSort (pure myFilterEwmh) . ewmhFullscreen $ -- so that ewmh treats scratchpads correctly
        docks $
            desktopConfig -- docks is supposed to avoid overlapping windows with dock
          { -- simple stuff
            terminal = myTerminal,
            focusFollowsMouse = myFocusFollowsMouse,
            clickJustFocuses = myClickJustFocuses,
            borderWidth = myBorderWidth,
            modMask = myModMask,
            workspaces = myWorkspaces,
            normalBorderColor = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            -- key bindings
            keys = myKeys,
            mouseBindings = myMouseBindings,
            -- hooks, layouts
            layoutHook = myLayout,
            -- manageDocks with trayer allows tray to not be focused like a window and be on all desktops instead of only on the first
            -- insertposition Above newer puts new windows on top (we want this for floating)
            -- insertposition Below Newer puts new windows below (we want this for tiled)
            manageHook = manageSpawn <+> myManageHook <+> namedScratchpadManageHook scratchpads <+> manageDocks,
            handleEventHook = myEventHook,

            -- polybar
            -- logHook = eventLogHook ,

            -- xmobar
            logHook =
              dynamicLogWithPP . myFilterPP $ -- filter out scratchpads
              myXmobarPP xmproc,

            startupHook = myStartupHook
          }
        `additionalKeysP` myEmacsKeys


eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = "" ++ ws ++ ""
        sort' = sortBy (compare `on` (!! 0))

myXmobarPP xmproc =
  xmobarPP
    { ppOutput = hPutStrLn xmproc,
      ppCurrent = xmobarColor "#98be65" "" . wrap "[" "]", -- Current workspace
      ppVisible = xmobarColor "#98be65" "" . clickable, -- Visible but not current workspace
      ppHidden = xmobarColor "#82AAFF" "" . clickable, -- Hidden workspaces
      ppHiddenNoWindows = xmobarColor "#c792ea" "" . clickable, -- Hidden workspaces (no windows)
      ppTitle = xmobarColor "#b3afc2" "" . shorten 60, -- Title of active window
      ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!", -- Urgent workspace
      -- bright grey
      ppLayout = xmobarColor white "",
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t], -- order of things in xmobar
      ppSep = xmobarColor white barColor "  :  ",
      ppWsSep = " ",
      -- ppSort =
      --   -- fmap
      --     -- (namedScratchpadFilterOutWorkspace .)
      --     (ppSort def),
      ppExtras = []
    }

-- This differentiation is needed because filterOutWs and filterOutWsPP lends different types, that way you can't use them interchangably and need two separate filters.
myFilterEwmh = filterOutWs filterList
myFilterPP = filterOutWsPP filterList
filterList = [scratchpadWorkspaceTag]

-- myFilter = filterOutWs [scratchpads]
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--

-- | Finally, a copy of the default bindings in simple textual tabular format.
-- using & doesn't seem to work too well
help :: String
help =
  unlines
    [ "The default modifier key is 'windows'. Default keybindings:",
      "",
      "-- launching and killing programs",
      "mod-Enter        Launch Terminal",
      "mod-Space        Launch app launcher",
      "mod-b            Launch Browser",
      "mod-n            Launch Explorer",
      "mod-c            Launch Floating Calculator",
      "mod-d            Launch Floating Discord",
      "mod-e            Launch Floating Emacs",
      "mod-g            Launch Floating ChatGPT",
      "mod-m            Launch Floating Email Client",
      "mod-s            Launch Floating Spotify",
      "mod-t            Launch Floating Terminal",
      "mod-w            Launch Floating Whatsapp",
      "mod-Alt+e        Launch emacsClient",
      "mod-Alt+d        Launch doomEmacs",
      "mod-Shift-q      Close/kill the focused window",
      "",
      "-- layouts",
      "mod-Shift-Space  Reset the layouts on the current workSpace to default",
      "mod-f            Toggle fullscreen",
      "",
      "-- floating layer support",
      "mod-t  Push window back into tiling; unfloat and re-tile it",
      "",
      "-- move focus",
      "mod-Tab          Next layout",
      "mod-Shift-Tab    Rotate all windows except master",
      "mod-j            Move focus to the window below",
      "mod-k            Move focus to the window above",
      "mod-h            Move focus to the left window",
      "mod-l            Move focus to the right window",
      "",
      "-- modifying the window order",
      "mod-Shift-m        Swap focused window with master",
      "mod-Shift-j  Swap the focused window with the next window",
      "mod-Shift-h  Swap the focused window with the left window",
      "mod-Shift-l  Swap the focused window with the right window",
      "mod-Shift-k  Swap the focused window with the previous window",
      "",
      "-- resizing the master/slave ratio",
      "mod-Alt-j  Shrink the master area",
      "mod-Alt-h  Shrink the master area",
      "mod-Alt-k  Expand the master area",
      "mod-Alt-l  Expand the master area",
      "",
      "-- quit, or restart",
      "mod-Shift-e  Quit xmonad",
      "mod-Shift-r  Restart xmonad",
      "mod-[1..9]   Switch to workSpace N",
      "",
      "-- Workspaces and screens",
      "mod-Shift-[1..9]   Move client to workspace N",
      -- "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
      -- "mod-Shift-{w}  Move client to screen 1, 2, or 3",
      "",
      "-- Mouse bindings: default actions bound to mouse events",
      "mod-button1  Set the window to floating mode and move by dragging",
      "mod-button2  Set the window to floating mode and Resize the window",
      "",
      "-- Screenshots",
      "print            Take a screenshot",
      "mod+print        Open the screenshot application"
      -- "",
      -- "-- Multimedia keys",
      -- "",
      -- "-- Brightness keys",
      -- "",
      -- "-- Volume keys"
    ]
