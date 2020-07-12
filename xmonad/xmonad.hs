
import           Data.Monoid
import           System.Exit
import           XMonad
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.Spacing
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

import qualified Data.Map                 as M
import qualified XMonad.StackSet          as W

myModMask       = mod4Mask
myTerminal      = "kitty"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth   = 3
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#fefefe"
myFocusedBorderColor = "#f07178"

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_d     ), spawn "dmenu_run")
    , ((modm,               xK_b     ), spawn "qutebrowser")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_n     ), refresh)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_m     ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm .|. shiftMask, xK_q     ), io exitSuccess)
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
    where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = spacingRaw True (Border 6 6 6 6) True (Border 6 6 6 6) True $ Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100


myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore]

myEventHook = mempty
myLogHook = return ()

myStartupHook = do
    spawnOnce "feh --bg-fill ~/Pictures/nix-wallpaper-dracula.png"
    spawnOnce "picom &"
    spawnOnce "fcitx &"

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"

    xmonad $ docks def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
