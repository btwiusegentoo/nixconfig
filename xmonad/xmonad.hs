
import           Data.Monoid
import           System.Exit
import           XMonad                             hiding ((|||))
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

import qualified Data.Map                           as M
import qualified XMonad.StackSet                    as W

myModMask       = mod4Mask
myTerminal      = "kitty --single-instance"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth   = 2
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#fefefe"
myFocusedBorderColor = "#f07178"
myGaps = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True

myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_d     ), spawn "dmenu_run")
    , ((modm,               xK_b     ), spawn "qutebrowser")
    , ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm,               xK_t     ), sendMessage $ JumpToLayout "Spacing Tall")
    , ((modm,               xK_f     ), sendMessage $ JumpToLayout "Full")
    , ((modm,               xK_m     ), sendMessage $ JumpToLayout "Mirror Spacing Tall")
    , ((modm,               xK_n     ), sendMessage $ JumpToLayout "Spacing BSP")
    , ((modm,               xK_s     ), sendMessage $ JumpToLayout "Spacing Spiral")
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_r     ), refresh)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_g     ), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_g     ), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_comma ), sendMessage Swap)
    , ((modm              , xK_period), sendMessage Rotate)
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

myLayout = avoidStruts(tiledgaps ||| bspgaps ||| Mirror tiledgaps ||| spiralgaps ||| Full)
    where
        tiledgaps = myGaps $ Tall nmaster delta ratio

        -- window number in master pane
        nmaster = 1

        -- percent of screen to increment by when resizing panes
        delta = 2/100

        -- default proportion of screen occupied by master pane
        ratio = 1/2

        bspgaps = myGaps emptyBSP
        spiralgaps = myGaps $ spiral (6/7)


myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore]

myEventHook = mempty
myLogHook =  return()

myStartupHook = do
    spawnOnce "feh --bg-fill ~/Pictures/nix-wallpaper-dracula.png"
    spawnOnce "picom &"
    spawnOnce "fcitx &"

main = do
    xmproc <- spawnPipe "xmobar -r ~/.xmonad/xmobar.hs"
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
            logHook            =  myLogHook <+> dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc
                                , ppCurrent = xmobarColor "#ab47bc" "" .wrap "[" "]" -- Current workspace
                                , ppVisible = xmobarColor  "#676e95" ""              -- workspace visible
                                , ppTitle = mempty
                                },
            startupHook        = myStartupHook
    };
