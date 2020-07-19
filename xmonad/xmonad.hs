
-- imports{{{
import           Control.Arrow                      (first)
import           Data.Monoid
import           System.Exit
import           XMonad                             hiding ((|||))
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.NoBorders            (noBorders, smartBorders)
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Prompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Man
import           XMonad.Prompt.Shell
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

import qualified Data.Map                           as M
import qualified XMonad.StackSet                    as W
-- }}}

-- variables{{{
myModMask       = mod4Mask
myTerminal      = "kitty --single-instance"
myFont = "xft:Monoid Nerd Font Mono:pixelsize=14:antialias=true:hinting=false"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth   = 5
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
myNormalBorderColor  = "#a6accd"
myFocusedBorderColor = "#c792ea"
myGaps = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
-- }}}

-- keybindings{{{
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $

    [ ((modm,                 xK_Return ), spawn $ XMonad.terminal conf)
    , ((modm,                 xK_d      ), shellPrompt myXPConfig)                  -- use xmonad prompt instead of dmenu.
    , ((modm .|. controlMask, xK_t      ), namedScratchpadAction myScratchPads "terminal") -- terminal scratchpad
    , ((modm .|. controlMask, xK_s      ), namedScratchpadAction myScratchPads "mixer") -- sound mixer scratchpad
    , ((modm .|. controlMask, xK_h      ), namedScratchpadAction myScratchPads "htop")  -- htop scratchpad
    , ((modm .|. controlMask, xK_n      ), namedScratchpadAction myScratchPads "nnn")   -- file manager scratchpad
    , ((modm .|. controlMask, xK_m      ), manPrompt myXPConfig)
    , ((modm,                 xK_b      ), spawn "qutebrowser")                     -- launch qutebrowser
    , ((modm,                 xK_p      ), spawn "touch ~/.cache/pomodoro_session") -- start pomodoro
    , ((modm .|. shiftMask,   xK_p      ), spawn "rm ~/.cache/pomodoro_session")    -- stop pomodoro
    , ((0                 ,   0x1008ff11), spawn "amixer -q sset Master 2%-")       -- decrease volume
    , ((0                 ,   0x1008ff13), spawn "amixer -q sset Master 2%+")       -- increase volume
    , ((0                 ,   0x1008FF12), spawn "amixer set Master toggle")        -- mute/unmute sound
    , ((modm .|. shiftMask,   xK_c      ), kill)
    , ((modm,                 xK_space  ), sendMessage NextLayout)
    , ((modm,                 xK_t      ), sendMessage $ JumpToLayout "Spacing Tall")
    , ((modm,                 xK_f      ), sendMessage $ JumpToLayout "Full")
    , ((modm,                 xK_m      ), sendMessage $ JumpToLayout "Mirror Spacing Tall")
    , ((modm,                 xK_n      ), sendMessage $ JumpToLayout "Spacing BSP")
    , ((modm,                 xK_s      ), sendMessage $ JumpToLayout "Spacing Spiral")
    , ((modm .|. shiftMask,   xK_space  ), setLayout $ XMonad.layoutHook conf)
    , ((modm,                 xK_r      ), refresh)
    , ((modm,                 xK_j      ), windows W.focusDown)
    , ((modm,                 xK_k      ), windows W.focusUp  )
    , ((modm,                 xK_g      ), windows W.focusMaster)
    , ((modm .|. shiftMask,   xK_g      ), windows W.swapMaster)
    , ((modm .|. shiftMask,   xK_j      ), windows W.swapDown  )
    , ((modm .|. shiftMask,   xK_k      ), windows W.swapUp    )
    , ((modm,                 xK_h      ), sendMessage Shrink)
    , ((modm,                 xK_l      ), sendMessage Expand)
    , ((modm              ,   xK_comma  ), sendMessage (IncMasterN 1))
    , ((modm              ,   xK_period ), sendMessage (IncMasterN (-1)))
    , ((modm              ,   xK_comma  ), sendMessage Swap)
    , ((modm              ,   xK_period ), sendMessage Rotate)
    , ((modm              ,   xK_o      ), spawn "light-locker-command --lock")
    , ((modm .|. shiftMask,   xK_q      ), io exitSuccess)
    , ((modm              ,   xK_q      ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- }}}

-- mouse bindings{{{
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
-- }}}

-- prompt keybindings{{{
myXPKeymap = M.fromList $
        map (first $ (,) 0)
        [ (xK_Return, setSuccess True >> setDone True)
        , (xK_KP_Enter, setSuccess True >> setDone True)
        , (xK_BackSpace, deleteString Prev)
        , (xK_Delete, deleteString Prev)
        , (xK_Left, moveCursor Prev)
        , (xK_Right, moveCursor Next)
        , (xK_Down, moveHistory W.focusUp')
        , (xK_Up, moveHistory W.focusDown')
        , (xK_Escape, quit)
        ]
-- }}}

-- prompt config{{{
myXPConfig = def
        { font = myFont
        , bgColor = "#202331"
        , fgColor = "#676E95"
        , bgHLight = "#444267"
        , fgHLight = "#A6ACCD"
        , borderColor = "#2b2a3e"
        , promptKeymap = myXPKeymap
        , promptBorderWidth = 0
        , position = Top
        , height = 22
        , autoComplete = Nothing
        , searchPredicate = fuzzyMatch
        , alwaysHighlight = True
        }
-- }}}

-- layout{{{
myLayout = avoidStruts $ smartBorders (tiledgaps ||| bspgaps ||| Mirror tiledgaps ||| spiralgaps ||| noBorders Full)
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
-- }}}

-- scratchpads{{{
myScratchPads = [ NS "terminal" spawnTerm  findTerm  manageTerm
                , NS "mixer"    spawnMixer findMixer manageMixer
                , NS "htop"     spawnhtop  findhtop  managehtop
                , NS "nnn"      spawnnnn   findnnn   managennn
                ]
    where
        centralh    = 0.9
        centralw    = 0.95
        centralt    = 0.95 - centralh
        centrall    = 0.95 - centralw

        spawnTerm   = myTerminal ++ " --name=terminalScratchpad"
        findTerm    = resource   =? "terminalScratchpad"
        manageTerm  = customFloating $ W.RationalRect l t w h
            where
                h   = 0.3
                w   = 1
                t   = 0
                l   = (1-w)/2

        spawnMixer  = myTerminal ++ " --name=mixerScratchpad" ++ " -e ncpamixer"
        findMixer   = resource   =? "mixerScratchpad"
        manageMixer = customFloating $ W.RationalRect centrall centralt centralw centralh

        spawnhtop   = myTerminal ++ " --name=htopScratchpad"  ++ " -e htop"
        findhtop    = resource   =? "htopScratchpad"
        managehtop  = customFloating $ W.RationalRect centrall centralt centralw centralh

        spawnnnn    = myTerminal ++ " --name=nnnScratchpad"   ++ " -e nnn -a"
        findnnn     = resource   =? "nnnScratchpad"
        managennn   = customFloating $ W.RationalRect centrall centralt centralw centralh
-- }}}

-- managehook{{{
myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore] <+> namedScratchpadManageHook myScratchPads
-- }}}

myEventHook = mempty
myLogHook =  return()

-- startuphook{{{
myStartupHook = do
    spawnOnce "feh --bg-fill /etc/wallpapers/default.png &"
    spawnOnce "fcitx &"
    spawnOnce "light-locker &"
-- }}}


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
            logHook            =  myLogHook <+> dynamicLogWithPP xmobarPP
                                { ppOutput = hPutStrLn xmproc
                                , ppCurrent = xmobarColor "#ab47bc" "" .wrap "[" "]" -- Current workspace
                                , ppVisible = xmobarColor  "#414863" ""              -- workspace visible
                                , ppLayout = xmobarColor "#82aaff" ""
                                , ppSep = " \63192 "
                                , ppTitle = mempty
                                },
            startupHook        = myStartupHook
    };

-- vim: foldmethod=marker:
