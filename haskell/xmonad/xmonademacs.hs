
-- imports{{{
import           Control.Arrow                  ( first )
import           System.Exit
import           XMonad                  hiding ( (|||) )
import           XMonad.Actions.Navigation2D
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.LayoutCombinators
import           XMonad.Layout.Spacing
import           XMonad.Layout.Spiral
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt
import           XMonad.Prompt.FuzzyMatch
import           XMonad.Prompt.Man
import           XMonad.Prompt.Shell
import           XMonad.Prompt.Unicode
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run
import           XMonad.Util.SpawnOnce

import qualified Data.Map                      as M
import qualified XMonad.StackSet               as W
-- }}}

-- variables{{{

-- type declaration here
myModMask :: KeyMask
myTerminal :: [Char]
myFont :: [Char]
myEmojiFont :: [Char]
myPromptHeight :: Dimension
myBorderWidth :: Dimension
myWorkspaces :: [[Char]]
myNormalBorderColor :: [Char]
myFocusedBorderColor :: [Char]

myModMask = mod4Mask
myTerminal = "alacritty"
myFont = "xft:TamzenForPowerline:size=14:antialias=true:hinting=false"
myEmojiFont = "xft:Apple Color Emoji:size=14"
myPromptHeight = 30
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth = 2
myWorkspaces =
  [ "\8203"
  , "\8203\8203"
  , "\8203\8203\8203"
  , "\8203\8203\8203\8203"
  , "\8203\8203\8203\8203\8203"
  , "\8203\8203\8203\8203\8203\8203"
  , "\8203\8203\8203\8203\8203\8203\8203"
  , "\8203\8203\8203\8203\8203\8203\8203\8203"
  , "\8203\8203\8203\8203\8203\8203\8203\8203\8203"
  ] -- zero width spaces
myNormalBorderColor = "#2b2a3e"
myFocusedBorderColor = "#c792ea"
myGaps = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
-- }}}

-- keybindings{{{
myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
       , ((modm, xK_v)     , spawn $ myTerminal ++ " -e nvim")
       , ((modm, xK_z), spawn $ myTerminal ++ " -e nvim -c term -c startinsert")
       , ((modm, xK_w)     , spawn "emacsclient -c")
       , ( (modm, xK_d)
         , shellPrompt myXPConfig
         )                  -- use xmonad prompt instead of dmenu.
       , ( (modm .|. controlMask, xK_t)
         , namedScratchpadAction myScratchPads "terminal"
         ) -- terminal scratchpad
       , ( (modm .|. controlMask, xK_s)
         , namedScratchpadAction myScratchPads "mixer"
         ) -- sound mixer scratchpad
       , ( (modm .|. controlMask, xK_h)
         , namedScratchpadAction myScratchPads "ytop"
         )  -- ytop scratchpad
       , ( (modm .|. controlMask, xK_n)
         , namedScratchpadAction myScratchPads "vifm"
         )   -- file manager scratchpad
       , ( (modm .|. controlMask, xK_m)
         , manPrompt myXPConfig
         ) -- search manpage
       , ( (modm .|. controlMask, xK_e)
         , mkUnicodePrompt "xsel"
                           ["-b"]
                           "/home/btw/textfiles/UnicodeData.txt"
                           myEmojiXPConfig
         ) -- emoji->clipboard
       , ( (modm, xK_b)
         , spawn "qutebrowser"
         )                     -- launch qutebrowser
       , ( (modm, xK_p)
         , spawn "touch ~/.cache/pomodoro_session"
         ) -- start pomodoro
       , ( (modm .|. shiftMask, xK_p)
         , spawn "rm ~/.cache/pomodoro_session"
         )    -- stop pomodoro
       , ( (0, 0x1008ff11)
         , spawn "amixer -q sset Master 2%-"
         )       -- decrease volume fn+a(HHKB Dvorak)
       , ( (0, 0x1008ff13)
         , spawn "amixer -q sset Master 2%+"
         )       -- increase volume fn+o(HHKB Dvorak)
       , ( (0, 0x1008FF12)
         , spawn "amixer set Master toggle"
         )        -- mute/unmute sound fn+e(HHKB Dvorak)
       , ( (0, 0x1008ff02)
         , spawn "xbacklight -inc 5"
         )        -- increase brightness
       , ( (0, 0x1008ff03)
         , spawn "xbacklight -dec 5"
         )        -- increase brightness
       , ( (0, xK_Print)
         , spawn "scrot screen_%Y-%m-%d-%H-%M-%S.png -e 'mv $f ~/Pictures/'"
         ) -- fn+c(HHKB Dvorak)
       , ( (0 .|. controlMask, xK_Print)
         , spawn "scrot -s screen_%Y-%m-%d-%H-%M-%S.png -e 'mv $f ~/Pictures/'"
         ) -- ctrl+fn+c(HHKB Dvorak)
       , ( (modm, xK_Print)
         , spawn "scrot tmp.png -e 'xclip $f && rm $f'"
         ) -- mod+fn+c(HHKB Dvorak)
       , ( (modm, xK_F1)
         , spawn "setxkbmap dvorak"
         ) -- Switch to Dvorak layout
       , ( (modm, xK_F2)
         , spawn "setxkbmap us"
         ) -- Switch to qwerty layout
       , ( (modm, xK_F3)
         , spawn "xinput --disable 11"
         ) -- Disable trackpad
       , ( (modm, xK_F4)
         , spawn "xinput --enable 11"
         ) -- Enable trackpad
       , ((modm .|. shiftMask, xK_c), kill)
       , ((modm, xK_space)          , sendMessage NextLayout)
       , ((modm, xK_t), sendMessage $ JumpToLayout "Spacing Tall")
       , ((modm, xK_f)              , sendMessage $ JumpToLayout "Full")
       , ((modm, xK_m), sendMessage $ JumpToLayout "Mirror Spacing Tall")
       , ((modm, xK_n), sendMessage $ JumpToLayout "Spacing BSP")
       , ((modm, xK_s), sendMessage $ JumpToLayout "Spacing Spiral")
       , ( (modm .|. shiftMask, xK_t)
         , withFocused $ windows . W.sink
         ) -- unfloat window
       , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
       , ((modm, xK_r)                  , refresh)
       , ( (modm, xK_h)
         , windowGo L False
         ) -- Focus horizontally like i3wm. it's useful in some layouts
       , ( (modm, xK_l)
         , windowGo R False
         ) -- ⬆
       , ((modm, xK_j)              , windows W.focusDown)
       , ((modm, xK_k)              , windows W.focusUp)
       , ((modm, xK_g)              , windows W.focusMaster)
       , ((modm .|. shiftMask, xK_g), windows W.swapMaster)
       , ((modm .|. shiftMask, xK_j), windows W.swapDown)
       , ((modm .|. shiftMask, xK_k), windows W.swapUp)
       , ((modm .|. shiftMask, xK_h), sendMessage Shrink)
       , ( (modm .|. shiftMask, xK_l)
         , sendMessage Expand
         )
    --, ((modm              ,   xK_comma  ), sendMessage (IncMasterN 1))
    --, ((modm              ,   xK_period ), sendMessage (IncMasterN (-1)))
       , ( (modm, xK_comma)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "Spacing BSP" -> sendMessage Swap
             _             -> sendMessage (IncMasterN 1)
         )
       , ( (modm, xK_period)
         , do
           layout <- getActiveLayoutDescription
           case layout of
             "Spacing BSP" -> sendMessage Rotate
             _             -> sendMessage (IncMasterN (-1))
         )
       , ((modm, xK_o), spawn "light-locker-command -l")
       , ( (modm .|. shiftMask, xK_q)
         , confirmPrompt myXPConfig "exit" $ io exitSuccess
         )
       , ((modm, xK_q), spawn "xmonad --recompile; xmonad --restart")
       ]
    ++ [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
       ]
-- }}}

-- mouse bindings{{{
myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

    -- mod-button2, Raise the window to the top of the stack
  , ( (modm, button2)
    , \w -> focus w >> windows W.shiftMaster
    )

    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
-- }}}

-- prompt keybindings{{{
myXPKeymap =
  M.fromList
    $  map
         (first $ (,) 0)
         [ (xK_Return   , setSuccess True >> setDone True)
         , (xK_KP_Enter , setSuccess True >> setDone True)
         , (xK_BackSpace, deleteString Prev)
         , (xK_Delete   , deleteString Prev)
         , (xK_Left     , moveCursor Prev)
         , (xK_Right    , moveCursor Next)
         , (xK_Down     , moveHistory W.focusUp')
         , (xK_Up       , moveHistory W.focusDown')
         , (xK_Escape   , quit)
         ]
    ++ map (first $ (,) controlMask) [(xK_v, pasteString)]
-- }}}

-- prompt config{{{
myXPConfig = def { font              = myFont
                 , bgColor           = "#232635"
                 , fgColor           = "#A6ACCD"
                 , bgHLight          = "#444267"
                 , fgHLight          = "#A6ACCD"
                 , borderColor       = "#2b2a3e"
                 , promptKeymap      = myXPKeymap
                 , promptBorderWidth = 0
                 , position          = Top
                 , height            = myPromptHeight
                 , autoComplete      = Nothing
                 , searchPredicate   = fuzzyMatch
                 , alwaysHighlight   = True
                 }

myEmojiXPConfig = def { font              = myEmojiFont
                      , bgColor           = "#232635"
                      , fgColor           = "#A6ACCD"
                      , bgHLight          = "#444267"
                      , fgHLight          = "#A6ACCD"
                      , borderColor       = "#2b2a3e"
                      , promptKeymap      = myXPKeymap
                      , promptBorderWidth = 0
                      , position          = Top
                      , height            = myPromptHeight
                      , autoComplete      = Nothing
                      , searchPredicate   = fuzzyMatch
                      , alwaysHighlight   = True
                      }
-- }}}

-- layout{{{
myLayout = avoidStruts $ smartBorders
  (tiledgaps ||| bspgaps ||| Mirror tiledgaps ||| spiralgaps ||| Full)
 where
  tiledgaps  = myGaps $ Tall nmaster delta ratio

  -- window number in master pane
  nmaster    = 1

  -- percent of screen to increment by when resizing panes
  delta      = 2 / 100

  -- default proportion of screen occupied by master pane
  ratio      = 1 / 2

  bspgaps    = myGaps emptyBSP
  spiralgaps = myGaps $ spiral (6 / 7)
-- }}}

-- scratchpads{{{
myScratchPads =
  [ NS "terminal" spawnTerm  findTerm  manageTerm
  , NS "mixer"    spawnMixer findMixer manageMixer
  , NS "ytop"     spawnytop  findytop  manageytop
  , NS "vifm"     spawnvifm  findvifm  managevifm
  ]
 where
  centralh   = 0.9
  centralw   = 0.9
  centralt   = 0.95 - centralh
  centrall   = 0.95 - centralw

  spawnTerm  = myTerminal ++ " --title=terminalScratchpad"
  findTerm   = title =? "terminalScratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h
   where
    h = 0.3
    w = 1
    t = 0
    l = (1 - w) / 2

  spawnMixer = myTerminal ++ " --title=mixerScratchpad" ++ " -e ncpamixer"
  findMixer  = title =? "mixerScratchpad"
  manageMixer =
    customFloating $ W.RationalRect centrall centralt centralw centralh

  spawnytop = myTerminal ++ " --title=ytopScratchpad" ++ " -e ytop"
  findytop  = title =? "ytopScratchpad"
  manageytop =
    customFloating $ W.RationalRect centrall centralt centralw centralh

  spawnvifm =
    myTerminal ++ " --title=vifmScratchpad" ++ " -e bash -c 'vifmrun'"
  findvifm = title =? "vifmScratchpad"
  managevifm =
    customFloating $ W.RationalRect centrall centralt centralw centralh
-- }}}

-- managehook{{{
myManageHook =
  composeAll
      [ className =? "Gimp" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , isFullscreen --> doFullFloat
      ]
    <+> namedScratchpadManageHook myScratchPads
-- }}}

-- loghook{{{
myLogHook h = dynamicLogWithPP xmobarPP
  { ppOutput          = hPutStrLn h
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort def) -- hide nsp
  , ppCurrent         = xmobarColor "#c792ea" "" . wrap "\61713" " "  -- Current workspace
  , ppVisible         = xmobarColor "#ab47bc" "" . wrap "\61842" " "
  , ppHidden          = xmobarColor "#ab47bc" "" . wrap "\61842" " "
  , ppHiddenNoWindows = xmobarColor "#FFFFFF" "" . wrap "\61915" " "
  , ppLayout          = xmobarColor "#82aaff" ""
  , ppSep             = " | "
  , ppTitle           = mempty
  }
-- }}}

-- startuphook{{{
myStartupHook = do
    -- set default wallpaper
  spawnOnce "feh --bg-fill /etc/wallpapers/wallpaper1.png &"
  -- now set custom wallpaper if config exists
  spawnOnce "nitrogen --restore &"
  -- spawn Japanese IME
  spawnOnce "fcitx -d &"
  -- start screen locker
  spawnOnce "light-locker --lock-on-suspend &"
  -- window animation
  spawnOnce "flashfocus &"
-- }}}

myEventHook = handleEventHook def <+> fullscreenEventHook

-- functions {{{

getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
  workspaces <- gets windowset
  return $ description . W.layout . W.workspace . W.current $ workspaces

-- }}}

main :: IO ()
main = do
  h <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ docks $ withNavigation2DConfig def $ ewmh
    def { handleEventHook = handleEventHook def <+> fullscreenEventHook } {
      -- simple stuff
                                                                            terminal = myTerminal
                                                                          , focusFollowsMouse = myFocusFollowsMouse
                                                                          , clickJustFocuses = myClickJustFocuses
                                                                          , borderWidth = myBorderWidth
                                                                          , modMask = myModMask
                                                                          , workspaces = myWorkspaces
                                                                          , normalBorderColor = myNormalBorderColor
                                                                          , focusedBorderColor = myFocusedBorderColor
      -- key bindings
                                                                          , keys = myKeys
                                                                          , mouseBindings = myMouseBindings
      -- hooks, layouts
                                                                          , layoutHook = myLayout
                                                                          , manageHook = myManageHook
                                                                          , handleEventHook = myEventHook
                                                                          , logHook = myLogHook
                                                                            h
                                                                          , startupHook = myStartupHook
                                                                          }

-- vim: ft=haskell sw=4 fdm=marker:
