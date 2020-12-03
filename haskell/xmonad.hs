-- This file is generated from "README.org"
import           Control.Arrow                  ( first )
import qualified Data.Map                      as M
import           System.Exit                    ( exitSuccess )
import           XMonad                  hiding ( (|||) )
import           XMonad.Actions.Navigation2D    ( Direction2D(L, R)
                                                , windowGo
                                                , withNavigation2DConfig
                                                )
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                )
import           XMonad.Hooks.ManageHelpers     ( doFullFloat
                                                , isFullscreen
                                                )
import           XMonad.Layout.BinarySpacePartition
                                                ( emptyBSP
                                                , Rotate(Rotate)
                                                , Swap(Swap)
                                                )
import           XMonad.Layout.LayoutCombinators
                                                ( (|||)
                                                , JumpToLayout(JumpToLayout)
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( spacingRaw
                                                , Border(Border)
                                                )
import           XMonad.Layout.Spiral           ( spiral )
import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt    ( confirmPrompt )
import           XMonad.Prompt.FuzzyMatch       ( fuzzyMatch )
import           XMonad.Prompt.Man              ( manPrompt )
import           XMonad.Prompt.Shell            ( shellPrompt )
import           XMonad.Prompt.Unicode          ( mkUnicodePrompt )
import qualified XMonad.StackSet               as W
import           XMonad.Util.Cursor             ( setDefaultCursor )
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                ( hPutStrLn
                                                , spawnPipe
                                                )
import           XMonad.Util.SpawnOnce          ( spawnOnce )
import           XMonad.Util.EZConfig           ( additionalKeysP )
myModMask :: KeyMask
myModMask = mod4Mask
myTerminal :: String
myTerminal = "alacritty"
myFont :: String
myFont = "xft:Liberation Sans:size=14"
myEmojiFont :: String
myEmojiFont = "xft:Apple Color Emoji:size=14"
myWorkspaces :: [String]
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
  ]
myBorderWidth :: Dimension
myBorderWidth = 2
myPromptHeight :: Dimension
myPromptHeight = 30
myNormalBorderColor :: String
myNormalBorderColor = "#2b2a3e"
myFocusedBorderColor :: String
myFocusedBorderColor = "#c792ea"
myGaps = spacingRaw False (Border 4 4 4 4) True (Border 4 4 4 4) True
myKeys :: [(String, X ())]
myKeys =
  [
    ("M-<Return>", spawn myTerminal)
  , ("M-S-c", kill) -- Close focused application
  , ("M-o", spawn "light-locker-command -l") -- lock screen
  , ("M-S-q", confirmPrompt myXPConfig "exit" $ io exitSuccess) -- prompt to kill xmonad
  , ("M-q", spawn "xmonad --recompile; xmonad --restart") -- Recompile and restart xmonad

  , ("M-v", spawn $ myTerminal ++ " -e nvim")
  , ("M-z", spawn "emacsclient -c -a emacs ~/")
  , ("M-w", spawn "emacsclient -c -a emacs")
  , ("M-b", spawn "qutebrowser")
  , ("M-e", spawn "~/.emacs_anywhere/bin/run")

  , ("M-C-t", namedScratchpadAction myScratchPads "terminal")
  , ("M-C-s", namedScratchpadAction myScratchPads "mixer")
  , ("M-C-h", namedScratchpadAction myScratchPads "bottom")
  , ("M-C-n", namedScratchpadAction myScratchPads "vifm")

  , ("M-d", shellPrompt myXPConfig)
  , ("M-C-m", manPrompt myXPConfig)
  , ("M-C-e", mkUnicodePrompt "xsel" ["-b"] "/etc/UnicodeData.txt" myEmojiXPConfig) -- copy emoji to clipboard

  , ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 2%-") -- fn+a on HHKB Dvorak
  , ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 2%+") -- fn+o on HHKB Dvorak
  , ("<XF86AudioMute>", spawn "amixer set Master toggle") -- fn+e on HHKB Dvorak

  , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")

  , ("C-<Print>", spawn "scrot -s screen_%Y-%m-%d-%H-%M-%S.png -e 'mv $f ~/Pictures/'") -- ctrl+fn+c on HHKB Dvorak
  , ("M-<Print>", spawn "scrot tmp.png -e 'xclip $f && rm $f'") -- mod+fn+c on HHKB Dvorak

  , ("M-S-<F1>", spawn "setxkbmap dvorak") -- Switch to Dvorak layout
  , ("M-S-<F2>", spawn "setxkbmap us") -- Switch to qwerty layout
  , ("M-S-<F3>", spawn "xinput --disable 11") -- Disable trackpad on laptop
  , ("M-S-<F4>", spawn "xinput --enable 11") -- Enable trackpad on laptop

  , ("M-<Space>", sendMessage NextLayout) -- Change to next layout in order

  , ("M-t", sendMessage $ JumpToLayout "Spacing Tall")
  , ("M-f", sendMessage $ JumpToLayout "Full")
  , ("M-m", sendMessage $ JumpToLayout "Mirror Spacing Tall")
  , ("M-n", sendMessage $ JumpToLayout "Spacing BSP")
  , ("M-s", sendMessage $ JumpToLayout "Spacing Spiral")

  , ("M-S-t", withFocused $ windows . W.sink) -- unfloat window

  , ("M-r", refresh)

  -- focus horizontally like i3wm
  , ("M-h", windowGo L False)
  , ("M-l", windowGo R False)

  , ("M-j", windows W.focusDown)
  , ("M-k", windows W.focusUp)
  , ("M-g", windows W.focusMaster)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  , ("M-S-g", windows W.swapMaster)

  , ("M-S-h", sendMessage Shrink)
  , ("M-S-l", sendMessage Expand)

  , ("M-,", do
        layout <- getActiveLayoutDescription
        case layout of
          "Spacing BSP" -> sendMessage Swap
          _             -> sendMessage $ IncMasterN 1
    )
  , ("M-.", do
        layout <- getActiveLayoutDescription
        case layout of
          "Spacing BSP" -> sendMessage Rotate
          _             -> sendMessage $ IncMasterN (-1)
    )
  ]
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
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
myClickJustFocuses :: Bool
myClickJustFocuses = False

myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

    -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster
    )

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]
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
myScratchPads =
  [ NS "terminal" spawnTerm   findTerm   manageTerm
  , NS "mixer"    spawnMixer  findMixer  manageMixer
  , NS "bottom"   spawnbottom findbottom managebottom
  , NS "vifm"     spawnvifm   findvifm   managevifm
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

  spawnbottom = myTerminal ++ " --title=bottomScratchpad" ++ " -e btm"
  findbottom  = title =? "bottomScratchpad"
  managebottom =
    customFloating $ W.RationalRect centrall centralt centralw centralh

  spawnvifm =
    myTerminal ++ " --title=vifmScratchpad" ++ " -e bash -c 'vifmrun'"
  findvifm = title =? "vifmScratchpad"
  managevifm =
    customFloating $ W.RationalRect centrall centralt centralw centralh
myManageHook =
  composeAll
      [ className =? "Gimp" --> doFloat
      , resource =? "desktop_window" --> doIgnore
      , isFullscreen --> doFullFloat
      ]
    <+> namedScratchpadManageHook myScratchPads
myStartupHook = do
  spawnOnce "feh --bg-fill /etc/wallpapers/wallpaper1.png &"
  spawnOnce "nitrogen --restore &"
  setDefaultCursor xC_left_ptr
  -- spawn Japanese IME
  spawnOnce "fcitx -d &"
  -- start screen locker
  spawnOnce "light-locker --lock-on-suspend &"
  -- window animation
  spawnOnce "flashfocus &"
myLogHook h = dynamicLogWithPP xmobarPP
  { ppOutput          = hPutStrLn h
  , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort def) -- hide nsp
  , ppCurrent         = xmobarColor "#c792ea" "" . wrap "\61713" " "  -- Current workspace
  , ppVisible         = xmobarColor "#ab47bc" "" . wrap "\61842" " "
  , ppHidden          = xmobarColor "#ab47bc" "" . wrap "\61842" " "
  , ppHiddenNoWindows = xmobarColor "#FFFFFF" "" . wrap "\61915" " "
  , ppLayout          = xmobarColor "#82aaff" ""
  , ppSep             = "  |  "
  , ppTitle           = mempty
  }
myEventHook = handleEventHook def <+> fullscreenEventHook
getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
  workspaces <- gets windowset
  return $ description . W.layout . W.workspace . W.current $ workspaces

main :: IO ()
main = do
  h <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ docks $ withNavigation2DConfig def $ ewmh
    def { handleEventHook = handleEventHook def <+> fullscreenEventHook }
      {
      -- simple stuff
        terminal           = myTerminal
      , focusFollowsMouse  = myFocusFollowsMouse
      , clickJustFocuses   = myClickJustFocuses
      , borderWidth        = myBorderWidth
      , modMask            = myModMask
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      -- key bindings
      -- , keys               = myKeys
      , mouseBindings      = myMouseBindings
      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = myEventHook
      , logHook            = myLogHook h
      , startupHook        = myStartupHook
      } `additionalKeysP` myKeys
