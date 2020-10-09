import           Xmobar


config :: Config
config = defaultConfig
  { font             = "xft:Tamzen-14,GohuFont Nerd Font-12,DejaVu Sans-12"
  , additionalFonts  = []
  , borderColor      = "#232635"
  , border           = BottomB
  , alpha            = 255
  , bgColor          = "#202331"
       --, alpha = 240
       --, bgColor = "#203123" -- Somehow color get messed up when using alpha issue#246 this is the workaround. invert 3rd4th <--> 5th6th like this.
  , fgColor          = "#676E95"
  , position         = TopSize L 100 30
  , textOffset       = -1
  , iconOffset       = 13
  , lowerOnStart     = True
  , pickBroadest     = False
  , persistent       = False
  , hideOnStart      = False
  , iconRoot         = "/etc/icons"
  , allDesktops      = True
  , overrideRedirect = True
  , commands         =
    [ Run $ Cpu ["-t", "<icon=cpu.xpm/> <total>%"] 10
    , Run $ Memory ["-t", "<icon=activity.xpm/> <used>MB"] 10
    , Run $ Swap
      ["-t", "<icon=arrow-right.xpm/> <icon=hard-drive.xpm/> <used>MB"]
      10
    , Run $ Date "%a %m/%_d" "date" 10
    , Run $ Date "%H:%M:%S" "time" 10
    , Run $ Com "uname" ["-r"] "" 36000
    , Run $ CommandReader "pymodoro" "pomodoro"
    , Run $ Volume
      "default"
      "Master"
      [ "-t"
      , "<status> <volume>%"
      , "--"
      , "-O"
      , "<icon=volume-2.xpm/>"
      , -- on
        "-o"
      , "<icon=volume-x.xpm/>"
      , -- off
        "-C"
      , "#FFFFFF"
      , "-c"
      , "#f07178"
      ]
      3
    , Run StdinReader
    ]
  , sepChar          = "%"
  , alignSep         = "}{"
  , template         =
    " %StdinReader% }\
                    \ <fc=#89ddff><icon=clock.xpm/> %time%</fc> \
                    \{ <fc=#f07178>%cpu%</fc>  <fc=#80cbc4>%memory% %swap%</fc>  <fc=#f78c6c><icon=calendar.xpm/> %date%</fc>  <fc=#82aaff>%default:Master%</fc>  <fc=#f07178>\57345 %pomodoro%</fc> \
                    \ <fc=#ffcb6b><icon=NewTux.xpm/> %uname% </fc> <icon=nix-snowflake.xpm/> <icon=haskell-logo.xpm/>  "
  }


main :: IO ()
main = xmobar config

-- vim: ft=haskell sw=4 fdm=marker:
