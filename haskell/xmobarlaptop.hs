import           Xmobar


config :: Config
config = defaultConfig
  { font = "xft:Spleen-9,GohuFont Nerd Font-10.5,DejaVu Sans-10.5"
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
    , Run $ BatteryN
      ["BAT0"]
      [ "-t"
      , "<acstatus><left>"
      , "-S"
      , "Off"
      , "-d"
      , "0"
      , "-m"
      , "3"
      , "-L"
      , "10"
      , "-H"
      , "90"
      , "-p"
      , "3"
      , "--low"
      , "#f07178"
      , "--normal"
      , "#676E95"
      , "--high"
      , "#80cbc4"
      , "--"
      , "-P"
      , "-a"
      , "notify-send -u critical 'Battery running out!!!!!!'"
      , "-A"
      , "7"
           -- Charged
      , "-i"
      , "<icon=battery-charging.xpm/>"
           -- AC on
      , "-O"
      , "<icon=battery-charging.xpm/> <timeleft> <watts>"
           -- Discharging
      , "-o"
      , "<icon=battery.xpm/> <timeleft> <watts>"
      , "-H"
      , "10"
      , "-L"
      , "7"
      , "-h"
      , "#80cbc4"
      , "-l"
      , "#f07178"
      ]
      50
      "battery"
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
                    \{ %battery% <fc=#f07178>%cpu%</fc>  <fc=#80cbc4>%memory% %swap%</fc>  <fc=#f78c6c><icon=calendar.xpm/> %date%</fc>  <fc=#82aaff>%default:Master%</fc>  <fc=#f07178>\57345 %pomodoro%</fc> \
                    \ <fc=#ffcb6b><icon=NewTux.xpm/> %uname% </fc> "
  }


main :: IO ()
main = xmobar config

-- vim: ft=haskell sw=4 fdm=marker:
