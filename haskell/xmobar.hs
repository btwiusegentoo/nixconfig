-- This file is generated from "README.org"
import           Xmobar


config :: Config
config = defaultConfig
  { font             = "xft:Liberation Sans-14,GohuFont Nerd Font-14"
  , additionalFonts  = []
  , borderColor      = "#232635"
  , border           = BottomB
  -- , alpha            = 255
  -- , bgColor          = "#3C435E"
  , alpha            = 220
  , bgColor          = "#203123" -- Somehow color get messed up when using alpha issue#246 this is the workaround. invert 3rd4th <--> 5th6th like this.
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
  , commands         = [ Run $ Cpu ["-t", "<icon=cpu.xpm/>  <total>%"] 10
                       , Run $ Memory ["-t", "<icon=ram.xpm/>  <used>MB"] 10
                       , Run $ Date "%a %m/%_d" "date" 10
                       , Run $ Date "%H:%M:%S" "time" 10
                       , Run $ Volume
                         "default"
                         "Master"
                         [ "-t"
                         , "<status>  <volume>%"
                         , "--"
                         , "-O"
                         , "<icon=volume.xpm/>"
                         , -- on
                           "-o"
                         , "<icon=volume-mute.xpm/>"
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
    "%StdinReader% }\
     \ <fc=#89ddff><icon=clock.xpm/>  %time%</fc> \
     \{ <fc=#f07178>%cpu%</fc> | <fc=#80cbc4>%memory%</fc>\
     \ | <fc=#f78c6c><icon=calendar.xpm/>  %date%</fc> | <fc=#82aaff>%default:Master%</fc>"
  }


main :: IO ()
main = xmobar config
