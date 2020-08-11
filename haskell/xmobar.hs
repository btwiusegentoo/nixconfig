import           Xmobar


config :: Config
config = defaultConfig {
        font = "xft:TamzenForPowerline-14:style=regular,GohuFont Nerd Font:size=12"
       , additionalFonts = []
       , borderColor = "#232635"
       , border = BottomB
       , alpha = 255
       , bgColor = "#232635"
       --, alpha = 240
       --, bgColor = "#233526" -- Somehow color get messed up when using alpha issue#246 this is the workaround. invert 3rd4th <--> 5th6th like this.
       , fgColor = "#A6ACCD"
       , position = TopSize L 100 30
       , textOffset = -1
       , iconOffset = -1
       , lowerOnStart = True
       , pickBroadest = False
       , persistent = False
       , hideOnStart = False
       , iconRoot = "."
       , allDesktops = True
       , overrideRedirect = True
       , commands = [ Run $ Cpu ["-t","\57958 <total>%"] 10
                    , Run $ Memory ["-t","\63578 <used>MB"] 10
                    , Run $ Swap ["-t", "易 \63433 <used>MB"] 10
                    , Run $ Date "%a %m/%_d %H:%M:%S" "date" 10
                    , Run $ Com "uname" [ "-r" ] "" 36000
                    , Run $ CommandReader "pymodoro" "pomodoro"
                    , Run $ Volume "default" "Master" ["-t", "<status> <volume>%", "--",
                                                       "-O", "\61480", -- on
                                                       "-o", "\61478", -- off
                                                       "-C", "#FFFFFF",
                                                       "-c", "#f07178"] 3
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }\
                    \{  <fc=#f07178>%cpu%</fc> | <fc=#80cbc4>%memory% %swap%</fc> | <fc=#f78c6c>\61555 %date%</fc> | <fc=#9cc4ff>%default:Master%</fc> |<fc=#f07178> \57345 %pomodoro%</fc> \
                    \| <fc=#ffcb6b>\61820 %uname% </fc><fc=#82aaff>\62227 </fc> <fc=#c792ea>\58911 </fc> "
       }


main :: IO()
main = xmobar config

-- vim: ft=haskell sw=4 fdm=marker:
