import           Xmobar


config :: Config
config = defaultConfig {
        font = "xft:Monoid Nerd Font:pixelsize=14:antialias=true:hinting=false"
       , additionalFonts = []
       , borderColor = "#2b2a3e"
       , border = BottomB
       , alpha = 255
       , bgColor = "#202331"
       , fgColor = "#FFFFFF"
       , position = TopSize L 100 20
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
                    , Run $ Memory ["-t","\63578 <usedratio>%"] 10
                    , Run $ Swap ["-t", "易 \63433 <usedratio>%"] 10
                    , Run $ Date "%a %b %_d %H:%M:%S" "date" 10
                    , Run $ Com "uname" [ "-s" , "-r" ] "" 36000
                    , Run $ Com "qdbus" [ "org.fcitx.Fcitx" , "/inputmethod" , "GetCurrentIM" ] "kbd" 10
                    , Run $ CommandReader "pymodoro" "pomodoro"
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader% }\
                    \{ <fc=#f07178>\57345 %pomodoro%</fc> <fc=#676e95>\63506 %kbd%</fc> \63192 <fc=#f07178>%cpu%</fc> \63192 <fc=#80cbc4>%memory% %swap%</fc> \63192 <fc=#f78c6c>\61555 %date%</fc>\
                    \ \63192 <fc=#ffcb6b>\61820 %uname% </fc><fc=#82aaff>\62227 </fc> <fc=#c792ea>\58911 </fc> "
       }


main :: IO()
main = do
    xmobar config
