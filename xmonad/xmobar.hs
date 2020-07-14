import           Xmobar

config :: Config
config = defaultConfig {
        font = "xft:Monoid Nerd Font:pixelsize=14:antialias=true:hinting=false"
       , additionalFonts = []
       , borderColor = "black"
       , border = TopB
       , alpha = 255
       , bgColor = "#34324a"
       , fgColor = "#A6ACCD"
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
       , commands = [ Run $ Cpu ["-L","3","-H","50",
                               "--normal","green","--high","red"] 10
                    , Run $ Memory ["-t","Mem: <usedratio>%"] 10
                    , Run $ Swap [] 10
                    , Run $ Com "uname" [ "-s" , "-r" ] "" 3600
                    , Run $ Date "%a %b %_d %Y %H:%M:%S" "date" 10
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% | %cpu% | %memory% * %swap% }\
                    \{ <fc=#f78c6c>%date%</fc> | %uname%"
       }

main :: IO()
main = xmobar config
