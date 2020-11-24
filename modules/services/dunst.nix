# This file is generated from "README.org"
{
  services.dunst = {
    enable = true;
    settings = {
        global.transparency = 10;
        global.alignment = "left";
        global.geometry = "300x5-30+20";
        global.font = "SFNS Display 14";
        global = {
            frame_color = "#959DCB";
            separator_color = "#959DCB";
        };
        urgency_low = {
            background = "#444267";
            foreground = "#676E95";
        };
        urgency_normal = {
            background = "#32374D";
            foreground = "#959DCB";
        };
        urgency_critical = {
            background = "#F07178";
            foreground = "#959DCB";
        };
        urgency_low.timeout = 10;
        urgency_normal.timeout = 10;
        urgency_critical.timeout = 10;
    };
  };
}
