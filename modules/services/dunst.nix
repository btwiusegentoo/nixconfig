# This file is generated from "README.org"
{
  services.dunst.enable = true;
  services.dunst.settings = {
      global.transparency = 10;
      global.alignment = "left";
      global.geometry = "300x5-30+20";
      urgency_low.timeout = 10;
      urgency_normal.timeout = 10;
      urgency_critical.timeout = 10;
  };
  services.dunst.settings.global.font = "SFNS Display 14";
  services.dunst.settings.global = {
      frame_color = "#959DCB";
      separator_color = "#959DCB";
  };
  services.dunst.settings.urgency_low = {
      background = "#444267";
      foreground = "#676E95";
  };
  services.dunst.settings.urgency_normal = {
      background = "#32374D";
      foreground = "#959DCB";
  };
  services.dunst.settings.urgency_critical = {
      background = "#F07178";
      foreground = "#959DCB";
  };
}
