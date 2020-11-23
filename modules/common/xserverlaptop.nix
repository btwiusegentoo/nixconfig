# This file is generated from "README.org"
{ pkgs, config, ... }:
{
  services.xserver = {
    enable = true;
    autorun = true;
    layout = "us";
    xkbVariant = "dvorak";
        autoRepeatDelay = 200;
        autoRepeatInterval = 25;
    dpi = 96;
    desktopManager.session = [
      {
        name = "home-manager";
        start = ''
          ${pkgs.runtimeShell} $HOME/.hm-xsession &
          waitPID=$!
        '';
      }
    ];
    displayManager = {
        lightdm = {
        enable = true;
        greeters.mini = {
            enable = true;
            extraConfig = ''
            [greeter]
            show-password-label = false
            [greeter-theme]
            text-color = "#A6ACCD"
            error-color = "#F07178"
            window-color = "#202331"
            border-color = "#202331"
            password-color = "#A6ACCD"
            password-background-color = "#202331"
            password-border-color = "#202331"
            border-width = 6px
            background-image = "/etc/wallpapers/wallpaper2-1080.png"
            '';
        };
        };
    };
  };
}
