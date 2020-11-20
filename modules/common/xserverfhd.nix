{ pkgs, config, ... }:
{
  services.xserver = {
    enable = true;
    autorun = true;
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
            border-width = 6px
            password-color = "#A6ACCD"
            password-background-color = "#202331"
            password-border-color = "#202331"
            background-image = "/etc/wallpapers/wallpaper2-1080.png"
          '';
        };
      };
    };
    autoRepeatDelay = 200;
    autoRepeatInterval = 25;
  };
}
