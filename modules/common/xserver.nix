{ pkgs, config, ... }:
let
    username = (import ../../uservars.nix).username;

in
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
        displayManager.lightdm = {
            enable = true;
            autoLogin.enable = true;
            autoLogin.user = "${username}";
            greeters.mini = {
                enable = true;
                user = "${username}";
                extraConfig = ''
                    [greeter]
                    show-password-label = false
                    [greeter-theme]
                    background-image = "/etc/wallpapers/wallpaper2.png"
                '';
            };
        };
        autoRepeatDelay = 200;
        autoRepeatInterval = 25;
    };
}
