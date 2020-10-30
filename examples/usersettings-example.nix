{ pkgs, ... }:
# This file goes inside machines/(machinename)
# contains machine specific configs and maybe secrets
{
    services.openssh.ports = [ 22 ];        # Change to better port if you want
    time.timeZone = "Europe/Amsterdam";      # set your local timezone. See timedatectl list-timezones
    services.xserver.displayManager.lightdm.greeters.mini.user = "myusername";
    services.xserver.displayManager.autoLogin.enable = true;
    services.xserver.displayManager.autoLogin.user = "myusername";

    users.users.myusername = {
        isNormalUser = true;
        extraGroups = [ "wheel" "input" "libvirtd" ];
        shell = pkgs.fish;
    };
    # use doas instead of sudo
    security.sudo.enable = false;
    security.doas = {
        enable = true;
        wheelNeedsPassword = true;
        extraRules = [
            { groups = [ "wheel" ]; noPass = false; keepEnv = true; persist = true;}
        ];
    };
    nix.allowedUsers = [ "@wheel" ];
}
