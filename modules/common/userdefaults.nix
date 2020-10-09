{ pkgs, config, ... }:
let
    username = (import ../../uservars.nix).username;
in
{
    users.users.${username} = {
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
