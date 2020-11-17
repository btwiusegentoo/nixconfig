{ config, pkgs, fetchgit, ... }:
{

    imports =
        [ # Include the results of the hardware scan.
            ./hardware-configuration.nix
            # import user settings
            ./usersettings.nix
            # import xserver configs
            ../../modules/common/xserver.nix
            # import system packages
            ../../modules/common/systempackages.nix
            # import locale configs
            ../../modules/common/globallocale.nix
        ];

    # Boot{{{
    # Use the systemd-boot EFI boot loader.
    boot = {
        loader.systemd-boot.enable = true;
        loader.efi.canTouchEfiVariables = true;
        # Enable latest linux kernel
        kernelPackages = pkgs.unstable.linuxPackages_latest;
        extraModulePackages = with pkgs.unstable.linuxPackages_latest; [ xpadneo ];
        extraModprobeConfig = ''
        options bluetooth disable_ertm=Y
        '';
    };
    # Supposedly better for the SSD.
    fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
    # Boot faster
    systemd.services.systemd-udev-settle.serviceConfig.TimeoutSec = 5;
    systemd.services.NetworkManager-wait-online.enable = false;
    # }}}

    # Hardware{{{
    hardware = {
        pulseaudio = (import ../../modules/services/pulseaudio.nix) { inherit pkgs; };
        bluetooth = (import ../../modules/common/bluetooth.nix) { inherit pkgs; };
        opengl.enable = true;
        opengl.driSupport = true;
        opengl.driSupport32Bit = true;
        opengl.extraPackages32 = with pkgs.unstable.pkgsi686Linux; [ libva ];
        cpu.amd.updateMicrocode = true;
    };
    # }}}

    zramSwap = (import ../../modules/services/zram.nix);

    programs = {
        dconf.enable = true;
        adb.enable = true;
        java.enable = true;
        java.package = pkgs.unstable.jdk;
    };

    # Networking{{{
    networking.hostName = "desktop1"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking = {
        dhcpcd.enable = false;
        useNetworkd = true;
        useDHCP = false;
        interfaces.enp9s0.useDHCP = true;
        networkmanager = {
            enable = true;
            dns = "systemd-resolved";
        };
    };
    #}}}

    fonts = (import ../../modules/common/fonts.nix) { inherit pkgs; };

    environment.variables = (import ../../modules/common/globalvars.nix);

    services = {
        fstrim.enable = true;                                   # Trim ssd
        blueman.enable = true;                                  # Used for bluetooth
        earlyoom.enable = true;
        openssh = import (../../modules/common/openssh.nix);
    };

    virtualisation = import (../../modules/virtualisation/default.nix);

    # enable sound
    sound.enable = true;

    services.xserver.layout = "us";
    services.xserver.xkbVariant = "dvorak";
    services.xserver.videoDrivers = [ "amdgpu" ];
    services.xserver.wacom.enable = true;

    environment.etc = import ../../modules/common/etcfiles.nix;

    nixpkgs.config = import ../../configs/nixpkgs-config.nix;

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?

}

# vim:ft=nix sw=4 fdm=marker:
