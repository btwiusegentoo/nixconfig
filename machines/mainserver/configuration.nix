
{ config, pkgs, fetchgit, ... }:

let
    unstable = pkgs.unstable;


    # import variables
    username = (import ../../uservars.nix).username;

in
{

    imports =
        [ # Include the results of the hardware scan.
            ./hardware-configuration.nix
            # import home-manager module
            (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos")
            # import unstable doas
            (import "${builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz}/nixos/modules/security/doas.nix")
            # import user settings
            ../../usersettings.nix
            # import user defaults
            ../../modules/common/userdefaults.nix
            # import system packages
            ../../modules/common/systempackages.nix
            # import locale configs
            ../../modules/common/globallocale.nix
        ];

    # Boot{{{
    boot = {
        loader.grub.enable = true;
        loader.grub.version = 2;
        # Enable latest linux kernel
        kernelPackages = unstable.linuxPackages_latest;
    };
    # Boot faster
    systemd.services.systemd-udev-settle.serviceConfig.TimeoutSec = 5;
    systemd.services.NetworkManager-wait-online.enable = false;
    # }}}

    # Hardware{{{
    hardware = {
        pulseaudio = (import ../../modules/services/pulseaudio.nix) { inherit pkgs; };
        bluetooth.enable = true;
        opengl.enable = true;
        opengl.driSupport = true;
        cpu.intel.updateMicrocode = true;
    };
    # }}}

    zramSwap = (import ../../modules/services/zram.nix);

    # Networking{{{
    networking.hostName = "nixos"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking = {
        dhcpcd.enable = false;
        useNetworkd = true;
        useDHCP = false;
        interfaces.wlp0s4f1u3u4.useDHCP = true;
        networkmanager = {
            enable = true;
            dns = "systemd-resolved";
        };
    };
    #}}}

    environment.variables = (import ../../modules/common/globalvars.nix);

    services = {
        blueman.enable = true;                                  # Used for bluetooth
        openssh = import (../../modules/common/openssh.nix);
    };

    # enable sound
    sound.enable = true;

    home-manager = {
        useUserPackages=true;
        verbose = true;
        users.${username} = import ./home.nix;
    };

    environment.etc = import (../../modules/common/etcfiles.nix);

    nixpkgs.config = import ../../configs/nixpkgs-config.nix;

    nixpkgs.overlays = import ../../overlays/all-overlays.nix { inherit pkgs; };

    nix.buildMachines = [ {
        hostName = "maindesktopbuild";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 2;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
    } ];
    nix.distributedBuilds = true;
	# optional, useful when the builder has a faster internet connection than yours
    nix.extraOptions = ''
        builders-use-substitutes = true
    '';

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?

}

# vim:ft=nix sw=4 fdm=marker:
