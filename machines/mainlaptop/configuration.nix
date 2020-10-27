
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
            # import cachix
            ./cachix.nix
            # import home-manager module
            (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/96d7de6db18d9a5bf254ddf3525bb4ef1d2a6bda.tar.gz}/nixos")
            # import unstable doas
            (import "${builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz}/nixos/modules/security/doas.nix")
            # import user settings
            ../../usersettings.nix
            # import user defaults
            ../../modules/common/userdefaults.nix
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
        loader.grub = {
            enable = true;
            version = 2;
            extraConfig = ''
                if keystatus --shift ; then
                    set timeout=-1
                else
                    set timeout=0
                fi
            '';
            enableCryptodisk = true;
            copyKernels = true;
        };
        loader.timeout = 0;
        # Enable latest linux kernel
        kernelPackages = unstable.linuxPackages_latest;
        kernelModules = [ "i915" "acpi_call" "tpm-rng" ];
        extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
        extraModprobeConfig = ''
            options bluetooth disable_ertm=Y
            options iwlwifi 11n_disable=8 bt_coex_active=N
            options thinkpad_acpi experimental=1 fan_control=1
        '';
        kernelParams = [
            "acpi_osi='!Windows 2012'"
            "acpi_backlight=vendor"
        ];
        plymouth.enable = true;
    };
    # Supposedly better for the SSD.
    fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
    # Boot faster
    systemd.services.systemd-udev-settle.enable = false;
    systemd.services.NetworkManager-wait-online.enable = false;
    systemd.extraConfig = ''
        DefaultTimeoutStopSec=10s
    '';
    # }}}

    # Hardware{{{
    hardware = {
        pulseaudio = (import ../../modules/services/pulseaudio.nix) { inherit pkgs; };
        bluetooth = (import ../../modules/common/bluetooth.nix) { inherit pkgs; };
        opengl.enable = true;
        opengl.driSupport = true;
        opengl.extraPackages = with pkgs; [
            vaapiIntel
            vaapiVdpau
            libvdpau-va-gl
            intel-media-driver
        ];
        cpu.intel.updateMicrocode = true;
        # trackpoint.enable = true;
        # trackpoint.emulateWheel= true;
        # trackpoint.device = "TPPS/2 IBM TrackPoint";
    };
    # }}}

    zramSwap = (import ../../modules/services/zram.nix);

    programs = {
        dconf.enable = true;
        adb.enable = true;
    };

    # Networking{{{
    networking.hostName = "nixos"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking = {
        dhcpcd.enable = false;
        useDHCP = false;
        interfaces.wlan0.useDHCP = false;
        networkmanager = {
            enable = true;
            wifi = {
                backend = "iwd";
                macAddress = "random";
                scanRandMacAddress = true;
            };
        };
    };
    #}}}

    fonts = (import ../../modules/common/fonts.nix) { inherit pkgs; };

    environment.variables = (import ../../modules/common/globalvars.nix);

    services = {
        fstrim.enable = true;                                   # Trim ssd
        blueman.enable = true;                                  # Used for bluetooth
        tlp = import (../../modules/services/tlp.nix);
        thinkfan = import (../../modules/services/thinkfan.nix);
        openssh = import (../../modules/common/openssh.nix);
    };

    #systemd.user.services = {
        #xkb-restore = {
            #description = "Restore keyboard layout after suspend";
            #after = [ "suspend.target" "graphical-session.target" ];
            #serviceConfig = {
                #Type = "simple";
                #Environment = "DISPLAY=:0";
                #ExecStartPre = "/usr/bin/env sleep 3";
                #ExecStart = "/usr/bin/xkbcomp /etc/.jislayoutremap.xkb :0";
            #};
            #wantedBy = [ "suspend.target" "graphical-session.target" ];
        #};
    #};
    systemd.user.services = {
        xkb-restore = {
            description = "Restore keyboard layout after suspend";
            after = [ "suspend.target" "graphical-session.target" ];
            serviceConfig = {
                Type = "simple";
                Environment = "DISPLAY=:0";
                ExecStartPre = "/usr/bin/env sleep 3";
                ExecStart = "${pkgs.bash}/bin/bash -c \"${pkgs.xorg.xkbcomp}/bin/xkbcomp -i $(${pkgs.xorg.xinput}/bin/xinput list | sed -n 's/.*Translated.*id=\\\([0-9]*\\\).*keyboard.*/\\\1/p') /etc/x230key.xkb :0\"";
            };
            wantedBy = [ "suspend.target" "graphical-session.target" ];
        };
    };
   
    # enable sound
    sound.enable = true;

    services.xserver.layout = "us";
    services.xserver.xkbVariant = "dvorak";
    # services.xserver.xkbOptions = "ctrl:nocaps,altwin:swap_alt_win,swap_lalt_lwin";
    services.xserver.videoDrivers = [ "intel" ];
    services.xserver.deviceSection = ''
        Option "TearFree" "true"
    '';
    services.xserver.inputClassSections = [
        ''
        Identifier "touchpad"
        MatchProduct "SynPS/2 Synaptics TouchPad"
        # MatchTag "lenovo_x230_all"
        Driver "synaptics"
        # fix touchpad resolution
        Option "VertResolution" "100"
        Option "HorizResolution" "65"
        # disable synaptics driver pointer acceleration
        Option "MinSpeed" "1"
        Option "MaxSpeed" "1"
        # tweak the X-server pointer acceleration
        Option "AccelerationProfile" "2"
        Option "AdaptiveDeceleration" "16"
        Option "ConstantDeceleration" "16"
        Option "VelocityScale" "20"
        Option "AccelerationNumerator" "30"
        Option "AccelerationDenominator" "10"
        Option "AccelerationThreshold" "10"
        # Disable two fingers right mouse click
        Option "TapButton2" "0"
        Option "HorizHysteresis" "100"
        Option "VertHysteresis" "100"
        # fix touchpad scroll speed
        Option "VertScrollDelta" "500"
        Option "HorizScrollDelta" "500"
        ''
    ];


    home-manager = {
        useUserPackages=true;
        verbose = true;
        users.${username} = import ./home.nix;
    };

    environment.etc = import ../../modules/common/etcfiles.nix { inherit pkgs; };

    nixpkgs.config = import ../../configs/nixpkgs-config.nix;

    nixpkgs.overlays = import ../../overlays/all-overlays.nix { inherit pkgs; };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?

}

# vim:ft=nix sw=4 fdm=marker:
