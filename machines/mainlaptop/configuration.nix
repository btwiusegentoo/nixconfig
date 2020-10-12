
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
        bluetooth.enable = true;
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
        tlp = {
            enable = true;
            extraConfig = ''
                SOUND_POWER_SAVE_ON_AC=0
                SOUND_POWER_SAVE_ON_BAT=1
                SOUND_POWER_SAVE_CONTROLLER=Y
                BAY_POWEROFF_ON_AC=0
                BAY_POWEROFF_ON_BAT=1
                DISK_APM_LEVEL_ON_AC="254 254"
                DISK_APM_LEVEL_ON_BAT="128 128"
                DISK_IOSCHED="none none"
                SATA_LINKPWR_ON_AC="med_power_with_dipm max_performance"
                SATA_LINKPWR_ON_BAT=min_power
                MAX_LOST_WORK_SECS_ON_AC=15
                MAX_LOST_WORK_SECS_ON_BAT=60
                NMI_WATCHDOG=0
                WIFI_PWR_ON_AC=off
                WIFI_PWR_ON_BAT=on
                WOL_DISABLE=Y
                CPU_SCALING_GOVERNOR_ON_AC=powersave
                CPU_SCALING_GOVERNOR_ON_BAT=powersave
                CPU_MIN_PERF_ON_AC=0
                CPU_MAX_PERF_ON_AC=100
                CPU_MIN_PERF_ON_BAT=0
                CPU_MAX_PERF_ON_BAT=50
                CPU_BOOST_ON_AC=1
                CPU_BOOST_ON_BAT=1
                SCHED_POWERSAVE_ON_AC=0
                SCHED_POWERSAVE_ON_BAT=1
                ENERGY_PERF_POLICY_ON_AC=performance
                ENERGY_PERF_POLICY_ON_BAT=power
                RESTORE_DEVICE_STATE_ON_STARTUP=0
                RUNTIME_PM_ON_AC=on
                RUNTIME_PM_ON_BAT=auto
                PCIE_ASPM_ON_AC=default
                PCIE_ASPM_ON_BAT=powersupersave
                USB_AUTOSUSPEND=1
            '';
        };
        thinkfan = {
            enable = true;
            fan = "tp_fan /proc/acpi/ibm/fan";
            sensors = ''
                hwmon /sys/class/thermal/thermal_zone0/temp
            '';
            levels = ''
                (0, 0,  60)
                (1, 53, 65)
                (2, 55, 66)
                (3, 57, 68)
                (4, 61, 70)
                # (5, 64, 71)
                # (7, 68, 32767)
                ("level full-speed", 63, 32767)
            '';
        };
        openssh = import (../../modules/common/openssh.nix);
    };

    #systemd.services = {
        #xkb-restore = {
            #description = "Restore keyboard layout after suspend";
            #after = [ "suspend.target" ];
            #serviceConfig = {
                #User = "%I";
                #Type = "simple";
                #Environment = "DISPLAY=:0";
                #ExecStartPre = "/bin/sleep 3";
                #ExecStart = "/usr/bin/xkbcomp /etc/.jislayoutremap.xkb :0";
            #};
            #wantedBy = [ "suspend.target" ];
        #};
    #};

    # enable sound
    sound.enable = true;

    services.xserver.layout = "us";
    services.xserver.xkbVariant = "dvorak";
    services.xserver.xkbOptions = "ctrl:nocaps,altwin:swap_alt_win,swap_lalt_lwin";
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

    environment.etc = import (../../modules/common/etcfiles.nix);

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
