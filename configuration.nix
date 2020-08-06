
{ config, pkgs, fetchgit, ... }:

let
    unstable = pkgs.unstable;

    # Python packages{{{
    my-python-packages = python-packages: with python-packages; [
        jedi
        pynvim
        numpy
        pip
        pylint
        pymodoro
    ];
    python-with-my-packages = unstable.python3.withPackages my-python-packages;
    # }}}

    # Custom python packages that I want globally{{{

    # pymodoro{{{
    pymodoro = unstable.python3Packages.buildPythonPackage rec {
        pname = "pymodoro";
        version = "1.14";
        src = pkgs.fetchgit {
            url = "https://github.com/dattanchu/pymodoro";
            rev = "f666ef9c62261e91351cc734ada23b5a2f029220";
            sha256 = "1gkq989cfq19cm4nxch6lf63sz9rmkkibaw7r0n2pcwdamjssgj1";
        };
    };
    # }}}

    # }}}

in
{

    # imports{{{
    imports =
        [ # Include the results of the hardware scan.
            ./hardware-configuration.nix
          # Include secrets
            ./secrets/sshconfig.nix
          # import home-manager module
            (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.03.tar.gz}/nixos")
          # import unstable doas
            (import "${builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz}/nixos/modules/security/doas.nix")
        ];
    # }}}

    # systemPackages{{{
    # packages that will be installed system-wide. to search, run
    # $ nix search wget
    environment.systemPackages = with pkgs; [
        wget
        git
        bat
        unstable.neovim
        unstable.nodejs
        unstable.yarn
        unstable.openssh
        unzip
        nix-prefetch-git
        nix-prefetch-github
        unstable.gnumake
        unstable.gcc
        python-with-my-packages
        cachix
        unstable.ipad_charge
        mosh-master
        unstable.fish
        pypi2nix
        home-manager
        # deps
        libffi
        xorg.libxcb
    ];
# }}}

    #Boot{{{
    # Use the systemd-boot EFI boot loader.
    boot = {
        loader.systemd-boot.enable = true;
        loader.efi.canTouchEfiVariables = true;
        # Enable latest linux kernel
        kernelPackages = unstable.linuxPackages_latest;
    };
    # Supposedly better for the SSD.
    fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
    # Boot faster
    systemd.services.systemd-udev-settle.enable = false;
    systemd.services.NetworkManager-wait-online.enable = false;
    # }}}

    # Hardware{{{
    hardware = {
        pulseaudio = {
            enable = true;
            extraModules = [ pkgs.pulseaudio-modules-bt ];
            package = pkgs.pulseaudioFull;
        };
        bluetooth.enable = true;
        opengl.enable = true;
        opengl.driSupport = true;
        cpu.amd.updateMicrocode = true;
    };
# }}}

    zramSwap = {
        enable = true;
        algorithm = "zstd";
        memoryPercent = 60;
    };

    #Networking{{{
    networking.hostName = "nixos"; # Define your hostname.
    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    networking.useDHCP = false;
    networking.interfaces.enp9s0.useDHCP = true;
    networking.networkmanager.enable = true;

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";}}}

    #fonts{{{
    fonts = {
        fontconfig.enable = true;
        enableFontDir = true;
        enableGhostscriptFonts = true;
        fonts = with pkgs; [
            (nerdfonts.override { fonts = ["Gohu"]; })
            unstable.tamzen
            #(tamzen-nerdfont.override { size = "10x20";})
            san-francisco-font 
            apple-color-emoji
            noto-fonts-cjk
        ];
    };
    #}}}

    # Select internationalisation properties.{{{
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "dvorak";
    };

    i18n.inputMethod.enabled = "fcitx";
    i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ mozc ];

    # Set your time zone.
    time.timeZone = "Asia/Tokyo";# }}}

    # set global environment variables {{{
    environment.variables = {
        EDITOR = "nvim";
        XMODIFIERS = "@im=fcitx";
        XMODIFIER = "@im=fcitx";
    };

    # }}}

    # services{{{

    services = {
        fstrim.enable = true;
        blueman.enable = true;
        gnome3.gnome-keyring.enable = true;
    };

    # Enable sound.
    sound.enable = true;
    #}}}

    #xserver{{{
    services.xserver = {
        enable = true;
        autorun = true;
        layout = "us";
        xkbVariant = "dvorak";
        dpi = 96;
        videoDrivers = [ "amdgpu" ];
        desktopManager.session = [
            {
                name = "home-manager";
                start = ''
                    ${pkgs.runtimeShell} $HOME/.hm-xsession &
                    waitPID=$!
                '';
            }
        ];
        #desktopManager.xterm.enable = false;
        #displayManager.defaultSession = "none+xmonad";
        displayManager.lightdm = {
            enable = true;
            autoLogin.enable = true;
            autoLogin.user = "btw";
            greeters.mini = {
                enable = true;
                user = "btw";
                extraConfig = ''
                    [greeter]
                    show-password-label = false
                    [greeter-theme]
                    background-image = "/etc/wallpapers/default.png"
                '';
            };
        };
        #windowManager.i3.enable = true;
        #windowManager.i3.package = pkgs.i3-gaps;
        #windowManager.xmonad = {
            #enable = true;
            #enableContribAndExtras = true;
            #haskellPackages = 
                #unstable.haskell.packages.ghc882;
            #extraPackages = haskellPackages: [
                #haskellPackages.xmonad-contrib
                #haskellPackages.xmonad-extras
                #haskellPackages.xmonad
            #];
        #};
        wacom.enable = true;
        autoRepeatDelay = 200;
        autoRepeatInterval = 25;
    };# }}}

    location.provider = "geoclue2";

    # specify home-manager file
    home-manager = {
        useUserPackages=true;
        verbose = true;
        users.btw = import ./home/home.nix;
    };


    # Define a user account. Don't forget to set a password with ‘passwd’.{{{
    users.users.btw = {
        isNormalUser = true;
        extraGroups = [ "wheel" "input" ];
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
    # }}}

    #etcfiles{{{

    # change wacom tablet orientation.{{{
    environment.etc."X11/xorg.conf.d/50-wacomtweak.conf".text = ''
    Section "InputClass" 
        Identifier "Wacom"
        MatchProduct "Wacom Bamboo 16FG 4x5 Finger"
        Driver "wacom"
        Option "Rotate" "Half"
        Option "AccelerationProfile" "-1"
        Option "AccelerationThreshold" "0.1"
    EndSection

    Section "InputClass" 
        Identifier "Wacom"
        MatchProduct "Wacom Bamboo 16FG 4x5 Pen"
        Driver "wacom"
        Option "Rotate" "Half"
    EndSection

    Section "InputClass" 
        Identifier "Wacom"
        MatchProduct "Wacom Bamboo 16FG 4x5 Pad"
        Driver "wacom"
        Option "Rotate" "Half"
    EndSection
    '';
    # }}}

    environment.etc."wallpapers/wallpaper1.png".source = ./wallpaper1.png;
    environment.etc."wallpapers/wallpaper2.png".source = ./wallpaper2.png;
    # I will not delete just to make a example.
    # use pkgs.writeScript to make executable.
    #environment.etc."lightdm/xrandr.sh".source = pkgs.writeScript "xrandr.sh" ''
        ##!/bin/sh
        #xrandr --output DVI-D-0 --scale 1.33333333333333x1.33333333333333 --panning 2560x1440
    #'';

    # }}}

    nixpkgs.config = import ./nixpkgs/config.nix;

    nixpkgs.overlays = import ./overlays/all-overlays.nix { inherit pkgs; };

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?

}

# vim:ft=nix sw=4 fdm=marker:
