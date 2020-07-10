# nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable

{ config, pkgs, ... }:

let
    unstable = import <nixpkgs-unstable> {};
    my-python-packages = python-packages: with python-packages; [
        jedi
        pynvim
        numpy
        pip
    ];
    python-with-my-packages = pkgs.python3.withPackages my-python-packages;

in
{
    imports =
        [ # Include the results of the hardware scan.
        ./hardware-configuration.nix
        ];


    # List packages installed in system profile. To search, run:
    # $ nix search wget
    environment.systemPackages = with pkgs; [
        wget
        sudo
        git
        unstable.neovim
        nodejs
        yarn
        unzip
        home-manager
        fish
        nix-prefetch-git
        gnumake
        gcc
        python-with-my-packages
        cachix
    ];


    #Boot{{{
    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    # Enable latest linux kernel
    boot.kernelPackages = pkgs.linuxPackages_latest;
    # Supposedly better for the SSD.
    fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
    # Boot faster
    systemd.services.systemd-udev-settle.enable = false;
    systemd.services.NetworkManager-wait-online.enable = false;# }}}

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
            corefonts
            nerdfonts
            san-francisco-font 
            apple-color-emoji
            noto-fonts-cjk
        ];
    };
    /*}}}*/

    # Select internationalisation properties.{{{
    # i18n.defaultLocale = "en_US.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "dvorak";
    };

    # Set your time zone.
    time.timeZone = "Asia/Tokyo";# }}}

    # Use neovim as default editor{{{
    environment.variables.EDITOR = "nvim";

    # }}}

    # services{{{

    # Enable the OpenSSH daemon.
    services.openssh.enable = true;

    # Enable sound.
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    services.xserver = {
        enable = true;
        autorun = true;
        layout = "us";
        xkbVariant = "dvorak";
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
    };# }}}

    # Define a user account. Don't forget to set a password with ‘passwd’.{{{
    users.users.btw = {
        isNormalUser = true;
        extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
        shell = pkgs.fish;
    };
    security.sudo.enable = true;# }}}

    #etcfiles{{{
    environment.etc."X11/xorg.conf.d/50-wacomtweak.conf".text = ''
    Section "InputClass" 
        Identifier "Wacom"
        MatchProduct "Wacom Bamboo 16FG 4x5 Finger"
        Driver "wacom"
        Option "Rotate" "Half"
    EndSection
    '';
# }}}

    nixpkgs.config = {
        allowUnfree = true;
    };

    nixpkgs.overlays = [# {{{

        (self: super: {
        neovim = super.neovim.override {
        viAlias = true;
        vimAlias = true;
        };
        })

        (import ./overlays/packages.nix)
    ];# }}}

    # This value determines the NixOS release from which the default
    # settings for stateful data, like file locations and database versions
    # on your system were taken. It‘s perfectly fine and recommended to leave
    # this value at the release version of the first install of this system.
    # Before changing this value read the documentation for this option
    # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
    system.stateVersion = "20.03"; # Did you read the comment?

}

# vim:shiftwidth=4 ft=nix foldmethod=marker:
