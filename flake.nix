# This file is generated from "README.org"
{
  description = "NixOS configuration for all machines";

  inputs = {
    home-manager = {
      url = "github:rycee/home-manager/release-20.09";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
    nur.url = "github:nix-community/NUR";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  
    nixpkgs.url = "github:nixos/nixpkgs/nixos-20.09";
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    master.url = "github:nixos/nixpkgs/master";
  };

  outputs = inputs@{ self, home-manager, nur, nixpkgs, ... }:
    let
      inherit (builtins) listToAttrs attrValues attrNames readDir;
      inherit (nixpkgs) lib;
      inherit (lib) removeSuffix;

      pkgs = (import nixpkgs) {
        system = "x86_64-linux";
        config = {
          allowUnfree = true;
          retroarch = {
            enableDolphin = true;
            enableCitra = true;
            enableParallelN64 = true;
            enableDesmume = true;
            enableSnes9x = true;
            enableNestopia = true;
            enableVbaNext = true;
          };
        };
        overlays = attrValues self.overlays;
      };

      defaults = { pkgs, ... }: {
        imports = [
          ./cachix.nix
          ./modules/common/nix.nix
          ./modules/common/doas.nix
          ./modules/common/console.nix
          ./modules/hardware/bluetooth.nix
          ./modules/hardware/zram.nix
          ./modules/hardware/earlyoom.nix
          ./modules/common/etcfiles.nix
          ./modules/common/systempackages.nix
          ./modules/common/globallocale.nix
          ./modules/services/pulseaudio.nix
          ./modules/services/clamav.nix
          ./modules/services/openssh.nix
        ];
      };
    in
    {
      overlays =
        let
          overlayFiles = listToAttrs (map
            (name: {
              name = removeSuffix ".nix" name;
              value = import (./overlays + "/${name}");
            })
            (attrNames (readDir ./overlays)));
        in
          overlayFiles // {
            nur = final: prev: {
              nur = import inputs.nur { nurpkgs = final.unstable; pkgs = final.unstable; };
            };
            unstable = final: prev: {
              unstable = import inputs.unstable {
                system = final.system;
                config = {
                  allowUnfree = true;
                  retroarch = {
                    enableDolphin = true;
                    enableCitra = true;
                    enableParallelN64 = true;
                    enableDesmume = true;
                    enableSnes9x = true;
                    enableNestopia = true;
                    enableVbaNext = true;
                  };
                };
                overlays = [
                  inputs.emacs-overlay.overlay;
                ];
              };
            };
            master = final: prev: {
              master = import inputs.master {
                system = final.system;
                config = {
                  allowUnfree = true;
                  retroarch = {
                    enableDolphin = true;
                    enableCitra = true;
                    enableParallelN64 = true;
                    enableDesmume = true;
                    enableSnes9x = true;
                    enableNestopia = true;
                    enableVbaNext = true;
                  };
                };
              };
            };
          };

      nixosConfigurations = {
        desktop1 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            [
              defaults
              ./machines/maindesktop/configuration.nix
              ./modules/common/xserver.nix
              ./modules/hardware/ssd.nix
              ./modules/common/fonts.nix
              ./modules/gui/blueman.nix
              home-manager.nixosModules.home-manager
                ({
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users.btw = { ... }: {
                    imports = [
                      ./machines/maindesktop/home.nix
                      ./modules/common/xdg.nix
                      ./modules/common/xmonad.nix
                      ./modules/services/dunst.nix
                      ./modules/services/picom.nix
                      ./modules/terminal/alacritty.nix
                      ./modules/gui/qutebrowser.nix
                      ./modules/gui/firefox.nix
                      ./modules/gui/mpv.nix
                      ./modules/gui/zathura.nix
                      ./modules/editors/emacs.nix
                      ./modules/terminal/fish.nix
                      ./modules/terminal/git.nix
                      ./modules/terminal/bat.nix
                      ./modules/terminal/fzf.nix
                      ./modules/terminal/lsd.nix
                      ./modules/terminal/starship.nix
                      ./modules/terminal/tmux.nix
                      ./modules/services/gpg.nix
                    ];
                  };
                })
            ];
          inherit pkgs;
        };
        laptop1 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            [
              defaults
              ./machines/mainlaptop/configuration.nix
              ./modules/common/xserverlaptop.nix
              ./modules/hardware/ssd.nix
              ./modules/hardware/tlp.nix
              ./modules/hardware/thinkfan.nix
              ./modules/hardware/libinput.nix
              ./modules/common/fonts.nix
              ./modules/gui/blueman.nix
              home-manager.nixosModules.home-manager
                ({
                  home-manager.useGlobalPkgs = true;
                  home-manager.useUserPackages = true;
                  home-manager.users.x230 = { ... }: {
                    imports = [
                      ./machines/mainlaptop/home.nix
                      ./modules/common/xdg.nix
                      ./modules/common/xmonad.nix
                      ./modules/services/dunst.nix
                      ./modules/services/picom.nix
                      ./modules/terminal/alacritty.nix
                      ./modules/gui/qutebrowser.nix
                      ./modules/gui/firefox.nix
                      ./modules/gui/mpv.nix
                      ./modules/gui/zathura.nix
                      ./modules/editors/emacs.nix
                      ./modules/terminal/fish.nix
                      ./modules/terminal/git.nix
                      ./modules/terminal/bat.nix
                      ./modules/terminal/fzf.nix
                      ./modules/terminal/lsd.nix
                      ./modules/terminal/starship.nix
                      ./modules/terminal/tmux.nix
                      ./modules/services/gpg.nix
                    ];
                  };
                })
            ];
          inherit pkgs;
        };
        server1 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            [
              defaults
              ./machines/mainserver/configuration.nix
              home-manager.nixosModules.home-manager
              ({
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.hac = { ... }: {
                  imports = [
                    ./machines/mainserver/home.nix
                    ./modules/terminal/fish.nix
                    ./modules/terminal/git.nix
                    ./modules/terminal/bat.nix
                    ./modules/terminal/fzf.nix
                    ./modules/terminal/lsd.nix
                    ./modules/terminal/starship.nix
                    ./modules/terminal/tmux.nix
                    ./modules/services/gpg.nix
                  ];
                };
              })
            ];
          inherit pkgs;
        };
      };
    };
}
