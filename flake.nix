# This file is generated from "README.org"
{
  description = "NixOS configuration for all machines";

    inputs = {
      home-manager = {
        url = "github:rycee/home-manager";
        inputs = {
          nixpkgs.follows = "nixpkgs";
        };
      };
      nur.url = "github:nix-community/NUR";
      emacs.url = "github:berbiche/emacs-pgtk-nativecomp-overlay";
  
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
              config = { allowUnfree = true; };
              overlays = attrValues self.overlays;
            };

            defaults = { pkgs, ... }: {
              imports = [
                ./cachix.nix
                ./modules/common/nix.nix
                ./modules/common/doas.nix
                ./modules/common/console.nix
                ./modules/common/bluetooth.nix
                ./modules/common/etcfiles.nix
                ./modules/common/systempackages.nix
                ./modules/common/globallocale.nix
                ./modules/services/pulseaudio.nix
                ./modules/services/openssh.nix
                ./modules/services/zram.nix
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
                  nur = import inputs.nur { nurpkgs = final; pkgs = final; };
                };
                emacsPgtk = final: prev: {
                  emacsGccPgtk = inputs.emacs.packages.${final.system}.emacsGccPgtk;
                };
                unstable = final: prev: {
                  unstable = import inputs.unstable {
                    system = final.system;
                    config.allowUnfree = true;
                  };
                };
                master = final: prev: {
                  master = import inputs.master {
                    system = final.system;
                    config.allowUnfree = true;
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
                      ./modules/common/fonts.nix
                      home-manager.nixosModules.home-manager
                      ({
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        home-manager.users.btw = { ... }: {
                          imports = [
                            ./machines/maindesktop/home.nix
                            ./modules/services/dunst.nix
                            ./modules/services/picom.nix
                            ./modules/terminal/alacritty.nix
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
                      ./modules/common/fonts.nix
                      home-manager.nixosModules.home-manager
                      ({
                        home-manager.useGlobalPkgs = true;
                        home-manager.useUserPackages = true;
                        home-manager.users.x230 = { ... }: {
                          imports = [
                            ./machines/mainlaptop/home.nix
                            ./modules/services/dunst.nix
                            ./modules/services/picom.nix
                            ./modules/terminal/alacritty.nix
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
                          ];
                        };
                      })
                    ];
                  inherit pkgs;
                };
      };
    };
}
