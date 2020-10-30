{
    description = "NixOS configuration for all machines";


    inputs = {
        home-manager = {
            url = "github:rycee/home-manager";
            inputs = {
                nixpkgs.follows = "nixpkgs";
            };
        };

        nixpkgs = {
            url = "github:nixos/nixpkgs/nixos-20.09";
        };
        unstable = {
            url = "github:nixos/nixpkgs/nixos-unstable";
        };
        master = {
            url = "github:nixos/nixpkgs/master";
        };
    };


    outputs = inputs@{ self, home-manager, nixpkgs, unstable, master, ... }:
        let
            pkgs = (import nixpkgs) {
                system = "x86_64-linux";
                config = { allowUnfree = true; };
            };
        in
        {
            nixosConfigurations = {
                desktop1 = nixpkgs.lib.nixosSystem {
                    system = "x86_64-linux";
                    modules =
                        let
                            defaults = { pkgs, ... }: {
                                _module.args.unstable = import inputs.unstable {
                                    inherit (pkgs.stdenv.targetPlatform) system;
                                    config.allowUnfree = true;
                                };
                                _module.args.master = import inputs.master {
                                    inherit (pkgs.stdenv.targetPlatform) system;
                                    config.allowUnfree = true;
                                };
                            };
                        in [
                            defaults
                            ./machines/maindesktop/configuration.nix
                            home-manager.nixosModules.home-manager
                                ({
                                    home-manager.useGlobalPkgs = true;
                                    home-manager.useUserPackages = true;
                                    home-manager.users.btw = { ... }: {
                                        imports = [
                                            ./machines/maindesktop/home.nix
                                        ];
                                        _module.args.unstable = import inputs.unstable {
                                            inherit (pkgs.stdenv.targetPlatform) system;
                                            config.allowUnfree = true;
                                        };
                                        _module.args.master = import inputs.master {
                                            inherit (pkgs.stdenv.targetPlatform) system;
                                            config.allowUnfree = true;
                                        };
                                    };
                                })
                        ];
                };
                laptop1 = nixpkgs.lib.nixosSystem {
                    system = "x86_64-linux";
                    modules =
                        let
                            defaults = { pkgs, ... }: {
                                _module.args.unstable = import inputs.unstable {
                                    inherit (pkgs.stdenv.targetPlatform) system;
                                    config.allowUnfree = true;
                                };
                                _module.args.master = import inputs.master {
                                    inherit (pkgs.stdenv.targetPlatform) system;
                                    config.allowUnfree = true;
                                };
                            };
                        in [
                            defaults
                            ./machines/mainlaptop/configuration.nix
                            home-manager.nixosModules.home-manager
                                ({
                                    home-manager.useGlobalPkgs = true;
                                    home-manager.useUserPackages = true;
                                    home-manager.users.x230 = { ... }: {
                                        imports = [
                                            ./machines/mainlaptop/home.nix
                                        ];
                                        _module.args.unstable = import inputs.unstable {
                                            inherit (pkgs.stdenv.targetPlatform) system;
                                            config.allowUnfree = true;
                                        };
                                        _module.args.master = import inputs.master {
                                            inherit (pkgs.stdenv.targetPlatform) system;
                                            config.allowUnfree = true;
                                        };
                                    };
                                })
                        ];
                };
                server1 = nixpkgs.lib.nixosSystem {
                    system = "x86_64-linux";
                    modules =
                        let
                            defaults = { pkgs, ... }: {
                                _module.args.unstable = import inputs.unstable {
                                    inherit (pkgs.stdenv.targetPlatform) system;
                                    config.allowUnfree = true;
                                };
                                _module.args.master = import inputs.master {
                                    inherit (pkgs.stdenv.targetPlatform) system;
                                    config.allowUnfree = true;
                                };
                            };
                        in [
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
                                        _module.args.unstable = import inputs.unstable {
                                            inherit (pkgs.stdenv.targetPlatform) system;
                                            config.allowUnfree = true;
                                        };
                                        _module.args.master = import inputs.master {
                                            inherit (pkgs.stdenv.targetPlatform) system;
                                            config.allowUnfree = true;
                                        };
                                    };
                                })
                        ];
                };

            };
        };
}
