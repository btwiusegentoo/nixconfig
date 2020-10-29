{
    description = "NixOS configuration for all machines";

    inputs = {
        home-manager = {
            url = "github:rycee/home-manager/bqv-flakes";
            inputs = {
                nixpkgs.follows = "nixpkgs";
                unstable.follows = "unstable";
                master.follows = "master";
            };
        };

        nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
        unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
        master.url = "github:NixOS/nixpkgs/master";
    };

    outputs = { self, home-manager, nixpkgs, unstable, master, ... }:
        let
            pkgs = (import nixpkgs) {
                system = "x86_64-linux";
            };
            unstable = (import unstable) {
                system = "x86_64-linux";
            };
            master = (import master) {
                system = "x86_64-linux";
            };
            username = (import ./uservars.nix).username;

        in
        {
            nixosConfigurations = {
                desktop1 = nixpkgs.lib.nixosSystem {
                    system = "x86_64-linux";
                    modules = [
                        ./machines/maindesktop/configuration.nix
                        home-manager.nixosModules.home-manager
                        {
                            home-manager.useGlobalPkgs = true;
                            home-manager.useUserPackages = true;
                            home-manager.users.${username} =  ./machines/maindesktop/home.nix;
                        }
                    ];
                };
                laptop1 = nixpkgs.lib.nixosSystem {
                    system = "x86_64-linux";
                    modules = [
                        ./machines/mainlaptop/configuration.nix
                        home-manager.nixosModules.home-manager
                        {
                            home-manager.useGlobalPkgs = true;
                            home-manager.useUserPackages = true;
                            home-manager.users.${username} =  ./machines/mainlaptop/home.nix;
                        }
                    ];
                };
                server1 = nixpkgs.lib.nixosSystem {
                    system = "x86_64-linux";
                    modules = [
                        ./machines/mainserver/configuration.nix
                        home-manager.nixosModules.home-manager
                        {
                            home-manager.useGlobalPkgs = true;
                            home-manager.useUserPackages = true;
                            home-manager.users.${username} =  ./machines/mainserver/home.nix;
                        }
                    ];
                };

            };
        };
}
