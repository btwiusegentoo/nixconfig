{
    description = "NixOS configuration for all machines";

    inputs = {
        home-manager = {
            url = "github:rycee/home-manager";
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
                            home-manager.users.btw =  ./machines/maindesktop/home.nix;
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
                            home-manager.users.x230 =  ./machines/mainlaptop/home.nix;
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
                            home-manager.users.hac =  ./machines/mainserver/home.nix;
                        }
                    ];
                };

            };
        };
}
