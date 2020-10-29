{ pkgs, ... }:

{
    package = pkgs.nixUnstable;
    distributedBuilds = true;

    extraOptions = ''
        experimental-features = nix-command flakes
        builders-use-substitutes = true
    '';

    buildMachines = [
    {
        hostName = "desktop1";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 3;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
    }
    {
        hostName = "server1";
        system = "x86_64-linux";
        maxJobs = 2;
        speedFactor = 0;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
    }
    {
        hostName = "laptop1";
        system = "x86_64-linux";
        maxJobs = 4;
        speedFactor = 2;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
    }
    ];
}
