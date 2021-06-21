# This file is generated from "README.org"
{ config, pkgs, fetchgit, ... }:
{

  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # import encrypted user settings/secrets
      ./usersettings.nix
    ];

  # Boot{{{
  # Use the systemd-boot EFI boot loader.
  boot = {
    extraModulePackages = with config.boot.kernelPackages; [ xpadneo ];
    extraModprobeConfig = ''
      options bluetooth disable_ertm=Y
    '';
    kernelParams = [
      "raid0.default_layout=2"
    ];
  };
  # }}}
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  systemd.services.systemd-udev-settle.serviceConfig.TimeoutSec = 5;
  systemd.services.NetworkManager-wait-online.enable = false;
  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  boot.plymouth.enable = true;
  
  # Luks encrypted partition
  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/02d555e3-f8d2-4617-860e-06c08bec521b";
  
  # Root partition after unlocking luks
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/ba342cbb-2d6c-453d-8d25-df8fad829c37";
      fsType = "xfs";
    };
  
  # Boot partition
  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/E5A7-FB2A";
      fsType = "vfat";
    };
  
  # nvme
  fileSystems."/media/plotnvme1" =
    {
      device = "/dev/disk/by-uuid/2d0338d3-bf7e-4cc7-9a24-149073864af7";
      fsType = "xfs";
    };
  
  # plotstore1
  fileSystems."/media/plotstore1" =
    {
      device = "/dev/disk/by-uuid/82ef2a4e-85b7-44a5-bb55-e251992ae882";
      fsType = "ext4";
    };
  # plotstore2
  fileSystems."/media/plotstore2" =
    {
      device = "/dev/disk/by-uuid/0e66fbd5-a36e-4b86-9d2a-b17865f0bb85";
      fsType = "ext4";
    };
  # plotstore3
  fileSystems."/media/plotstore3" =
    {
      device = "/dev/disk/by-uuid/2cfb3ba2-9672-4c78-8f8d-97077b1fed0b";
      fsType = "ext4";
    };
  # plotstore4
  fileSystems."/media/plotstore4" =
    {
      device = "/dev/disk/by-uuid/51139f8b-cc05-4906-a3b7-0141c7b91baf";
      fsType = "ext4";
    };
  # plotstore5
  fileSystems."/media/plotstore5" =
    {
      device = "/dev/disk/by-uuid/9b6bf31c-334a-40f5-8456-1ecc5c3b2a99";
      fsType = "ext4";
    };
  # plotstore6
  fileSystems."/media/plotstore6" =
    {
      device = "/dev/disk/by-uuid/37a868ba-52d6-4303-bd89-2ebd945371f5";
      fsType = "ext4";
    };
  # plotstore7
  fileSystems."/media/plotstore7" =
    {
      device = "/dev/disk/by-uuid/aafe2fd7-cfab-49b6-9ef9-7dc9162fc017";
      fsType = "ext4";
    };
  # plotstore8
  fileSystems."/media/plotstore8" =
    {
      device = "/dev/disk/by-uuid/04faa23b-7e56-4edc-a47c-b66593982cb1";
      fsType = "ext4";
    };
  # plotstore9
  fileSystems."/media/plotstore9" =
    {
      device = "/dev/disk/by-uuid/f2499cb3-fbc3-4045-96f4-af4e469571e5";
      fsType = "ext4";
    };
  # plotstore10
  fileSystems."/media/plotstore10" =
    {
      device = "/dev/disk/by-uuid/3a3d17c9-cfac-461f-bda8-eab311c8012d";
      fsType = "ext4";
    };
  # plotstore11
  fileSystems."/media/plotstore11" =
    {
      device = "/dev/disk/by-uuid/454afe17-1deb-4319-85a1-40090e2d06c3";
      fsType = "xfs";
    };
  # Swap partition uuid
  swapDevices =
    [{ device = "/dev/disk/by-uuid/aedff2c4-f34c-410c-9612-aa9df0dd3cef"; }];

  hardware.enableRedistributableFirmware = true;
  sound.enable = true;
  hardware.cpu.amd.updateMicrocode = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = with pkgs; [
    rocm-opencl-icd
    rocm-opencl-runtime
    amdvlk
  ];
  hardware.opengl.extraPackages32 = with pkgs.unstable.pkgsi686Linux; [ libva ];

  programs = {
    dconf.enable = true;
    adb.enable = true;
    java.enable = true;
    java.package = pkgs.unstable.jdk;
  };

  services.fstrim.interval = "daily";

  networking.hostName = "desktop1";
  networking.firewall.allowedTCPPorts = [ 8080 9090 9777 10128 8444 8555 8447 6885 6888 8744 8777 9444 9447 8520 8547 55520 ];
  # Networking{{{
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    dhcpcd.enable = false;
    useNetworkd = true;
    useDHCP = false;
    interfaces.enp9s0.useDHCP = true;
    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
  };
  #}}}

  environment.variables = (import ../../modules/common/globalvars.nix);

  virtualisation = import (../../modules/virtualisation/default.nix);
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_390;
  services.xserver.wacom.enable = true;
  services.udev.packages = [ pkgs.dolphinEmu ];
  services.avahi.enable = true;
  # Required to use Kodi Zeroconf

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
