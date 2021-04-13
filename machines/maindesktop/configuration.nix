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
      "amdgpu.vm_fragment_size=9"
      "amdgpu.ppfeaturemask=0xffffffff"
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

  networking.hostName = "desktop1";
  networking.firewall.allowedTCPPorts = [ 8080 9090 9777 10128 8444 8555 ];
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
  services.xserver.videoDrivers = [ "amdgpu" ];
  services.xserver.deviceSection = ''
  Option "TearFree" "true"
  '';
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
