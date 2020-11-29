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

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.timeout = 0;
  boot.loader.grub.extraConfig = ''
  if keystatus --shift ; then
      set timeout=-1
  else
      set timeout=0
  fi
  '';
  boot.loader.grub.copyKernels = true;
  systemd.services.systemd-udev-settle.serviceConfig.TimeoutSec = 5;
  systemd.services.NetworkManager-wait-online.enable = false;
  boot.kernelPackages = pkgs.unstable.linuxPackages_latest;
  boot.plymouth.enable = true;

  # Luks encrypted partition
  boot.loader.grub.enableCryptodisk = true;
  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/29e93967-944e-4306-adb2-8d2441ca5ed5";
  
  # Set what drive to install grub
  boot.loader.grub.device = "/dev/sda";
  
  # Root partition after unlocking luks
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/4979bbc1-28e8-4505-9a3e-2d2fc0f41fa3";
      fsType = "xfs";
    };
  
  # Boot partition
  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/f5979f75-d494-49ba-a149-82a612dcf543";
      fsType = "ext2";
    };
  
  # Swap partition uuid
  swapDevices =
    [{ device = "/dev/disk/by-uuid/8bb10870-d71d-484b-a861-d702a2f55484"; }];

  hardware.enableRedistributableFirmware = true;
  sound.enable = true;
  hardware.cpu.intel.updateMicrocode = true;

  networking.hostName = "server1";
  # Networking{{{

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    dhcpcd.enable = false;
    useNetworkd = true;
    useDHCP = false;
    interfaces.wlp0s4f1u3u4.useDHCP = true;
    networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
  };
  #}}}

  environment.variables = (import ../../modules/common/globalvars.nix);

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
