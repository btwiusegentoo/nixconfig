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

  boot = {
    kernelModules = [ "i915" "tpm-rng" ];
    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];
    extraModprobeConfig = ''
      options bluetooth disable_ertm=Y
      options iwlwifi 11n_disable=8 bt_coex_active=N
      options thinkpad_acpi experimental=1 fan_control=1
    '';
    kernelParams = [
      "acpi_osi='!Windows 2012'"
      "acpi_backlight=vendor"
    ];
  };
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
  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/27740e7b-5bb7-482a-94dc-72df547f1f66";
  
  # Set what drive to install grub
  boot.loader.grub.device = "/dev/sda";
  
  # Root partition after unlocking luks
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/b904a3b9-a30e-425c-9e6c-6f8a56cedbf9";
      fsType = "xfs";
    };
  
  # Boot partition
  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/f548cd01-269e-4b56-8aab-2cf06f278f88";
      fsType = "ext2";
    };
  
  # Swap partition uuid
  swapDevices =
    [{ device = "/dev/disk/by-uuid/1c57f70d-7083-4109-8cd5-f407a51d39cf"; }];

  hardware.enableRedistributableFirmware = true;
  sound.enable = true;
  hardware.cpu.intel.updateMicrocode = true;
  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiIntel
    vaapiVdpau
    libvdpau-va-gl
    intel-media-driver
  ];

  programs = {
    dconf.enable = true;
    adb.enable = true;
    java.enable = true;
    java.package = pkgs.unstable.jdk;
  };

  networking.hostName = "laptop1";
  # Networking{{{
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking = {
    dhcpcd.enable = false;
    useDHCP = false;
    interfaces.wlan0.useDHCP = false;
    networkmanager = {
      enable = true;
      wifi = {
        backend = "iwd";
        macAddress = "random";
        scanRandMacAddress = true;
      };
    };
  };
  #}}}

  environment.variables = (import ../../modules/common/globalvars.nix);

  virtualisation = import (../../modules/virtualisation/default.nix);

<<<<<<< HEAD
    systemd.user.services.xkb-restore = {
        description = "Restore keyboard layout after suspend";
        after = [ "suspend.target" "graphical-session.target" ];
        serviceConfig = {
          Type = "simple";
          Environment = "DISPLAY=:0";
          ExecStartPre = "/usr/bin/env sleep 3";
          ExecStart = "${pkgs.bash}/bin/bash -c \"${pkgs.xorg.xkbcomp}/bin/xkbcomp -i $(${pkgs.xorg.xinput}/bin/xinput list | sed -n 's/.*Translated.*id=\\\([0-9]*\\\).*keyboard.*/\\\1/p') /etc/thinkpadlayout.xkb :0\"";
        };
        wantedBy = [ "suspend.target" "graphical-session.target" ];
    };
=======
  systemd.user.services.xkb-restore = {
    description = "Restore keyboard layout after suspend";
    after = [ "suspend.target" "graphical-session.target" ];
    serviceConfig = {
      Type = "simple";
      Environment = "DISPLAY=:0";
      ExecStartPre = "/usr/bin/env sleep 3";
      ExecStart = "${pkgs.bash}/bin/bash -c \"${pkgs.xorg.xkbcomp}/bin/xkbcomp -i $(${pkgs.xorg.xinput}/bin/xinput list | sed -n 's/.*Translated.*id=\\\([0-9]*\\\).*keyboard.*/\\\1/p') /etc/thinkpadlayout.xkb :0\"";
    };
    wantedBy = [ "suspend.target" "graphical-session.target" ];
  };
>>>>>>> 1c5393f55cef425aad643174325fc948d244971c

  services.xserver.videoDrivers = [ "intel" ];
  services.xserver.deviceSection = ''
  Option "TearFree" "true"
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?

}
