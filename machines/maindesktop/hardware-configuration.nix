{ config, lib, pkgs, ... }:

{

  hardware.enableRedistributableFirmware = true;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  # point root to xfs root partition
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/ba342cbb-2d6c-453d-8d25-df8fad829c37";
      fsType = "xfs";
    };

  # point this to where is encrypted with luks
  boot.initrd.luks.devices."root".device = "/dev/disk/by-uuid/02d555e3-f8d2-4617-860e-06c08bec521b";

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/E5A7-FB2A";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/aedff2c4-f34c-410c-9612-aa9df0dd3cef"; }];

  nix.maxJobs = lib.mkDefault 16;
}
