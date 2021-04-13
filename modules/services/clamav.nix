# This file is generated from "README.org"
{
  services.clamav.daemon.enable = true;
  services.clamav.daemon.extraConfig = ''
  ExcludePath ^/media/hdd1/
  ExcludePath ^/media/hdd2/
  ExcludePath ^/media/hdd3/
  ExcludePath ^/media/raid0/
  '';
  services.clamav.updater.enable = true;
  services.clamav.updater.frequency = 24;
  services.clamav.updater.interval = "hourly";
  services.clamav.updater.extraConfig = ''
  
  '';
}
