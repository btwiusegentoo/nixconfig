# This file is generated from "README.org"
{
  services.clamav.daemon.enable = true;
  services.clamav.daemon.extraConfig = ''
  
  '';
  services.clamav.updater.enable = true;
  services.clamav.updater.frequency = 24;
  services.clamav.updater.interval = "hourly";
  services.clamav.updater.extraConfig = ''
  
  '';
}
