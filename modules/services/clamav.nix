# This file is generated from "README.org"
{
  services.clamav.daemon = {
    enable = true;
    extraConfig = ''
    
    '';
  };
  services.clamav.updater = {
    enable = true;
    frequency = 24;
    interval = "hourly";
    extraConfig = ''
    
    '';
  };
}
