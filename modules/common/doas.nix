# This file is generated from "README.org"
{
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    wheelNeedsPassword = true;
    extraRules = [
      { groups = [ "wheel" ]; noPass = false; keepEnv = true; persist = true; }
    ];
  };
}
