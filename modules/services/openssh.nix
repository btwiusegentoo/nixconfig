# This file is generated from "README.org"
{
  services.openssh = {
    enable = true;
    challengeResponseAuthentication = true;
    forwardX11 = false;
    openFirewall = true;
    passwordAuthentication = false;
    permitRootLogin = "no";
  };
}
