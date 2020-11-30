# This file is generated from "README.org"
{
  services.openssh.enable = true;
  services.openssh.challengeResponseAuthentication = true;
  services.openssh.forwardX11 = false;
  services.openssh.openFirewall = true;
  services.openssh.passwordAuthentication = false;
  services.openssh.permitRootLogin = "no";
}
