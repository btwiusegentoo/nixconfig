{
  enable = true;
  enableSshSupport = true;
  grabKeyboardAndMouse = true;
  extraConfig = ''
    allow-emacs-pinentry
    allow-loopback-pinentry
  '';
  pinentryFlavor = "curses";
}
