{ pkgs, ... }:
{
  enable = true;
  package = pkgs.nur.repos.reedrw.picom-next-ibhagwan;
  backend = "glx";
  experimentalBackends = true;
  opacityRule = [
    "85:class_g  = 'Zathura'"
    "85:class_g  = 'TelegramDesktop'"
    "85:class_g  = 'Discord'"
    "85:class_g  = 'Emacs'"
    "100:class_g = 'keynav'"
  ];
  extraOptions = ''
    detect-client-opacity = true;
    detect-rounded-corners = true;
    blur:
    {
        method = "kawase";
        strength = 10;
        background = false;
        background-frame = false;
        background-fixed = false;
    };
    blur-background-exclude = [
        "class_g = 'keynav'"
    ];
    corner-radius = 5;
    rounded-corners-exclude = [
        "window_type = 'dock'",
        "_NET_WM_STATE@:32a *= '_NET_WM_STATE_FULLSCREEN'",
        "class_g = 'keynav'",
    ];
    round-borders = 1;
    round-borders-exclude = [
        "class_g = 'keynav'"
    ];
  '';
}
