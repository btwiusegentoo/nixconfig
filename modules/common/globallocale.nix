{ pkgs, config, ... }:

{
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
        font = "Lat2-Terminus16";
        keyMap = "dvorak";
    };

    i18n.inputMethod.enabled = "fcitx";
    i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
}
