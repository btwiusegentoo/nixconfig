# This file is generated from "README.org"
{ pkgs, config, ... }:

{
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.inputMethod.enabled = "fcitx";
  i18n.inputMethod.fcitx.engines = with pkgs.fcitx-engines; [ mozc ];
}
