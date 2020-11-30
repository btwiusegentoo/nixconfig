# This file is generated from "README.org"
{ pkgs, ... }:
{
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.pulseaudio.support32Bit = true;
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  hardware.pulseaudio.daemon.config = {
      default-sample-rate = "48000";
      alternate-sample-rate = "44100";
      default-sample-channels = "2";
      default-channel-map = "front-left,front-right";
      default-fragments = "2";
      default-fragment-size-msec = "125";
      enable-lfe-remixing = "no";
      high-priority = "yes";
      nice-level = "-11";
      realtime-scheduling = "yes";
      realtime-priority = "9";
      rlimit-rtprio = "9";
      resample-method = "soxr-vhq";
      daemonize = "no";
      default-sample-format = "float32le";
  };
}
