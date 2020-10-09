{ pkgs }:
{
    enable = true;
    support32Bit = true;
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    package = pkgs.pulseaudioFull;
    daemon.config = {
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
