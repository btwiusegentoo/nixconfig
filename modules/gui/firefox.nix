{ pkgs, unstable, ... }:

{
    enable = true;
    package = unstable.firefox;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        vimium
        ublock-origin
    ];
    profiles = {
        default = {
            isDefault = true;
            name = "Default";
            settings = {
                "browser.startup.homepage" = "https://btwiusegentoo.github.io/start.html";
                "browser.search.isUS" = true;
            };
        };
    };
}
