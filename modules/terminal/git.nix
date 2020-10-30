{ pkgs, unstable }:

{
    enable = true;
    package = unstable.git;
    extraConfig = {
        init = {
            defaultBranch = "main";
        };
        url = {
            "git@github.com:" = {
                pushinsteadOf = "https://github.com/";
            };
        };
    };
    userName = "btwiusegentoo";
    userEmail = "66811008+btwiusegentoo@users.noreply.github.com";
}
