{ pkgs }:
{
  enable = true;
  package = pkgs.unstable.git;
  extraConfig = {
    init = {
      defaultBranch = "main";
    };
    url = {
      "git@github.com:" = {
        pushinsteadOf = "https://github.com/";
      };
    };
    core = {
      editor = "emacsclient -c";
    };
  };
  userName = "btwiusegentoo";
  userEmail = "66811008+btwiusegentoo@users.noreply.github.com";
}
