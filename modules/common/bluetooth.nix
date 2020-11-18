{ pkgs }:
{
  enable = true;
  package = pkgs.bluezFull;
  config = {
    General = {
      ControllerMode = "bredr";
    };
  };
}
