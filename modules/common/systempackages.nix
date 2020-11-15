{ pkgs, unstable, master, config, ... }:
let
    my-python-packages = python-packages: with python-packages; [
        jedi
        pynvim
        numpy
        pip
        pylint
        pymodoro
    ];
    python-with-my-packages = unstable.python37.withPackages my-python-packages;
    # pymodoro{{{
    pymodoro = unstable.python37Packages.buildPythonPackage rec {
        pname = "pymodoro";
        version = "1.14";
        src = pkgs.fetchFromGitHub {
            owner = "dattanchu";
            repo = "pymodoro";
            rev = "0f24e5012945bb241f144fab2672d3d880c00b5c";
            sha256 = "0pz40whyijkxbd7mi059bzgrhmqzrcrd7fzmwl11hndpn9zl0r37";
        };
    };
    # }}}
#
in
{
    environment.systemPackages = with pkgs; [
        wget
        git
        git-crypt
        bat
        sox
        unstable.neovim
        unstable.nodejs
        unstable.yarn
        unstable.openssh
        unzip
        unstable.gnumake
        unstable.gcc10
        unstable.xorg.xbacklight
        python-with-my-packages
        cachix
        unstable.ipad_charge
        unstable.fish
        home-manager
        unstable.flashfocus
        # deps
        libffi
        xorg.libxcb
    ];
}
