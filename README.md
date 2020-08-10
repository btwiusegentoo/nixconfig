\
<code><p align="center">
home-manager + xmonad + neovim💛
</p></code>




![screenshot1](/pictures/screenshot1.png)
![screenshot2](/pictures/screenshot2.png)
![wallpaper](/pictures/wallpaper1.png)




[![nixos](https://img.shields.io/static/v1?style=for-the-badge&logo=nixos&label=%E2%A0%80&message=nixos&labelColor=azure&color=cornflowerblue)](https://nixos.org/)
[![neovim](https://img.shields.io/static/v1?style=for-the-badge&logo=neovim&label=%E2%A0%80&message=neovim&labelColor=blue&color=green)](https://neovim.io/)
[![xmonad](https://img.shields.io/static/v1?style=for-the-badge&logo=haskell&label=%E2%A0%80&message=xmonad&labelColor=blueviolet&color=black)](https://xmonad.org/)



![Haskell Linter](https://github.com/btwiusegentoo/nixconfig/workflows/Haskell%20Linter/badge.svg)

<h1 align="center">
NixOS configuration
</h1>


note: this repo is a personally backup. and my system is still work in progress.
I try to install packages/manage config files declarative using `configuration.nix` and `home.nix` as much as possible.
I personally use vim folds(marker) to make config files readable.
If you are new to nixos, maybe you can get idea how to install neovim plugins and fish plugins,haskell packages,python packages from this config.

## Installation

I still have many room of improvement and I don't recommend installing this config.
Maybe I will force push or make breaking changes without notice. but you can try if you want.
Make sure you already have formatted partition and mounted to /mnt. I recommend to encrypt with luks.
If no, follow nixos manual [here](https://nixos.org/nixos/manual/)

### Clone repo and symlink
Choose your username here.
You have to make home directory and clone repo inside user home directory.
It's hardcoded to use `$HOME/.nixconfig` on my config. You can read my config and change if you don't like.

inside NixOS live USB
```bash
git clone https://github.com/btwiusegentoo/nixconfig /mnt/home/<username>/.nixconfig
ln -s /mnt/home/<username>/.nixconfig/configuration.nix /mnt/etc/nixos/configuration.nix
```

### Setup
Before installation, you have to set your personal variables and settings.
Otherwise installation will fail.
copy example config
```bash
cp examples/uservars-example.nix uservars.nix
cp examples/usersettings-example.nix usersettings.nix
```
And edit.
You have to set your personal configs like username,timezone,kbd layout etc here.
You can imagine setup wizard of phones,macOS,Windows.
I decided to use this way because 
- `uservars.nix` and `usersettings.nix` is included in .gitignore. I can put my secrets here.
- I can have a template so I don't have to make a private repo for my secrets.
- gives more extensibility.

You have to edit hardware-configuration.nix. I have my configuration (luks+lvm+xfs) in my repo but everyone have different partition UUIDs.

### Install
After configuring, just run
```bash
nixos-install
```

### Post install
- don't forget to set user password.
- In order to set user password, You have to login to root `su` and change user password `passwd <username>`
- You have to run `doas chown -R <username> ~/.nixconfig` after logged in to make configs editable.
- You will have to update configuration.nix symlink after installation.

I think will make a script or TUI or something to make installation easier in the future. it's a bit complicated now.

## Summary of Features



-   home-manager
-   xmonad+xmobar
-   neovim nightly + plugins(includes coc)
-   haskell environment
-   Japanese IME(mozc+fcitx)
-   Keybinds optimized for Dvorak layout and HHKB
-   Alacritty
-   qutebrowser
-   fish shell + plugins
-   Tamzen bitmap font as main font
-   NerdFont as fallback(GohuFont)
-   Apple emoji
-   Apple SF fonts installed
-   Some packages from unstable while running stable branch


## Credits

[NixOS/nixos-artwork](https://github.com/NixOS/nixos-artwork/tree/master/logo)
-\> NixOS logo. used on wallpaper. Just changed colors. used under
[CC-BY license](https://creativecommons.org/licenses/by/4.0/)
