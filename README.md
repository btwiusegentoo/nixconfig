
<h1 align="center">
NixOS configuration
</h1>

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)
![Haskell Linter](https://github.com/btwiusegentoo/nixconfig/workflows/Haskell%20Linter/badge.svg)

![screenshot1](/pictures/screenshot1.png)
![screenshot2](/pictures/screenshot2.png)
![wallpaper](/pictures/wallpaper1.png)


note: this repo is a personally backup. and my system is still work in progress.
I try to install packages/manage config files declarative using `configuration.nix` and `home.nix` as much as possible.
I personally use vim folds(marker) to make config files readable.
I will store here my configs for every PC or server I have for convenience.


## Installation

I still have many room of improvement and I don't recommend installing this config.
Maybe I will force push or make breaking changes without notice. but you can try if you want.
Make sure you already have formatted partition and mounted to /mnt. I recommend to encrypt with luks.
If you don't know how to format partition, follow nixos manual [here](https://nixos.org/nixos/manual/)
or, search google.

## About directory structure
*  `machines` machine specific configuration.nix and home.nix, cachix settings etc here
* `configs` common config files that most goes inside `$HOME/.config`
* `doom.d` doom-emacs config
* `modules` Nix expressions for each programs. I separated in modules to make easier to import from other machines.
* `haskell` currently,xmonad and xmobar config
* `icons` .xpm icons. I use mostly for xmobar
* `examples` template for user specific configs that you need to copy to project root before installation.
* `overlays` nix expressions for overlays here. also calls custom packages from here.
* `packages` custom packages
* `patches` mostly patches I use for overlays or custom packages.
* `pictures` Pictures like screenshot and wallpaper
* `textfiles` All textfiles that is used for installation. not documentation.

### Clone repo and symlink
Choose your username here.
You have to make home directory and clone repo inside user home directory.
It's hardcoded to use `$HOME/.nixconfig` on my config. You can read my config and change if you don't like.

inside NixOS live USB
```bash
git clone https://github.com/btwiusegentoo/nixconfig /mnt/home/<username>/.nixconfig
ln -s /mnt/home/<username>/.nixconfig/machines/maindesktop/configuration.nix /mnt/etc/nixos/configuration.nix
```

### Setup
Before installation, you have to set your personal variables and settings.
Otherwise installation will fail.
First, cd into cloned repo, and copy example config
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
- You have to git clone doom-emacs manually.

I think I will make a script or TUI or something to make installation easier in the future. it's a bit complicated now.

## Summary of Features



-   home-manager
-   xmonad+xmobar
-   neovim nightly + plugins(includes coc)
-   Added doom-emacs recently.
-   haskell environment
-   Japanese IME(mozc+fcitx)
-   Keybinds optimized for Dvorak layout and HHKB for xmonad(trust me, default vim hjkl keybinds works great in Dvorak)
-   Alacritty
-   qutebrowser
-   fish shell + plugins
-   Spleen font
-   NerdFont as fallback(GohuFont)
-   Apple emoji
-   Apple SF fonts installed
-   Some packages from unstable while running stable branch


## Credits

- [NixOS/nixos-artwork](https://github.com/NixOS/nixos-artwork/tree/master/logo) -> NixOS logo. Changed colors and used in wallpaper. Also converted to xpm(without modifications. only cropped) and used in xmobar. used under [CC-BY license](https://creativecommons.org/licenses/by/4.0/)
- [Tux Logo](https://commons.wikimedia.org/wiki/File:NewTux.svg) used in xmobar
- [Feather Icons](https://github.com/feathericons/feather) used in xmobar
