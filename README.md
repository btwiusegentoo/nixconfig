<br>
<br>
<p align="center"><code>home-manager + xmonad + neovim💛</code></p>
<br>
<br>
<br>

![screenshot1](/screenshot1.png)
![screenshot2](/screenshot2.png)
![wallpaper](/wallpaper2.png)
<br>
<br>
<br>

[![nixos](https://img.shields.io/static/v1?style=for-the-badge&logo=nixos&label=%E2%A0%80&message=nixos&labelColor=azure&color=cornflowerblue)](https://nixos.org/)
[![neovim](https://img.shields.io/static/v1?style=for-the-badge&logo=neovim&label=%E2%A0%80&message=neovim&labelColor=blue&color=green)](https://neovim.io/)
[![xmonad](https://img.shields.io/static/v1?style=for-the-badge&logo=haskell&label=%E2%A0%80&message=xmonad&labelColor=blueviolet&color=black)](https://xmonad.org/)
<br>
<br>
![Haskell Linter](https://github.com/btwiusegentoo/nixconfig/workflows/Haskell%20Linter/badge.svg)

<br>
<h1 align="center">NixOS configuration</h1>
<br>
note: this repo is a personally backup. and my system is still work in progress.<br>
I try to install packages/manage config files declarative using <code>configuration.nix</code> and <code>home.nix</code> as much as possible.<br>
I personally use vim's folding(marker) to make config files readable.<br>
If you are new to nixos, maybe you can get idea how to install neovim plugins and fish plugins,haskell packages,python packages from this config.
<br>
<h2>Installation</h2>
<br>
I don't recommend installing this config.
but you can try if you want.
It's hardcoded now in several places to use `/home/btw/.nixconfig.` so you have to use as it is or change username.
You need to have `git` installed and you have to be on nixos-stable.
You have to change hardware configurations.
```bash
git clone https://github.com/btwiusegentoo/nixconfig /home/btw/.nixconfig
ln -s /home/btw/.nixconfig/configuration.nix /etc/nixos/configuration.nix
```
home-manager is configured as module so channel is not needed anymore.

<br>
<h2>Summary of Features</h2>
<br>
<ul type="square">
    <li>home-manager</li>
    <li>xmonad+xmobar</li>
    <li>neovim nightly + plugins(includes coc)</li>
    <li>haskell environment</li>
    <li>Japanese IME(mozc+fcitx)</li>
    <li>Keybinds optimized for Dvorak layout and HHKB</li>
    <li>Alacritty</li>
    <li>qutebrowser</li>
    <li>fish shell + plugins</li>
    <li>Tamzen bitmap font as main font</li>
    <li>NerdFont as fallback(GohuFont)</li>
    <li>Apple emoji</li>
    <li>Apple SF fonts installed</li>
    <li>Some packages from unstable while running stable branch</li>
    <li>It assumes you are using 1080p and scales to WQHD. You will want to disable if you are using other resolution. You can find in home.nix (xsession.profileExtra) If you will change resolution, I think you will also want to change font sizes.</li>

</ul>

<h2>Credits</h2>
<a href="https://github.com/NixOS/nixos-artwork/tree/master/logo">NixOS/nixos-artwork</a> -> NixOS logo. used on wallpaper. Just changed colors. used under <a href="https://creativecommons.org/licenses/by/4.0/">CC-BY license</a>
