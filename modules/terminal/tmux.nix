{ pkgs, ... }:

{
  programs.tmux = {
    enable = true;
    plugins = with pkgs; [
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.prefix-highlight
    ];
    keyMode = "vi";
    extraConfig = ''
      set-option -g default-terminal "screen-256color"
      set -ga terminal-overrides ",*256col*:Tc"
      source-file ./.palenight-tmux
      set-option -g prefix C-Space
      bind C-Space send-prefix
      bind s split-window -h
      bind v split-window -v
    '';
  };
}
