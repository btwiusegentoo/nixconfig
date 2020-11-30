{ pkgs, ... }:

{
  programs.tmux.enable = true;
  programs.tmux.plugins = with pkgs; [
    tmuxPlugins.vim-tmux-navigator
    tmuxPlugins.prefix-highlight
  ];
  programs.tmux.keyMode = "vi";
  programs.tmux.extraConfig = ''
      set-option -g default-terminal "screen-256color"
      set -ga terminal-overrides ",*256col*:Tc"
      source-file ./.palenight-tmux
      set-option -g prefix C-Space
      bind C-Space send-prefix
      bind s split-window -h
      bind v split-window -v
  '';
}
