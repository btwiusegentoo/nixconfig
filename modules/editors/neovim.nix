{ pkgs }:
let
  unstable = pkgs.unstable;
  master = pkgs.master;
  plugins = pkgs.callPackage ./customvimplugins.nix { };
in
{
  enable = true;
  viAlias = true;
  vimAlias = true;
  package = unstable.neovim-unwrapped;
  configure = {
    # plugins {{{
    plug.plugins = with master.vimPlugins // plugins; [
      coc-nvim
      coc-json
      coc-css
      coc-snippets
      coc-clangd
      coc-prettier
      coc-html
      coc-lua
      coc-pairs
      palenight-vim
      nvim-colorizer-lua #norcalli/nvim-colorizer.lua
      indentLine
      vista-vim
      vim-sneak
      vim-fugitive
      gv-vim
      #vim-gitgutter
      nerdcommenter
      lightline-vim
      lightline-ale
      lightline-bufferline
      sky-color-clock-vim
      vim-devicons
      semshi
      vim-python-pep8-indent
      vim-fish
      vim-nix
      cosco-vim
      vim-cpp-modern
      indenthaskell
      vim-stylishask
      hlint-refactor-vim
      haskell-vim
      vim-tmux-navigator
      tmuxline
      quick-scope
      gv-vim
      tabular
      dashboard-nvim
      fzf-vim
      fzf-proj
      fzfWrapper
      vim-which-key
      indent-blankline
      vim-visual-multi
      lazygit-nvim
      vim-orgmode
      rust-vim
      coc-rust-analyzer
      vim-rooter
      nvim-utils
    ]; #}}}

    # plugins that don't work with vim-plug goes here.
    packages.myVimPackage = with pkgs.master.vimPlugins // plugins; {
      # loaded on launch
      start = [ ];
      # loaded manually with packadd
      opt = [ ];
    };

    customRC = import ../../configs/nvim.vim;
  };
}
