

-- cosco.vim 
nvim.g.auto_comma_or_semicolon = 1            -- Enable auto comma/semicolon insertion mode
nvim.g.cosco_ignore_comment_lines = 1         -- Disable cosco.vim in comment lines
nvim.g.cosco_filetype_whitelist = {           -- Specify filetypes to enable cosco.vim
    "php";
    "javascript";
}

-- vim-sneak
nvim.g['sneak#label'] = 1                     -- Enable label-mode(it's like EasyMotion)

-- indentLine
nvim.g.indentLine_char = ""                  -- Specify a character to be used as indent line
nvim.g.indentLine_first_char = ""            -- Specify a character to be used as indent line on the first level
nvim.g.indentLine_showFirstIndentLevel = 1    -- Specify whether the first indent level should be shown
nvim.g.indentLine_color_gui = "#4E5579"       -- Specify indent line color
nvim.g.indentLine_fileTypeExclude = {         -- Specify a list of file types to disable indentLine
    "dashboard";
    "fzf";
    "coc-explorer";
}
nvim.g.indentLine_bufTypeExclude = {          -- Specify a list of buffer types to disable indentLine
    "help";
}

-- vim-rooter
nvim.g.rooter_cd_cmd = "lcd"                  -- Use lcd instead of cd (change current tab path)
nvim.g.rooter_silent_chdir = 1                -- Don't echo project directory
nvim.g.rooter_patterns = {                    -- Specify project root identifiers
    ".git";
    "Makefile";
    "*.sln";
    "build/env.sh";
}

-- Nix
nvim.g.nix_recommended_style = 0              -- use my shiftwidth in nix file because I prefer 4 spaces

-- Haskell
nvim.g.stylishask_on_save = 1                 -- Run stylish-haskell automatically when saving Haskell file
nvim.g.haskell_enable_quantification = 1      -- to enable highlighting of `forall`
nvim.g.haskell_enable_recursivedo = 1         -- to enable highlighting of `mdo` and `rec`
nvim.g.haskell_enable_arrowsyntax = 1         -- to enable highlighting of `proc`
nvim.g.haskell_enable_pattern_synonyms = 1    -- to enable highlighting of `pattern`
nvim.g.haskell_enable_typeroles = 1           -- to enable highlighting of type roles
nvim.g.haskell_enable_static_pointers = 1     -- to enable highlighting of `static`
nvim.g.haskell_backpack = 1                   -- to enable highlighting of backpack keywords
