local commonOptions = {
    hidden = true;             -- Hide buffer instead of closing when opening file
    modelines = 2;             -- Set modeline lines number (# vim: sw=4)<-this
    showtabline = 2;           -- Show top tabbar(In this case, I use bufferbar)
    showmode = false;          -- Don't show mode below. lightline already shows.
    signcolumn = "yes";        -- Show signcolumn all time.
    foldmethod = "marker";     -- Set folding to use marker
    pumblend = 10;             -- Set autocompletion opacity
    winblend = 0;              -- Set floating windows opacity(completely opacity here)
    shortmess = "filnxtToOFc"; -- Default + c (Disables annoying completion messages)
    updatetime = 100;          -- Set delay for |CursorHold| autocommand event.(used for plugins) this also affects swaps.

    -- Hybrid number
    number = true;
    relativenumber = true;
    -- Indent
    tabstop = 4;                -- use four spaces for tabs
    shiftwidth = 4;             -- use four spaces for indent when shifting(>>)
    expandtab = true;           -- Convert tabs to space
    shiftround = true;          -- When shifting line(>>), set indentation to the nearest multiple of "shiftwidth"
    -- Wrap
    breakindent = true;         -- Wrap text with indentation instead of going to beginning of line.
    breakindentopt = "shift:1"; -- Use 1 indentation when wrapping text
    showbreak = "⤿";            -- Use this character to show wrapped lines
    linebreak = true;           -- Wrap line without breaking words
    -- Search
    ignorecase = true;          -- Do case "in"sensitive search
    smartcase = true;           -- Case sensitive search when caps is used
    wrapscan = true;            -- When search reachs end of file, wrap around to beginning
    inccommand = "split";       -- Preview replace results
    -- Backups
    backup = false;             -- Disable normal file backup
    writebackup = false;        -- Disable backup remains until successfully written file
}
-- Removed options not need anymore in neovim. see https://neovim.io/doc/user/vim_diff.html

for name, value in pairs(commonOptions) do
    nvim.o[name] = value
end
