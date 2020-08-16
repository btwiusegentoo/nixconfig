require 'nvim_utils'

local commonOptions = {
    -- Hybrid number
    number = true;
    relativenumber = true;
    -- Set folding to marker
    foldmethod = "marker";
    -- Set autocompletion opacity
    pumblend = 10;
    -- Set floating windows opacity(make completely opaque)
    winblend = 0;
    --Use autocompletion for commands.
    wildmenu = true;
    -- Indent
    tabstop = 4;             -- use four spaces for tabs
    shiftwidth = 4;          -- use four spaces for indent when shifting(>>)
    expandtab = true;        -- Convert tabs to space
    smarttab = true;         -- Insert tabstop number when <tab> is pressed.
    shiftround = true;       -- When shifting line(>>), set indentation to the nearest multiple of "shiftwidth"
    -- Search
    ignorecase = true;       -- Do case "in"sensitive search
    smartcase = true;        -- Case sensitive search when caps is used
    wrapscan = true;         -- When search reachs end of file, wrap around to beginning
    incsearch = true;        -- Highlight matches while searching
    inccommand = "split";    -- Preview replace results

    modelines = 2;           -- Set modeline lines number (# vim: sw=4)<-this
    showtabline = 2;         -- Show top tabbar
    showmode = false;        -- Don't show mode below. lightline already shows.
}

for name, value in pairs(commonOptions) do
    nvim.o[name] = value
end
