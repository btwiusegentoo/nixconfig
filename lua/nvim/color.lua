require 'nvim_utils'

-- Set true color
nvim.o.termguicolors = true
-- Set colorscheme
nvim.command('colorscheme palenight')
-- Set palenight theme italics
nvim.g.palenight_terminal_italics = 1
-- Set custom colors
nvim.command('hi! Normal guibg=NONE')
nvim.command('hi! SignColumn guibg=NONE')
nvim.command('hi! MatchParen guifg=#F07178 guibg=#202331')
nvim.command('hi! LineNr guibg=NONE guifg=#a6accd')
nvim.command('hi! CursorLineNr guifg=#82aaff')
nvim.command('hi! NormalFloat guifg=#A6ACCD guibg=#292D3E')
nvim.command('hi! VertSplit guifg=#4E5579')
