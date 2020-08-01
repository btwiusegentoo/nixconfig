''
" get neovim version
let g:neovim_version = matchstr(execute('version'), 'NVIM v\zs[^\n]*')
set modelines=5
syntax on
filetype plugin indent on
" hybrid number
set nu rnu
" use marker fold. still better to write modeline everytime.
set foldmethod=marker
" autocompletion opacity
set pumblend=10
" floating windows(like fzf) opacity
set winblend=10
set wildmenu
set showtabline=2
set noshowmode
set cmdheight=2
set ignorecase
set smartcase
set wrapscan
set incsearch
set inccommand=split
"indent
set tabstop=4
set shiftwidth=4
set softtabstop=0
set expandtab
set smarttab
set shiftround
set breakindent
set breakindentopt=shift:1
set showbreak=⤿
set linebreak
set hidden
set nobackup
set nowritebackup
set updatetime=300
set shortmess+=c
set signcolumn=yes
" use 4 shiftwidth in nix file because I prefer
let g:nix_recommended_style=0

let g:auto_comma_or_semicolon = 1
let g:palenight_terminal_italics = 1
colorscheme palenight
set termguicolors
" custom colors
hi! Normal guibg=NONE
hi! SignColumn guibg=NONE
hi! LineNr guibg=NONE guifg=#a6accd
hi! CursorLineNr guifg=#82aaff
" remove annoying tilde(EndOfBuffer)
let &fcs='eob: '
lua <<EOF
require'colorizer'.setup()
EOF

"haskell{{{
let g:stylishask_on_save = 1
let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
"}}}

"cosco.vim
let g:cosco_ignore_comment_lines = 1
let g:cosco_filetype_whitelist = ['php', 'javascript']

" vim-sneak
let g:sneak#label = 1

" indentLine
let g:indentLine_char = ''
let g:indentLine_first_char = ''
let g:indentLine_showFirstIndentLevel = 1
let g:indentLine_setColors = 0
let g:indentLine_fileTypeExclude = [ 'dashboard', 'fzf', 'coc-explorer' ]
let g:indentLine_bufTypeExclude = [ 'help' ]

" dashboard{{{
"custom header

     
let g:dashboard_custom_header = [
\"",
\"               ╓╬╫╦                     ╞╫╥                                                                                                          ",
\"             ╓╬╫╫╫╫╫╥                   ╞╫╫╫╦                                                        ",
\"           ╔╫╫╫╫╫╫╫╫╫╦                  ╞╫╫╫╫╫╦              ",
\"         ╦╫╫╫╫╫╫╫╫╫╫╫╫╫┐                ╞╫╫╫╫╫╫╫╦                                                                                                              ╦╦╦╦╦╦",
\"       ╥ ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦               ╞╫╫╫╫╫╫╫╫╫╦╥                                                                                                          ╒╫╫╫╫╫╫",
\"     ╔╠╠╫  ╚╫╫╫╫╫╫╫╫╫╫╫╫╫╫              ╞╫╫╫╫╫╫╫╫╫╫╫╫                                                                                                          ╨╨╨╨╨╨",
\"     ╠╠╠╠╠╦ ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦            ╞╫╫╫╫╫╫╫╫╫╫╫╫",
\"     ╠╠╠╠╠╠╦  ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦           ╞╫╫╫╫╫╫╫╫╫╫╫╫           ╓╥     ╓╥╦╦╦╦╦╥               ╥╥╦╦╦╦╥╥               ╥╥╦╦╦╦╦╥╥      ║╫╫╫╫╫╦            ╫╫╫╫╫╩  ╫╫╫╫╫╫    ╫╫╫╫╫  ╓╦╫╫╫╫╫╫╫╦    ╥╦╫╫╫╫╫╫╫╫╦╥",
\"     ╠╠╠╠╠╠╠╠╦ ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╥         ╞╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╕ ╓╫╩╨╙   └╙╨╫╫╥        ╓╦╫╩╨└     ╙╨╫╦        ╓╦╫╩╨╙└  └╙╨╩╫╫╦    ╫╫╫╫╫╫╕          ╫╫╫╫╫╫  ╒╫╫╫╫╫╫    ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╫╫╫╫┐╦╫╫╫╫╫╫╫╫╫╫╫╫╫╫",
\"     ╠╠╠╠╠╠╠╠╠╦  ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦        ╞╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╦╫╨           ╫╫╦      ╫╫╨            ╙╫╥     ╫╫╩            ╙╫╫╦   ╫╫╫╫╫╫         ╫╫╫╫╫╫   ╒╫╫╫╫╫╫    ╫╫╫╫╫╫╫╨     ╙╫╫╫╫╫╫╫╫╨     ╨╫╫╫╫╫╫",
\"     ╠╠╠╠╠╠╠╠╠╠╠╥ ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╥      ╞╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫─    ╫╫─              ╙╫╕   ╫╫╩               ╫╫╦   ╫╫╫╫╫╫       ║╫╫╫╫╫    ╒╫╫╫╫╫╫    ╫╫╫╫╫╫        ╙╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╕  ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦     ╞╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫═   ╫╫╫                ╫╫  ║╫╫                ╙╫╫╕   ╫╫╫╫╫╦     ╔╫╫╫╫╫╛    ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡   ╚╫╫╫╫╫╫╫╫╫╫╫╫╫╫    ╞╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫═   ╫╫╩╩╩╩╩╩╩╩╩╩╩╩╩╩╩╩╩╩╨  ╫╫╛                 ╫╫╡   ╙╫╫╫╫╫╦   ╓╫╫╫╫╫╛     ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡    ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦  ╞╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫═   ╫╫╕                    ╫╫╕                 ╫╫╡    ╙╫╫╫╫╫╕  ╫╫╫╫╫╩      ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡      ╚╫╫╫╫╫╫╫╫╫╫╫╫╫╫  ╫╫╫╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫═   ║╫╫                    ╫╫╫                ╒╫╫╛     ╚╫╫╫╫╫ ╫╫╫╫╫╩       ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡       ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦ ╚╫╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫═    ╫╫╕                    ╫╫┐               ╫╫╫       ║╫╫╫╫╦╫╫╫╫╫        ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡         ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦  ╫╫╫╫╫╫╫╫╫           ║╫╡              ╫╫═     ╫╫╦                    ╫╫╥             ╫╫╩         ╫╫╫╫╫╫╫╫╫         ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡          ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╥ ╚╫╫╫╫╫╫╫           ║╫╡              ╫╫═      ╙╫╫╦╥       ╓╦╫╫╩      ╙╫╫╦╥       ╥╦╫╩└           ╫╫╫╫╫╫╫          ╒╫╫╫╫╫╫    ╫╫╫╫╫╫         ╫╫╫╫╫╫        ║╫╫╫╫╫╕",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡            ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦ ╙╫╫╫╫╫╫           ╚╩╛              ╩╩─         ╙╨╩╩╩╩╩╩╨╨╙            ╙╨╩╩╩╩╩╩╨╨└               ╙╙╙╙╙            ╨╨╨╨╨╨    ╨╨╨╨╨╨         ╨╨╨╨╨╨        ╙╨╨╨╨╨─",
\"     ╠╠╠╠╠╠╠╠╠╠╠╠╡             ╙╫╫╫╫╫╫╫╫╫╫╫╫╫╫╥ ╚╫╫╫╫",
\"     ╙╬╠╠╠╠╠╠╠╠╠╠╡               ╫╫╫╫╫╫╫╫╫╫╫╫╫╫╦ ╙╫╫╩",
\"        ╩╠╠╠╠╠╠╠╠╡                ╚╫╫╫╫╫╫╫╫╫╫╫╫╫╫",
\"          ╩╠╠╠╠╠╠╡                 ╙╫╫╫╫╫╫╫╫╫╫╫╫╨",
\"            ╙╣╠╠╠╡                   ╨╫╫╫╫╫╫╫╩╨                              ",
\"              ╙╬╠╡                    ╙╫╫╫╫╩                                                                                 ",
\"                ╙╡                      ╩╩                                                                                                                                   ",
\"",
\"                                                                                                                                                                                             version: ". g:neovim_version ."",
\"",
\"",
\ ]
"headercolor
hi! dashboardHeader guifg=#c3e88d
hi! dashboardCenter guifg=#c792ea
hi! dashboardShortcut guifg=#89ddff
hi! dashboardFooter guifg=#676E95
"use fzf in dashboard
let g:dashboard_default_executive = "fzf"

let g:dashboard_custom_section = {
\ 'last_session'         :['  Reload last session                   SPC s l'],
\ 'find_history'         :['  Recently opened files                 SPC f h'],
\ 'find_word'            :['  Find  word                            SPC f d'],
\ 'find_file'            :['  Find  File                            SPC f f'],
\ }
"}}}

" dashboard functions {{{
function! FIND_HISTORY()
    History
endfunction

function! FIND_FILE()
    Files
endfunction

function! FIND_WORD()
    Rg
endfunction

function! LAST_SESSION()
    SessionLoad
endfunction
" }}}

" whichkey{{{
set timeoutlen=150 
call which_key#register('<Space>', "g:which_key_map_space")
let g:which_key_map_space = {} " Define dictionary.
let g:which_key_map_space.F = 'Format file with coc'
let g:which_key_map_space.w = 'Save current file'
let g:which_key_map_space.T = 'Open new buffer'
let g:which_key_map_space.n = 'Open Vista(tagbar)'
let g:which_key_map_space.y = 'Yank selection to clipboard'
let g:which_key_map_space.p = 'Paste from clipboard'
let g:which_key_map_space.P = 'Same as p but puts text before the cursor'
let g:which_key_map_space.yy = 'Yank entire line'
let g:which_key_map_space.Y = 'Same as yy'
let g:which_key_map_space.t = 'Open terminal in new buffer'

let g:which_key_map_space.f = {
        \ 'name' : '+FZF',
        \ 'f' : 'Files',
        \ 'h' : 'History',
        \ 'l' : 'Lines',
        \ 'd' : 'Word',
        \ }

let g:which_key_map_space.b = {
        \ 'name' : '+Buffers',
        \ 'q' : 'Close current buffer',
        \ 'b' : 'FZF Buffers',
        \ }

let g:which_key_map_space.s = {
        \ 'name' : '+Session',
        \ 's' : 'Save current session',
        \ 'l' : 'Load last session',
        \ }

let g:which_key_map_space.c = {
        \ 'name' : '+NERDCommenter',
        \ 'c' : 'Comment out the current line or selected text',
        \ 'n' : 'Same as c but forces nesting',
        \ 'SPC' : 'Toggles the comment state of the selected line(s)',
        \ 'm': 'Comments the given lines using only one set of multipart delimiters',
        \ 'i': 'Toggles the comment state of the selected line(s) individually',
        \ 's': 'Comments out the selected lines with a pretty block formatted layout',
        \ 'y': 'Same as c except that the commented line(s) are yanked first',
        \ '$': 'Comments the current line from the cursor to the end of line',
        \ 'A': 'Adds comment delimiters to the end of line and goes into insert mode between them',
        \ 'a': 'Switches to the alternative set of delimiters',
        \ 'u': 'Uncomments the selected line(s)',
        \ 'l': 'Same as c except that the delimiters are aligned down the left side',
        \ 'b': 'Same as c except that the delimiters are aligned down the both sides',
        \ }
"}}}

" fzf {{{
"match colorscheme
let g:fzf_commits_log_options = '--graph --color=always'
let g:fzf_tags_command = 'ctags -R'
" use fd
let $FZF_DEFAULT_COMMAND = 'fd --type f'
" set fzf options
let $FZF_DEFAULT_OPTS = '--reverse --color=bg+:#292D3E,bg:#292D3E,spinner:#89DDFF,hl:#82AAFF,fg:#8796B0,header:#82AAFF,info:#FFCB6B,pointer:#89DDFF,marker:#89DDFF,fg+:#959DCB,prompt:#c792ea,hl+:#82AAFF'
" use float window
let g:fzf_layout = { 'window': 'call FZF_float_window()' }
function! FZF_float_window()
    let width = min([&columns - 4, max([80, &columns - 20])])
    let height = min([&lines - 10, max([20, &lines - 20])])
    let row = (&lines - height) / 2
    let col = (&columns - width) / 2
    let opts = {'relative': 'editor', 'row': row, 'col': col, 'width': width, 'height': height, 'style': 'minimal'}

    let top = "╭" . repeat("─", width - 2) . "╮"
    let mid = "│" . repeat(" ", width - 2) . "│"
    let bot = "╰" . repeat("─", width - 2) . "╯"
    let lines = [top] + repeat([mid], height - 2) + [bot]
    let s:buf = nvim_create_buf(v:false, v:true)
    call nvim_buf_set_lines(s:buf, 0, -1, v:true, lines)
    call nvim_open_win(s:buf, v:true, opts)
    hi! FloatWinBorder guifg=#34324a
    set winhl=Normal:FloatWinBorder
    call nvim_open_win(nvim_create_buf(v:false, v:true), v:true, opts)
    au BufWipeout <buffer> exe 'bw '.s:buf
endfunction
" }}}

"lightline{{{
let g:lightline = {
\ 'colorscheme': 'palenight',
\ 'active': {
\   'left': [ [ 'mode', 'paste' ],
\             [ 'cocstatus', 'currentfunction', 'gitbranch', 'readonly', 'filename', 'modified' ] ]
\ },
\ 'component_function': {
\   'gitbranch': 'fugitive#head',
\   'cocstatus': 'coc#status',
\   'currentfunction': 'CocCurrentFunction',
\   'filetype': 'MyFiletype',
\   'fileformat': 'MyFileformat',
\ },
\ 'tabline': {
\   'left': [ ['buffers'] ],
\   'right': [ ['close'] ]
\ },
\ 'component_expand': {
\   'buffers': 'lightline#bufferline#buffers'
\ },
\ 'component_type': {
\   'buffers': 'tabsel'
\ },
\ }

let g:lightline#bufferline#enable_devicons   = 1
let g:lightline#bufferline#unicode_symbols   = 1
let g:lightline#bufferline#show_number       = 1
let g:lightline#bufferline#filename_modifier = ':t'
let s:palette = g:lightline#colorscheme#palenight#palette
let s:palette.tabline.tabsel = [ [ '#292D3E', '#82b1ff', 'NONE', 'NONE' ] ]
let s:palette.tabline.left  = [ [ '#bfc7d5', '#3E4452', 'NONE', 'NONE'] ]
let s:palette.tabline.right  = [ [ '#292d3e', '#82b1ff', 'NONE', 'NONE' ] ]
unlet s:palette

function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype . ' ' . WebDevIconsGetFileTypeSymbol() : 'no ft') : ""
endfunction

function! MyFileformat()
    return winwidth(0) > 70 ? (&fileformat . ' ' . WebDevIconsGetFileFormatSymbol()) : ""
endfunction
"}}}

" airline
let g:airline_theme = 'palenight'
let g:airline#extensions#tabline#enabled = 1

"Keybinds{{{
" set leader and localleader
let g:mapleader = "\<Space>"
let g:maplocalleader = ","
" which-key
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
vnoremap <silent> <localleader> :<c-u>WhichKeyVisual ','<CR>

" save(spc w)
nnoremap <Leader>w :w<CR>
"move window with ctrlhjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
" open vista(right sidebar)
nmap <silent> <Leader>n :<C-u>Vista!!<CR>
" coc-explorer
map <C-n> :CocCommand explorer<CR>
"move lines
nnoremap <A-j> :m .+1<CR>==
nnoremap <A-k> :m .-2<CR>==
inoremap <A-j> <Esc>:m .+1<CR>==gi
inoremap <A-k> <Esc>:m .-2<CR>==gi
vnoremap <A-j> :m '>+1<CR>gv=gv
vnoremap <A-k> :m '<-2<CR>gv=gv
"copy to clipboard
vnoremap <leader>y "+y
nnoremap <leader>Y "+yg_
nnoremap <leader>y "+y
nnoremap <leader>yy "+yy
"paste from clipboard
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P
"move buffers with Shift JK like qutebrowser
nnoremap <S-j> :bnext<CR>
nnoremap <S-k> :bprevious<CR>

" buffers
" new buffer
nnoremap <leader>T :enew<cr>
" Close the current buffer and move to the previous one
nnoremap <leader>bq :bp <BAR> bd #<CR>
" search buffers
nnoremap <silent> <leader>bb :Buffers<CR>

" fzf
nnoremap <silent> <Leader>fh :History<CR>
nnoremap <silent> <Leader>ff :Files<CR>
nnoremap <silent> <Leader>fl :Lines<CR>
nnoremap <silent> <Leader>fd :Rg<CR> 

" close fzf with esc
autocmd! FileType fzf tnoremap <buffer> <esc> <c-c>

" dashboard-nvim
nmap <Leader>ss :<C-u>SessionSave<CR>
nmap <Leader>sl :<C-u>SessionLoad<CR>

" coc
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
nnoremap <leader><S-f>  :call CocAction('format')<CR>
nnoremap <C-K> :call <SID>show_documentation()<CR>

" terminal
tnoremap <Esc> <C-\><C-n>

" open terminal
nnoremap <leader>t :terminal<CR>

"}}}

"Coc{{{
let g:coc_global_extensions = ['coc-python', 'coc-syntax', 'coc-emoji', 'coc-discord-neovim', 'coc-explorer']

function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
    else
        call CocAction('doHover')
    endif
endfunction
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
    autocmd!
    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

command! -nargs=0 Format :call CocAction('format')
command! -nargs=? Fold :call CocAction('fold', <f-args>)
command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')
inoremap <silent><expr> <TAB>
    \ pumvisible() ? "\<C-n>" :
    \ <SID>check_back_space() ? "\<TAB>" :
    \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col -1] =~# '\s'
endfunction
"}}}

" autocommands/functions

" make sure neovim restores cursor shape when exit to console.
augroup RestoreCursorShapeOnExit
    autocmd!
    autocmd VimLeave * set guicursor=a:ver25-blinkon0
augroup END
" disable indentline in terminal
autocmd TermOpen * IndentLinesDisable
" disable tabline inside dashboard
autocmd FileType dashboard set showtabline=0 | autocmd WinLeave <buffer> set showtabline=2

" vim: fdm=marker sw=4 ft=vim:
''
