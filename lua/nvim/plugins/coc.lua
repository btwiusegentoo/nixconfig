-- coc extensions that don't work installing with Nix goes here
nvim.g.coc_global_extensions = {
    'coc-python',
    'coc-syntax',
    'coc-emoji',
    'coc-discord-neovim',
    'coc-explorer'
}

nvim.exec([[
    command! -nargs=0 Format :call CocAction('format')
    command! -nargs=? Fold :call CocAction('fold', <f-args>)
    command! -nargs=0 OR :call CocAction('runCommand', 'editor.action.organizeImport')
    function! CheckBackSpace() abort
      let col = col('.') - 1
      return !col || getline('.')[col - 1]  =~# '\s'
    endfunction
    inoremap <silent><expr> <TAB> pumvisible() ? "\<C-n>" : CheckBackSpace() ? "\<TAB>" :coc#refresh()
    inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
]], '')


local autocmds = {
    coc = {
        {"FileType typescript,json setl farmatexpor=CocAction('formatSelected')"};
        {"User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')"};
        {"CursorHold * silent call CocActionAsync('highlight')"};
    };
}

nvim_create_augroups(autocmds)
