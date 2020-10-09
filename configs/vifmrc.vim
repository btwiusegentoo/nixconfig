" vim: filetype=vim :

" Command used to edit files in various contexts.  The default is vim.
" If you would like to use another vi clone such as Elvis or Vile
" you will need to change this setting.

set vicmd=nvim

" This makes vifm perform file operations on its own instead of relying on
" standard utilities like `cp`.  While using `cp` and alike is a more universal
" solution, it's also much slower when processing large amounts of files and
" doesn't support progress measuring.

set syscalls

" Trash Directory
" The default is to move files that are deleted with dd or :d to
" the trash directory.  If you change this you will not be able to move
" files by deleting them and then using p to put the file in the new location.
" I recommend not changing this until you are familiar with vifm.
" This probably shouldn't be an option.

set trash

" This is how many directories to store in the directory history.

set history=100

" Automatically resolve symbolic links on l or Enter.

set nofollowlinks

" With this option turned on you can run partially entered commands with
" unambiguous beginning using :! (e.g. :!Te instead of :!Terminal or :!Te<tab>).

" set fastrun

" Natural sort of (version) numbers within text.

set sortnumbers

" Maximum number of changes that can be undone.

set undolevels=100

" Use Vim's format of help file (has highlighting and "hyperlinks").
" If you would rather use a plain text help file set novimhelp.

set vimhelp

" If you would like to run an executable file when you
" press return on the file name set this.

set norunexec

" Format for displaying time in file list. For example:
" TIME_STAMP_FORMAT=%m/%d-%H:%M
" See man date or man strftime for details.

set timefmt=%m/%d\ %H:%M

" Show list of matches on tab completion in command-line mode

set wildmenu

" Display completions in a form of popup with descriptions of the matches

set wildstyle=popup

" Display suggestions in normal, visual and view modes for keys, marks and
" registers (at most 5 files).  In other view, when available.

set suggestoptions=normal,visual,view,otherpane,keys,marks,registers

" Ignore case in search patterns unless it contains at least one uppercase
" letter

set ignorecase
set smartcase

" Don't highlight search results automatically

set nohlsearch

" Use increment searching (search while typing)
set incsearch

" Try to leave some space from cursor to upper/lower border in lists

set scrolloff=4

" Set custom status line look

"set statusline="  Hint: %z%= %A %10u:%-7g %15s %20d  "

" ------------------------------------------------------------------------------

" :mark mark /full/directory/path [filename]

mark b ~/bin/
mark h ~/

" ------------------------------------------------------------------------------

" :com[mand][!] command_name action
" The following macros can be used in a command
" %a is replaced with the user arguments.
" %c the current file under the cursor.
" %C the current file under the cursor in the other directory.
" %f the current selected file, or files.
" %F the current selected file, or files in the other directory.
" %b same as %f %F.
" %d the current directory name.
" %D the other window directory name.
" %m run the command in a menu window

command! df df -h %m 2> /dev/null
command! diff vim -d %f %F
command! zip zip -r %f.zip %f
command! run !! ./%f
command! make !!make %a
command! mkcd :mkdir %a | cd %a
command! vgrep vim "+grep %a"
command! reload :write | restart

" ------------------------------------------------------------------------------

" The file type is for the default programs to be used with
" a file extension.
" :filetype pattern1,pattern2 defaultprogram,program2
" :fileviewer pattern1,pattern2 consoleviewer
" The other programs for the file type can be accessed with the :file command
" The command macros %f, %F, %d, %F may be used in the commands.
" The %a macro is ignored.  To use a % you must put %%.

" For automated FUSE mounts, you must register an extension with :file[x]type
" in one of following formats:
"
" :filetype extensions FUSE_MOUNT|some_mount_command using %SOURCE_FILE and %DESTINATION_DIR variables
" %SOURCE_FILE and %DESTINATION_DIR are filled in by vifm at runtime.
" A sample line might look like this:
" :filetype *.zip,*.jar,*.war,*.ear FUSE_MOUNT|fuse-zip %SOURCE_FILE %DESTINATION_DIR
"
" :filetype extensions FUSE_MOUNT2|some_mount_command using %PARAM and %DESTINATION_DIR variables
" %PARAM and %DESTINATION_DIR are filled in by vifm at runtime.
" A sample line might look like this:
" :filetype *.ssh FUSE_MOUNT2|sshfs %PARAM %DESTINATION_DIR
" %PARAM value is filled from the first line of file (whole line).
" Example first line for SshMount filetype: root@127.0.0.1:/
"
" You can also add %CLEAR if you want to clear screen before running FUSE
" program.

" File associations{{{
" Pdf
filextype *.pdf zathura %c %i &

" PostScript
filextype *.ps,*.eps,*.ps.gz zathura %f &

" Djvu
filextype *.djvu zathura %f &

" Audio
filetype *.wav,*.mp3,*.flac,*.m4a,*.wma,*.ape,*.ac3,*.og[agx],*.spx,*.opus mpv %f

" Video
filextype *.avi,*.mp4,*.wmv,*.dat,*.3gp,*.ogv,*.mkv,*.mpg,*.mpeg,*.vob,
         \*.fl[icv],*.m2v,*.mov,*.webm,*.ts,*.mts,*.m4v,*.r[am],*.qt,*.divx,
         \*.as[fx]
         \ mpv %f &

" Web
filextype *.html,*.htm
        \ {Open with qutebrowser}
        \ qutebrowser %f %i &,
        \ {Open with firefox}
        \ firefox %f &,
filetype *.html,*.htm links, lynx

" Object
filetype *.o nm %f | less

" Man page
filetype *.[1-8] man ./%c
fileviewer *.[1-8] man ./%c | col -b

" Images
filextype *.bmp,*.jpg,*.jpeg,*.png,*.gif,*.xpm
        \ sxiv %f

" MD5
filetype *.md5
       \ {Check MD5 hash sum}
       \ md5sum -c %f %S,

" SHA1
filetype *.sha1
       \ {Check SHA1 hash sum}
       \ sha1sum -c %f %S,

" SHA256
filetype *.sha256
       \ {Check SHA256 hash sum}
       \ sha256sum -c %f %S,

" SHA512
filetype *.sha512
       \ {Check SHA512 hash sum}
       \ sha512sum -c %f %S,

" GPG signature
filetype *.asc
       \ {Check signature}
       \ !!gpg --verify %c,

" Torrent
filetype *.torrent ktorrent %f &
fileviewer *.torrent dumptorrent -v %c

" FuseZipMount
filetype *.zip,*.jar,*.war,*.ear,*.oxt,*.apkg
       \ {Mount with fuse-zip}
       \ FUSE_MOUNT|fuse-zip %SOURCE_FILE %DESTINATION_DIR,
       \ {View contents}
       \ zip -sf %c | less,
       \ {Extract here}
       \ tar -xf %c,
fileviewer *.zip,*.jar,*.war,*.ear,*.oxt zip -sf %c

" ArchiveMount
filetype *.tar,*.tar.bz2,*.tbz2,*.tgz,*.tar.gz,*.tar.xz,*.txz
       \ {Mount with archivemount}
       \ FUSE_MOUNT|archivemount %SOURCE_FILE %DESTINATION_DIR,
fileviewer *.tgz,*.tar.gz tar -tzf %c
fileviewer *.tar.bz2,*.tbz2 tar -tjf %c
fileviewer *.tar.txz,*.txz xz --list %c
fileviewer *.tar tar -tf %c

" Rar2FsMount and rar archives
filetype *.rar
       \ {Mount with rar2fs}
       \ FUSE_MOUNT|rar2fs %SOURCE_FILE %DESTINATION_DIR,
fileviewer *.rar unrar v %c

" IsoMount
filetype *.iso
       \ {Mount with fuseiso}
       \ FUSE_MOUNT|fuseiso %SOURCE_FILE %DESTINATION_DIR,

" SshMount
filetype *.ssh
       \ {Mount with sshfs}
       \ FUSE_MOUNT2|sshfs %PARAM %DESTINATION_DIR %FOREGROUND,

" FtpMount
filetype *.ftp
       \ {Mount with curlftpfs}
       \ FUSE_MOUNT2|curlftpfs -o ftp_port=-,,disable_eprt %PARAM %DESTINATION_DIR %FOREGROUND,

" Fuse7z and 7z archives
filetype *.7z
       \ {Mount with fuse-7z}
       \ FUSE_MOUNT|fuse-7z %SOURCE_FILE %DESTINATION_DIR,
fileviewer *.7z 7z l %c

" Office files
filextype *.odt,*.doc,*.docx,*.xls,*.xlsx,*.odp,*.pptx libreoffice %f &
fileviewer *.doc catdoc %c
fileviewer *.docx docx2txt.pl %f -

" TuDu files
filetype *.tudu tudu -f %c

" Qt projects
filextype *.pro qtcreator %f &

" Directories
filextype */
        \ {View in thunar}
        \ Thunar %f &,
"}}}

" Ueberzug{{{
" Ueberzug file preview. requires vifmimg
" Don't forget to install in $PATH
fileviewer *.pdf
    \ vifmimg pdfpreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear

fileviewer *.epub
    \ vifmimg epubpreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear

fileviewer *.avi,*.mp4,*.wmv,*.dat,*.3gp,*.ogv,*.mkv,*.mpg,*.mpeg,*.vob,
    \*.fl[icv],*.m2v,*.mov,*.webm,*.ts,*.mts,*.m4v,*.r[am],*.qt,*.divx,
    \ vifmimg videopreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear

fileviewer *.bmp,*.jpg,*.jpeg,*.png,*.xpm
    \ vifmimg draw %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear

fileviewer *.gif
    \ vifmimg gifpreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear

fileviewer *.ico
    \ vifmimg magickpreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear
    
fileviewer <audio/*>
    \ vifmimg audiopreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear
    
fileviewer <font/*>
    \ vifmimg fontpreview %px %py %pw %ph %c
    \ %pc
    \ vifmimg clear

" vifmimg pdf scroll
map > :!vifmimg inc<CR>
map < :!vifmimg dec<CR>
"}}}

" Syntax highlighting in preview
"
" Explicitly set highlight type for some extensions
"
" 256-color terminal
fileviewer *.[ch],*.[ch]pp highlight -O xterm256 -s dante --syntax c %c
fileviewer Makefile,Makefile.* highlight -O xterm256 -s dante --syntax make %c
"
" Open all other files with default system programs (you can also remove all
" :file[x]type commands above to ensure they don't interfere with system-wide
" settings).  By default all unknown files are opened with 'vi[x]cmd'
" uncommenting one of lines below will result in ignoring 'vi[x]cmd' option
" for unknown file types.
" For *nix:
filetype * xdg-open
" For OS X:
" filetype * open
" For Windows:
" filetype * start, explorer

" ------------------------------------------------------------------------------

" What should be saved automatically between vifm sessions.  Drop "savedirs"
" value if you don't want vifm to remember last visited directories for you.
set vifminfo=dhistory,savedirs,chistory,state,tui,shistory,
    \phistory,fhistory,dirstack,registers,bookmarks,bmarks

" ------------------------------------------------------------------------------

" Examples of configuring both panels

" Customize view columns a bit (enable ellipsis for truncated file names)
"
" set viewcolumns=-{name}..,6{}.

" Filter-out build and temporary files
"
" filter! /^.*\.(lo|o|d|class|py[co])$|.*~$/

" ------------------------------------------------------------------------------

" Sample mappings

" Start shell in current directory
nnoremap s :shell<cr>

" Display sorting dialog
nnoremap S :sort<cr>

" Toggle visibility of preview window
nnoremap w :view<cr>
vnoremap w :view<cr>gv

" Open file in the background using its default program
nnoremap gb :file &<cr>l

" Interaction with system clipboard
if has('win')
    " Yank current directory path to Windows clipboard with forward slashes
    nnoremap yp :!echo %"d:gs!\!/! %i | clip<cr>
    " Yank path to current file to Windows clipboard with forward slashes
    nnoremap yf :!echo %"c:gs!\!/! %i | clip<cr>
elseif executable('xclip')
    " Yank current directory path into the clipboard
    nnoremap yd :!echo %d | xclip %i<cr>
    " Yank current file path into the clipboard
    nnoremap yf :!echo %c:p | xclip %i<cr>
elseif executable('xsel')
    " Yank current directory path into primary and selection clipboards
    nnoremap yd :!echo -n %d | xsel --input --primary %i &&
                \ echo -n %d | xsel --clipboard --input %i<cr>
    " Yank current file path into into primary and selection clipboards
    nnoremap yf :!echo -n %c:p | xsel --input --primary %i &&
                \ echo -n %c:p | xsel --clipboard --input %i<cr>
endif

" Mappings for faster renaming
nnoremap I cw<c-a>
nnoremap cc cw<c-u>
nnoremap A cw

" Open console in current directory
nnoremap ,t :!alacritty &<cr>

" Open editor to edit vifmrc and apply settings after returning to vifm
nnoremap ,c :write | edit $MYVIFMRC | restart<cr>

" Toggle wrap setting on ,w key
nnoremap ,w :set wrap!<cr>

" Edit file in neovim
nmap o :write | edit %f | restart<cr>

" Example of standard two-panel file managers mappings
nnoremap <f3> :!less %f<cr>
nnoremap <f4> :edit<cr>
nnoremap <f5> :copy<cr>
nnoremap <f6> :move<cr>
nnoremap <f7> :mkdir<space>
nnoremap <f8> :delete<cr>


" Use ripgrep instead of grep
"
set grepprg='rg --line-numbers %i %a %s'


" colorscheme
" Write here because seems like symlink doesn't work.
" palenight color scheme for vifm

" Reset all styles first
highlight clear

highlight Border	cterm=none	ctermfg=default	ctermbg=default

highlight TopLine	cterm=none	ctermfg=002	ctermbg=default
highlight TopLineSel	cterm=bold	ctermfg=002	ctermbg=default

highlight Win		cterm=none	ctermfg=251	ctermbg=default
highlight Directory	cterm=bold	ctermfg=004	ctermbg=default
highlight CurrLine	cterm=bold,inverse	ctermfg=default	ctermbg=default
highlight OtherLine	cterm=bold	ctermfg=default	ctermbg=default
highlight Selected	cterm=none	ctermfg=003	ctermbg=008

highlight JobLine	cterm=bold	ctermfg=251	ctermbg=008
highlight StatusLine	cterm=none	ctermfg=008	ctermbg=default
highlight ErrorMsg	cterm=bold	ctermfg=001	ctermbg=default
highlight WildMenu	cterm=bold	ctermfg=015	ctermbg=008
highlight CmdLine	cterm=none	ctermfg=007	ctermbg=default

highlight Executable	cterm=bold	ctermfg=002	ctermbg=default
highlight Link		cterm=bold	ctermfg=006	ctermbg=default
highlight BrokenLink	cterm=bold	ctermfg=001	ctermbg=default
highlight Device	cterm=bold,standout	ctermfg=000	ctermbg=011
highlight Fifo		cterm=none	ctermfg=003	ctermbg=default
highlight Socket	cterm=bold	ctermfg=005	ctermbg=default

