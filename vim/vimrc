set nocompatible


""" Terminal specific settings

" Make vim update screen window title
if &term =~ "^screen"
    set t_ts=k
    set t_fs=\
endif

set background=dark
" Set up color settings and scheme based on terminal type
if $TERM =~ '^screen-bce' || $TERM =~ '^rxvt-256' || $TERM =~ '^xterm-256'
    set t_Co=256
    colo molokai
elseif $TERM =~ '^rxvt'
    set t_Co=88
    colo inkpot
elseif $TERM =~ '^linux'
    set t_Co=8
else
    set t_Co=16
endif


autocmd!

filetype on
filetype plugin on
filetype indent on
syntax on


" old titlestring
"set titlestring=%<%{hostname()}:%F\ %(%m\ %)[%l/%L\ %P]\ %y\ VIM
" new titlestring (short for screen window names)
"autocmd BufEnter * let &titlestring = hostname().expand(":%t")
autocmd BufEnter * let &titlestring = expand("%f")


""" Filetype specific options

" git commit diff viewing
autocmd BufRead,BufNewFile COMMIT_EDITMSG setf git
autocmd BufNewFile,BufRead *.erb setf eruby
" add html ft to php files for snippet support
autocmd BufRead,BufNewFile *.php set filetype=php.html
autocmd BufRead,BufNewFile *.snippet? set filetype=snippet
" python and makefiles need real tabs
autocmd FileType python noexpandtab
autocmd FileType make   noexpandtab
autocmd FileType snippet set softtabstop=8 noexpandtab


""" Filetype specific commands

" re-read vimrc after writing it
autocmd BufWritePost \.vimrc :source $HOME/.vimrc
autocmd BufWritePost vim/vimrc :source $HOME/.vimrc
" Useful when customizing xterm
autocmd BufWritePost \.Xdefaults :!xrdb ~/.Xdefaults
" PHP syntax check (CTRL-L)
autocmd FileType php noremap <C-L> :!/usr/bin/php -l %<CR>


" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif


" automatically give executable permissions if file begins with #! and contains
" '/bin/' in the path (not sure I want this to be automatic
"autocmd bufwritepost * if getline(1) =~ "^#!" | if getline(1) =~ "/bin/" | silent !chmod a+x <afile> | endif | endif



""" General options

set encoding=utf-8
set fileencodings=utf-8
let mapleader=","
set ruler
set pastetoggle=<F12>
set title
set laststatus=2
set visualbell t_vb=
set formatoptions+=tcaroqnw
set ttyfast
set history=50
set nowrap
set showcmd
set startofline
set hidden
set backspace=indent,eol,start
set timeoutlen=500
"set spell

set autoindent
set smartindent

set tabstop=8
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set smarttab

set incsearch
set hlsearch

set matchpairs+=<:>
set showmatch
set matchtime=2

set ignorecase
set smartcase

set scrolloff=3
set sidescroll=1
set sidescrolloff=2

set completeopt=longest,menuone
set wildmenu
set wildmode=longest,list

set statusline=%f\ %1*%m%r%*%h%w\ %{fugitive#statusline()}%=[%{&ff}\ %{strlen(&fenc)?&fenc:'none'}\ %{&ft}]\ [%LL\ %P\ %l,%v]
hi User1 term=inverse,bold cterm=inverse,bold ctermfg=red

" Put backup/swap files all in one place
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" show trailing whitespace
set list listchars=tab:\ \ ,trail:·

" highlight the current line (all the way to the right edge) and column
"autocmd WinEnter * setlocal cursorline
"autocmd WinLeave * setlocal nocursorline
"autocmd WinEnter * setlocal cursorcolumn
"autocmd WinLeave * setlocal nocursorcolumn
autocmd CursorMoved,CursorMovedI * call s:Cursor_Moved()
let g:last_pos = 0
" define highlighting colours for cursor line/column
hi CursorLine term=none cterm=none ctermbg=0 gui=none guibg=#B50DB9
hi CursorColumn term=none cterm=none ctermbg=0 gui=none guibg=#B50DB9


""" Indent options
let g:PHP_default_indenting = 1


""" Key mappings

" A few dvorak adjustments
noremap s l
noremap S L
noremap t j
noremap n k
noremap l n
noremap L N
noremap j J

" Window nav mappings
nmap <C-n> <C-W>W
nmap <C-t> <C-W>w
"map <C-S-T> <C-W>j<C-W>_     " move down one window and maximize
"map <C-S-N> <C-W>k<C-W>_     " move up one window and maximize
"map <C-H> <C-W>h           " move left one window
"map <C-L> <C-W>l           " move right one window
"map <C--> <C-W>-
"map <C-=> <C-W>+
"map <M-,> <C-W><
"map <M-.> <C-W>>

" toggle hlsearch
"map <Leader>h :set hls!<bar>set hls?<CR>

" Load vimrc in a split window and switch to it
map <Leader>V :sp ~/.vimrc<cr><C-W>w

" cd to the dir containing the current file
map <Leader>cd :lcd %:h<cr>

" Set up retabbing on a source file
nmap <Leader>rr :1,$retab<cr>

" In visual mode press * or # to search for the current selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" Searches for the current selection using Ack
vnoremap <silent> gf :call VisualSearch('gf')<CR>

" Quickfix window maps
map <Leader>qq :copen<cr>
map <Leader>qc :cclose<cr>
map <C-x> :cn<cr>
map <C-k> :cp<cr>



""" Plugin configs and keymaps

" Surround plugin
" Change visual surround mappings so s works for movement again
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>Vsurround

" NERDTree mappings and configuration
map <F10> :NERDTreeToggle<CR>
map <F11> :NERDTreeMirror<CR>
let g:NERDTreeMapOpenInTab="a"
let g:NERDTreeMapOpenInTabSilent="A"
let g:NERDTreeMapJumpFirstChild="N"
let g:NERDTreeMapJumpLastChild="T"
let g:NERDTreeMapJumpNextSibling="<C-S-T>"
let g:NERDTreeMapJumpPrevSibling="<C-S-N>"
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeAutoCenter=1
let g:NERDTreeAutoCenterThreshold=6

" SuperTab config
let g:SuperTabMappingForward = '<c-n>'
let g:SuperTabMappingBackward = '<c-p>'
let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabLongestHighlight = 1

" Lusty Juggler config
let g:LustyJugglerShowKeys = 'a'
let g:LustyJugglerAltTabMode = 1
let g:LustyJugglerSuppressRubyWarning = 1
nmap <Leader>b :LustyJuggler<cr>
nmap <C-p> :LustyJugglePrevious<cr>

" Lusty Explorer config
nmap <Leader>f :LustyFilesystemExplorer<cr>
nmap <Leader>h :LustyFilesystemExplorerFromHere<cr>
nmap <Leader>e :LustyBufferExplorer<cr>
nmap <Leader>gb :LustyBufferGrep<cr>
let g:LustyExplorerSuppressRubyWarning = 1

" Ack config
nmap <Leader>gf :Ack 

" fugitive (git) config
map <Leader>gs :Gstatus<cr>
map <Leader>gd :Gdiff<cr>
map <Leader>gg :Ggrep 
map <Leader>glg :Glog<cr>
map <Leader>gci :Gcommit<cr>
map <Leader>gmv :Gmove 
map <Leader>grm :Gremove

" Snipmate config
let g:snips_author = 'Nathan Howell'

" Jsbeautify config
nnoremap <silent> <leader>jb :call g:Jsbeautify()<cr>

" Yankring config
let g:yankring_enabled = 0
let g:yankring_replace_n_pkey = '<Leader>yp'
let g:yankring_replace_n_nkey = '<Leader>yn'

""" Taglist config
map <Leader>,t :TlistToggle<CR>
map <Leader>,a :TlistAddFiles
map <Leader>,r :TlistAddFilesRecursive .
let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Exit_OnlyWindow = 1
let Tlist_File_Fold_Auto_Close = 1
let Tlist_Compact_Format = 1
let Tlist_Use_Right_Window = 0
let Tlist_Inc_Winwidth = 0
let Tlist_Close_On_Select = 0
let Tlist_Process_File_Always = 1



""" Functions

" Check whether the cursor has moved to a new line and toggle
" cursorline highlighting (on if on a new line, off if not).
" TODO doesn't seem 100% reliable (eg. C-u, C-d don't toggle it)
" from: http://vim.wikia.com/wiki/Highlight_cursor_line_after_cursor_jump
function! s:Cursor_Moved()
    let cur_pos = winline()
    if g:last_pos == 0
        setlocal cursorline
        let g:last_pos = cur_pos
        return
    endif
    let diff = g:last_pos - cur_pos
    if diff >= 1 || diff <= -1
        setlocal cursorline
    else
        setlocal nocursorline
    endif
    let g:last_pos = cur_pos
endfunction

" A visual search mode function. Searches for the current selection
" forwards, backwards, or with Ack.
" from: http://amix.dk/vim/vimrc.html
function! VisualSearch(direction) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gf'
        execute Ack(l:pattern)
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction



""" Everything below this line is old/broken/unused or I just haven't looked it over yet

" enable a shortcut for tidy using ~/.tidyrc config
" map <Leader>T :!tidy -config ~/.tidyrc<cr><cr>

" Scrolling commands (I had forgotten all about these, so I probably won't
" miss them)
"nmap <C-o> <PageUp>
"nmap <C-e> <PageDown>
"map <C-o> <PageUp>
"map <C-e> <PageDown>
"imap <C-o> <PageUp>
"imap <C-e> <PageDown>
" since I'm using c-o elsewhere
"noremap <C-> :pop<cr>

" Tab mappings (delete soon?)
":nmap <C-n> :tabprevious<cr>
":nmap <C-t> :tabnext<cr>
":map <C-n> :tabprevious<cr>
":map <C-t> :tabnext<cr>
":nmap <C-N> :tabmove tabpagenr() - 2<cr>
":nmap <C-T> :tabmove tabpagenr() + 2<cr>
":map <C-N> :tabmove tabpagenr() - 2<cr>
":map <C-T> :tabmove tabpagenr() + 2<cr>
":imap <C-n> <ESC>:tabprevious<cr>i
":imap <C-t> <ESC>:tabnext<cr>i
":nmap <C-t> :tabnew<cr>
":imap <C-t> <ESC>:tabnew<cr> 
"nmap <tab> :bn<cr>
"nmap <s-tab> :bp<cr>

"if has("gui_running")
  "colo vilight
  "set guioptions=aAim    " don't want a toolbar or menu
  "set guifont=Liberation\ Mono\ 11
  "set mousehide
"endif

" keyword completion for perl
"set iskeyword+=:
" keyword completion for python (and ruby?)
"set iskeyword+=.

"let g:no_html_toolbar = 1
"let g:html_tag_case = 'lower'
"let g:html_template = '$HOME/.vim/html_template'
"let g:html_authorname = 'Nathan Howell'
"let g:html_authoremail = 'nath@nhowell.net'

"runtime ftplugin/man.vim
