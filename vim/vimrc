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

filetype plugin indent on
syntax on

runtime! ftplugin/man.vim

" old titlestring
"set titlestring=%<%{hostname()}:%F\ %(%m\ %)[%l/%L\ %P]\ %y\ VIM
" new titlestring (short for screen window names)
"autocmd BufEnter * let &titlestring = hostname().expand(":%t")
autocmd BufEnter * let &titlestring = expand("%f")

" No wrapping for the quickfix window
autocmd BufReadPost quickfix setlocal nowrap

""" Filetype specific options

" add proper filetype to files that need it
autocmd BufRead,BufNewFile COMMIT_EDITMSG setf git
autocmd BufNewFile,BufRead *.erb setf eruby
autocmd BufRead,BufNewFile *.snippet? set filetype=snippet
" some files need real tabs
autocmd FileType python set noexpandtab
autocmd FileType make set noexpandtab
autocmd FileType snippet set noexpandtab
" some files should have different tabsizes and other options
autocmd FileType html set ts=2 sts=2 sw=2
autocmd FileType javascript set ts=4 sts=4 sw=4
autocmd FileType snippet set sts=8 sw=8 noet
autocmd FileType haskell set ts=2 sts=2 sw=2

" highlight error logging functions
hi ErrorLogFunction term=inverse,bold cterm=inverse,bold ctermfg=red

" Change some highlight colours
hi Search term=bold cterm=bold ctermfg=black ctermbg=green
hi IncSearch term=bold cterm=bold ctermfg=yellow ctermbg=red
hi Todo term=bold cterm=bold ctermfg=black ctermbg=blue

" Use my own diff highlighting regardless of colour scheme
hi DiffAdd term=none cterm=none ctermfg=black ctermbg=green
hi DiffChange term=none cterm=none ctermfg=black ctermbg=blue
hi DiffDelete term=none cterm=none ctermfg=black ctermbg=red
hi DiffText term=bold cterm=bold ctermfg=black ctermbg=yellow

" custom popup menu colouring
hi Pmenu term=none cterm=none ctermfg=gray ctermbg=black
hi PmenuSel term=bold cterm=bold ctermfg=black ctermbg=green


""" Filetype specific commands

" re-read vimrc after writing it
autocmd BufWritePost \.vimrc :source $HOME/.vimrc
autocmd BufWritePost */vim/vimrc :source $HOME/.vimrc
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
set formatoptions+=tcroqnw
set ttyfast
set history=50
set wrap
set linebreak
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
autocmd CursorMoved,CursorMovedI * call s:Cursor_Moved()
let g:last_pos = 0
" define highlighting colours for cursor line/column
hi CursorLine term=none cterm=none ctermbg=0 gui=none guibg=#B50DB9
hi CursorColumn term=none cterm=none ctermbg=0 gui=none guibg=#B50DB9


""" Indent options
let g:PHP_default_indenting = 0


""" Key mappings

" A few dvorak adjustments
noremap s l
noremap S L
noremap t j
noremap n k
noremap l n
noremap L N
noremap j J

" Unmap my dvorak movement keys in select mode so snippets don't suck
sunmap s
sunmap S
sunmap t
sunmap n
sunmap N

" Window nav mappings
nmap <C-n> :call SwitchWindowOrBuffer('b')<CR>
nmap <C-t> :call SwitchWindowOrBuffer('f')<CR>
nmap <F4> <C-W>o
nmap <F5> <C-W>c
nmap <F11> <C-W>_
nmap <F6> <C-W>=
nmap <F8> <C-W>-
nmap <F9> <C-W>+
nmap <F7> <C-W><
nmap <F10> <C-W>>

" No more Ex mode mapping. Do something useful instead.
map Q gq

" toggle hlsearch
"map <Leader>h :set hls!<bar>set hls?<CR>

" Load vimrc in a split window and switch to it
nmap <Leader>V :sp ~/.vimrc<cr><C-W>w

" cd to the dir containing the current file
nmap <Leader>cd :lcd %:h<cr>

" Set up retabbing on a source file
nmap <Leader>rr :1,$retab<cr>

" In visual mode press * or # to search for the current selection
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" Searches for the current selection using Ack
vnoremap <silent> gf :call VisualSearch('gf')<CR>

" Quickfix window maps
nmap <C-x> :copen<cr>
nmap <C-q> :cclose<cr>
nmap <C-j> :cn<cr>
nmap <C-k> :cp<cr>

" Align convenience maps
nmap <Leader>ac :AlignCtrl 
nmap <Leader>a vii:Align 
nmap <Leader>a: vii:Align :<cr>
nmap <Leader>a= vii:Align =<cr>
nmap <Leader>a=> vii:Align =><cr>

" Source .vimrc
nmap <Leader>VS :so ~/.vimrc<cr>

" Session management maps
nmap <Leader>SS :mksession! ~/.vim/sessions/
nmap <Leader>SL :source ~/.vim/sessions/

" Turn off diff options
nmap <Leader>do :diffoff<cr>
nmap <Leader>do! :diffoff!<cr>

" Use C-l instead of C-y to insert the first option in the auto completion 
" popup (useful with autocomplpop plugin)
inoremap <C-l> <C-R>=pumvisible() ? "\<lt>C-y>" : "\<lt>C-l>"<cr>



""" Plugin configs and keymaps

" AutoComplPop
let g:acp_mappingDriven = 1
"let g:acp_behaviorSnipmateLength = 1

" Surround plugin
" Change visual surround mappings so s works for movement again
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>Vsurround

" NERDTree mappings and configuration
nmap <Leader>nt :NERDTreeToggle<CR>
nmap <Leader>nm :NERDTreeMirror<CR>
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
let g:SuperTabLongestEnhanced = 1

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
nmap <Leader>gs :Gstatus<cr>
nmap <Leader>gd :Gdiff<cr>
nmap <Leader>gg :Ggrep 
nmap <Leader>glg :Glog<cr>
nmap <Leader>gci :Gcommit<cr>
nmap <Leader>gmv :Gmove 
nmap <Leader>grm :Gremove
nmap <Leader>gpu :Git push<cr>
nmap <Leader>gt :w<cr>:bd<cr>:diffoff!<cr>
nmap <Leader>gta :Gread<cr>:w<cr>:bd<cr>:diffoff!<cr>
" clean up all those buffers fugitive leaves behind
nmap <Leader>gbd :bdelete fugitive://<C-A><cr>

" Snipmate config
let g:snips_author = 'Nathan Howell'

" Jsbeautify config
nnoremap <silent> <leader>jb :call g:Jsbeautify()<cr>

" Yankring config
let g:yankring_enabled = 0
let g:yankring_replace_n_pkey = '<Leader>yp'
let g:yankring_replace_n_nkey = '<Leader>yn'

""" Taglist config
nmap <Leader>,t :TlistToggle<CR>
nmap <Leader>,a :TlistAddFiles
nmap <Leader>,r :TlistAddFilesRecursive .
let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Exit_OnlyWindow = 1
let Tlist_File_Fold_Auto_Close = 1
let Tlist_Compact_Format = 1
let Tlist_Use_Right_Window = 0
let Tlist_Inc_Winwidth = 0
let Tlist_Close_On_Select = 0
let Tlist_Process_File_Always = 1



""" Functions

" I keep hitting :W when saving. It may as well work.
command! W :w

" Switch windows/buffers, depending on whether multiple windows exist.
function! SwitchWindowOrBuffer(d)
    if winbufnr(2) == -1
        if a:d == 'f'
            execute 'normal :bnext'
        elseif a:d == 'b'
            execute 'normal :bprev'
        endif
    else
        if a:d == 'f'
            execute 'normal :wincmd w'
        elseif a:d == 'b'
            execute 'normal :wincmd W'
        endif
    endif
endfunction

" Check whether the cursor has moved to a new line and toggle
" cursorline highlighting (on if on a new line, off if not).
" TODO doesn't seem 100% reliable (eg. C-u, C-d don't toggle it)
" from: http://vim.wikia.com/wiki/Highlight_cursor_line_after_cursor_jump
function! s:Cursor_Moved()
    let cur_pos = line('.')
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
" from: http://amix.dk/vim/vimrc.html (but slightly customized)
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
