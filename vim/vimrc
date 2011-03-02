"==============================================================================
" Initial setup

set nocompatible
" Clear autocmds
autocmd!

" Make vim update screen window title
if &term =~ "^screen"
    set t_ts=k
    set t_fs=\
endif

set background=dark
" Set up color settings and scheme based on terminal type
if has('gui_running')
    colo mustang
else
    if $TERM =~ '^screen-bce' || $TERM =~ '^rxvt-256' || $TERM =~ '^xterm-256'
        set t_Co=256
        colo mustang
    elseif $TERM =~ '^rxvt'
        set t_Co=88
        colo inkpot
    elseif $TERM =~ '^linux'
        set t_Co=8
    else
        set t_Co=16
    endif
endif

" Set up pathogen for loading plugins
filetype off
let g:pathogen_disabled = ["gundo"]
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" Enable filetype detection and syntax highlighting
filetype plugin indent on
syntax on

" Set titlestring when switching buffers (kept short for screen window names)
autocmd BufEnter * let &titlestring = expand("%:t")

" Set up man page viewing
let $PAGER=''
runtime! ftplugin/man.vim

" Pull in dbext database profiles if they exist
if filereadable($HOME.'/dbext_profiles')
    source $HOME/dbext_profiles
endif



"==============================================================================
" Filetype specific options

" add filetype to files that need it
"autocmd BufRead,BufNewFile COMMIT_EDITMSG setlocal filetype git
autocmd BufRead,BufNewFile *.erb setlocal filetype eruby
autocmd BufRead,BufNewFile *.snippet? setlocal filetype=snippet sts=8 sw=8 noet
autocmd BufRead,BufNewFile *.mustache,*.ms setlocal filetype=mustache

" some files need real tabs (I default to spaces for indentation)
autocmd FileType make setlocal noexpandtab
autocmd FileType snippet setlocal noexpandtab

" some files should have different tabsizes and other options
autocmd FileType css,html,javascript setlocal noet

autocmd FileType html,xml setlocal matchpairs+=<:>

" No wrapping for the quickfix window
autocmd BufReadPost quickfix setlocal nowrap

" re-read vimrc after writing it
autocmd BufWritePost \.vimrc :source $HOME/.vimrc
autocmd BufWritePost */vim/vimrc :source $HOME/.vimrc

" Useful when customizing xterm
autocmd BufWritePost \.Xdefaults :!xrdb ~/.Xdefaults

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif



"==============================================================================
" Custom highlighting

" Highlight error logging functions (php error_log, js console.log, etc)
" Terms to be highlighted are defined in ~/.vim/after/syntax/*
hi ErrorLogFunction term=inverse,bold cterm=inverse,bold ctermfg=red ctermbg=black

" Change some highlight group colours, overriding the colour scheme
" search terms
hi Search term=bold cterm=bold ctermfg=black ctermbg=green
hi IncSearch term=bold cterm=bold ctermfg=yellow ctermbg=red
" todo
hi Todo term=bold cterm=bold ctermfg=red ctermbg=yellow
" popup menu
hi Pmenu term=none cterm=none ctermfg=gray ctermbg=black
hi PmenuSel term=bold cterm=bold ctermfg=black ctermbg=green
" diff viewer
hi DiffAdd term=none cterm=none ctermfg=black ctermbg=green
hi DiffChange term=none cterm=none ctermfg=black ctermbg=blue
hi DiffDelete term=none cterm=none ctermfg=black ctermbg=red
hi DiffText term=bold cterm=bold ctermfg=black ctermbg=yellow
" statusline
hi StatusLine ctermfg=185 ctermbg=black

" make special chars (tabs, trailing spaces, etc) barely visible
hi NonText cterm=none ctermfg=237
hi SpecialKey cterm=bold ctermfg=235

" highlight lines longer than 80 chars
hi OverLength ctermbg=234
match OverLength /\%81v.\+/



"==============================================================================
" General options

let mapleader=","
set encoding=utf-8
set fileencodings=utf-8
set ruler
set pastetoggle=<F12>
set title
set laststatus=2
set visualbell t_vb=
set formatoptions+=tcroqnw
set ttyfast
set history=500
set wrap
set linebreak
set showcmd
set nostartofline
set hidden
set backspace=indent,eol,start
set timeoutlen=500

set autoindent
set smartindent

set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab
set smarttab

set incsearch
set hlsearch

set showmatch
set matchtime=2

set ignorecase
set smartcase
set gdefault

set scrolloff=3
set sidescroll=1
set sidescrolloff=2

set completeopt=longest,menuone
set wildmenu
set wildmode=longest,list

set statusline=%f\ %1*%m%r%*%h%w\ %{fugitive#statusline()}%=[%{&ff}\ %{strlen(&fenc)?&fenc:'none'}\ %{&ft}]\ [%LL\ %P\ %l,%v]
" Highlights modified marker in statusline
hi User1 term=inverse,bold cterm=inverse,bold ctermfg=red

" Put backup/swap files all in one place
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" Show trailing whitespace and tabs as visible chars
set list listchars=tab:⇥\ ,trail:·

if v:version >= '703'
    set relativenumber
    hi ColorColumn ctermbg=234 guibg=233
    set cc=+3
    set undodir=~/.vim/undo
    set undofile
endif

" highlight the current line (all the way to the right edge) and column
autocmd CursorMoved,CursorMovedI * call s:Cursor_Moved()
let g:last_screen_pos = 0
let g:last_file_pos = 0
" define highlighting colours for cursor line
hi CursorLine term=none cterm=none ctermbg=234 gui=none guibg=#333333



"==============================================================================
" Key mappings

" A few dvorak movement adjustments
noremap s l
noremap S L
noremap t gj
noremap n gk
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
nmap <F6> <C-W>=
nmap <F7> <C-W><
nmap <F8> <C-W>-
nmap <F9> <C-W>+
nmap <F10> <C-W>>
nmap <F11> <C-W>_

" No more Ex mode mapping. Do something useful instead.
map Q gq

" Move a line of text
" TODO these don't actually work (the mappings, I mean).
nmap <M-t> mz:m+<cr>`z
nmap <M-n> mz:m-2<cr>`z
vmap <M-t> :m'<-2<cr>`>my`<mzgv`yo`z
vmap <M-n> :m'>+<cr>`<my`>mzgv`yo`z

" quickly turn off search highlighting
map <Leader><space> :noh<cr>

" Load vimrc in a split window and switch to it
nmap <Leader>V :sp ~/.vimrc<cr><C-W>w
" Source .vimrc
nmap <Leader>VS :so ~/.vimrc<cr>

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
nmap <Leader>co :copen<cr>
nmap <Leader>cx :cclose<cr>

" Location window maps
nmap <Leader>lo :lopen<cr>
nmap <Leader>lx :lclose<cr>

" Session management maps
nmap <Leader>SS :mksession! ~/.vim/sessions/
nmap <Leader>SL :source ~/.vim/sessions/

" Turn off diff options
nmap <Leader>do :diffoff<cr>
nmap <Leader>do! :diffoff!<cr>

" Use C-l instead of C-y to insert the first option in the auto completion 
" popup (useful with autocomplpop plugin)
inoremap <Nul> <C-R>=pumvisible() ? "\<lt>C-y>" : "\<lt>C-l>"<cr>

" Quick PHP syntax check (CTRL-l)
autocmd FileType php noremap <C-l> :!/usr/bin/php -l %<CR>



"==============================================================================
" Plugin configs

" Indent-object
let g:indent_guides_auto_colors = 0
hi IndentGuidesEven ctermbg=234 ctermfg=235
hi IndentGuidesOdd ctermbg=235 ctermfg=236

" Space
let g:space_no_character_movements = 1
let g:space_disable_select_mode = 1
let g:space_no_search = 1

" Gundo
nnoremap <F3> :GundoToggle<CR>
let g:gundo_map_move_older="t"
let g:gundo_map_move_newer="n"

" Autoclose
nmap <Leader>acm <Plug>ToggleAutoCloseMappings

" AutoComplPop
let g:acp_mappingDriven = 1
"let g:acp_behaviorSnipmateLength = 1

" Surround
" Change visual mode surround mappings so s works for movement again
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>Vsurround

" NERDTree
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

" SuperTab
let g:SuperTabMappingForward = '<c-n>'
let g:SuperTabMappingBackward = '<c-p>'
let g:SuperTabDefaultCompletionType = 'context'
let g:SuperTabLongestHighlight = 1
let g:SuperTabLongestEnhanced = 1

" Lusty Juggler
let g:LustyJugglerShowKeys = 'a'
let g:LustyJugglerAltTabMode = 1
let g:LustyJugglerSuppressRubyWarning = 1
nmap <Leader>b :LustyJuggler<cr>
nmap <C-p> :LustyJugglePrevious<cr>

" Lusty Explorer
nmap <Leader>f :LustyFilesystemExplorer<cr>
nmap <Leader>h :LustyFilesystemExplorerFromHere<cr>
nmap <Leader>e :LustyBufferExplorer<cr>
vmap <Leader>gb :LustyBufferGrep<cr>
let g:LustyExplorerSuppressRubyWarning = 1

" Ack
nmap <Leader>gf :LAck 

" fugitive (git)
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

" Snipmate
let g:snips_author = 'Nathan Howell'

" Jsbeautify
nnoremap <silent> <leader>jb :call g:Jsbeautify()<cr>

" Align
map <Leader>ac :AlignCtrl 
nmap <Leader>a vii:Align 
vmap <Leader>a :Align 

" Yankring
let g:yankring_enabled = 0
let g:yankring_replace_n_pkey = '<Leader>yp'
let g:yankring_replace_n_nkey = '<Leader>yn'



"==============================================================================
" Custom functions and commands

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
" orig from: http://vim.wikia.com/wiki/Highlight_cursor_line_after_cursor_jump
function! s:Cursor_Moved()
    let cur_screen_pos = winline()
    let cur_file_pos = line('.')
    if g:last_file_pos == 0 || g:last_file_pos == 0
        setlocal cursorline
        let g:last_screen_pos = cur_screen_pos
        let g:last_file_pos = cur_file_pos
        return
    endif
    let sdiff = g:last_screen_pos - cur_screen_pos
    let fdiff = g:last_file_pos - cur_file_pos
    if sdiff >= 1 || sdiff <= -1 || fdiff >= 1 || fdiff <= -1
        setlocal cursorline
    else
        setlocal nocursorline
    endif
    let g:last_screen_pos = cur_screen_pos
    let g:last_file_pos = cur_file_pos
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
        execute "normal ?" . l:pattern . ""
    elseif a:direction == 'gf'
        execute "normal :LAck " . l:pattern . ""
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . ""
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction
