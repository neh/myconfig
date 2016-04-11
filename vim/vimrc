" Initial Setup {{{ -----------------------------------------------------------

" Clear autocmds
autocmd!

" Enable filetype detection and syntax highlighting
filetype plugin indent on
syntax on

" Make vim update screen window title
" if &term =~ "^screen"
"     set t_ts=k
"     set t_fs=\
" endif
" Workaround to make italics work in tmux sessions
if $TMUX != '' && &term =~ "xterm-256color"
    set t_so=[7m
    set t_ZH=[3m
endif

" Set titlestring when switching buffers (kept short for screen window names)
autocmd BufEnter * let &titlestring = expand("%:t").' - VIM'

" Set up man page viewing
" let $PAGER=''
" runtime! ftplugin/man.vim

runtime macros/matchit.vim

" Pull in dbext database profiles if they exist
if filereadable($HOME.'/dbext_profiles')
    source $HOME/dbext_profiles
endif


" }}}
" Plugins {{{ -----------------------------------------------------------------

call plug#begin('~/.vim/plugged')

" Essential
Plug 'vim-scripts/bufmru.vim'
Plug 'bkad/CamelCaseMotion'
Plug 'tpope/vim-fugitive'
Plug 'michaeljsmith/vim-indent-object'
Plug 'dimasg/vim-mark'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-repeat'
Plug 'chrisbra/SudoEdit.vim'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'coderifous/textobj-word-column.vim'

" Language/filetype-specific
Plug 'ehamberg/vim-cute-python'
Plug 'fatih/vim-go'
Plug 'sheerun/vim-polyglot'

" The Rest
Plug 'airblade/vim-gitgutter'
Plug 'Raimondi/delimitMate'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'majutsushi/tagbar'
Plug 'mattn/gist-vim'
Plug 'vim-scripts/dbext.vim'
Plug 'scrooloose/syntastic'
Plug 'sjl/gundo.vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/AnsiEsc.vim'
Plug 'tpope/vim-sleuth'
" Plug 'vim-scripts/easytags.vim'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-lastpat'
Plug 'tpope/vim-commentary'
Plug 'jamessan/vim-gnupg'
Plug 'dyng/ctrlsf.vim'
Plug 'tmux-plugins/vim-tmux-focus-events'

Plug 'blindFS/vim-taskwarrior'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install --all' }
Plug 'junegunn/fzf.vim'
Plug 'fmoralesc/vim-pad'
Plug 'justinmk/vim-sneak'
Plug 'pearofducks/ansible-vim'
Plug 'ConradIrwin/vim-bracketed-paste'
Plug 'christoomey/vim-tmux-navigator'
Plug 'jreybert/vimagit'
Plug 'blueyed/vim-diminactive'
Plug 'nhooyr/neoman.vim'
Plug 'vimoutliner/vimoutliner'
Plug 'jceb/vim-orgmode'
Plug 'tpope/vim-speeddating'
Plug 'junegunn/goyo.vim'
Plug 'terryma/vim-expand-region'
Plug 'thirtythreeforty/lessspace.vim'

" Colors
Plug 'rainux/vim-desert-warm-256'
Plug 'vim-scripts/Sorcerer'
Plug 'w0ng/vim-hybrid'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'NLKNguyen/papercolor-theme'
Plug 'morhetz/gruvbox'

Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }

call plug#end()

" }}}
" Color settings {{{ -----------------------------------------------------------------

set background=dark
if has('nvim')
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1

    " gruvbox colorscheme
    let g:gruvbox_italic = 1
    let g:gruvbox_contrast_dark = 'medium'
    let g:gruvbox_contrast_light = 'hard'
    let g:gruvbox_invert_selection = 0
    colo gruvbox
else
    " Set up color settings and scheme based on terminal type
    if $TERM == 'xterm' || $TERM == 'xterm-256color' || $TERM =~ '256'
        set t_Co=256
        colo Tomorrow-Night
    else
        set t_Co=16
    endif
endif

" }}}
" Filetype specific options {{{ -----------------------------------------------

" add filetype to files that need it
"autocmd BufRead,BufNewFile COMMIT_EDITMSG setlocal filetype git
autocmd BufRead,BufNewFile *.erb setlocal filetype=eruby
autocmd BufRead,BufNewFile Berksfile setlocal filetype=ruby
autocmd BufRead,BufNewFile *.snippet? setlocal filetype=snippet sts=8 sw=8 noet
autocmd BufRead,BufNewFile *.mustache,*.ms setlocal filetype=mustache
autocmd BufRead,BufNewFile *.md setlocal filetype=markdown

" some files need real tabs (I default to spaces for indentation)
autocmd FileType make setlocal noexpandtab
autocmd FileType snippet setlocal noexpandtab

"autocmd FileType text,mail setlocal formatprg=perl\ -MText::Autoformat\ -e\ 'autoformat{all=>1}'
autocmd FileType text,mail setlocal formatoptions+=t formatprg=par\ -w80\ -q

autocmd FileType html setlocal makeprg=tidy\ -q\ -e\ %
autocmd FileType html setlocal errorformat=line\ %l\ column\ %v\ -\ %m
autocmd FileType html setlocal equalprg=tidy\ -q\ -w\ -i

autocmd FileType ruby setlocal sts=2 sw=2 ts=2

" No wrapping for the quickfix window
autocmd BufReadPost quickfix setlocal nowrap

" re-read vimrc after writing it
autocmd BufWritePost *vimrc source $HOME/.vimrc | call ReloadAirline()
autocmd BufRead *vimrc,init.vim,*zshrc,*tmux.conf setlocal foldmethod=marker

autocmd BufRead,BufNewFile *.zsh-theme setlocal filetype=zsh
autocmd BufRead,BufNewFile *tmux.conf setlocal filetype=tmux

" Useful when customizing xterm
autocmd BufWritePost *Xdefaults*,*Xresources* :!xrdb ~/.Xresources
" give machine-specific X setting files the proper filetype
autocmd BufRead,BufNewFile *Xdefaults-* setlocal filetype=xdefaults

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif

" folding setup
let php_folding = 1
autocmd FileType javascript call JavaScriptFold()
autocmd FileType css,php setlocal foldmethod=syntax

" completion setup
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
autocmd FileType php setlocal omnifunc=phpcomplete#CompletePHP

let php_sql_query = 1
let php_htmlInStrings = 1
let php_parent_error_close = 1
let php_parent_error_open = 1


" }}}
" Custom highlighting {{{ -----------------------------------------------------

" Highlight error logging functions (php error_log, js console.log, etc)
" Terms to be highlighted are defined in ~/.vim/after/syntax/*
hi ErrorLogFunction term=inverse,bold cterm=inverse,bold ctermfg=red ctermbg=black

" Change some highlight group colours, overriding the colour scheme search terms
hi Search term=none cterm=bold ctermfg=252 ctermbg=22
hi IncSearch term=none cterm=none ctermfg=232 ctermbg=41
" todo
hi Todo term=bold cterm=bold ctermfg=red ctermbg=yellow
" popup menu
hi Pmenu term=none cterm=none ctermfg=250 ctermbg=238
hi PmenuSel term=bold cterm=bold ctermfg=black ctermbg=250

if $TMUX != '' || $TERM == 'rxvt-256color'
    hi Folded cterm=italic gui=italic
    hi Comment cterm=italic gui=italic
endif

" make special chars (tabs, trailing spaces, etc) barely visible
hi SpecialKey cterm=none ctermfg=red
" other special chars (line wrap chars etc.)
" hi NonText cterm=none ctermfg=240 ctermbg=235

autocmd BufEnter !CTRLSF hi ExtraWhitespace ctermbg=124 ctermfg=white guibg=red guifg=white
autocmd Syntax !CTRLSF syn match ExtraWhitespace /\s\+$\| \+\ze\t/ containedin=ALL

hi Title cterm=bold ctermfg=118

hi scmLineAdded ctermfg=green
hi scmLineChanged ctermfg=yellow
hi scmLineRemoved ctermfg=red

hi EndOfBuffer ctermbg=black guibg=#000000

" }}}
" General options {{{ ---------------------------------------------------------

let mapleader=","
map <Space> <Leader>
let maplocalleader="\\"
set updatetime=750
set number
set relativenumber
if !has('nvim')
    set encoding=utf-8
endif
set fileencodings=utf-8
set ruler
set pastetoggle=<F12>
set title
set laststatus=2
set visualbell t_vb=
set formatoptions+=croqnwl
if has ('ttyfast')
    set ttyfast
endif
set history=500
set nowrap
set linebreak
set showcmd
set nostartofline
set hidden
set backspace=indent,eol,start
if ! has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        au InsertEnter * set timeoutlen=0
        au InsertLeave * set timeoutlen=500
    augroup END
endif
set noshowmode
set lazyredraw

set shortmess=atI

set autoindent
set smartindent

set textwidth=79
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

set splitright
set splitbelow

set completeopt=longest,menuone

if has("wildmenu")
    set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.jpeg,*.png
    set wildignore+=*~,*.swp,*.tmp,.DS_Store
    set wildmenu
    set wildmode=longest,list
endif

if has('unnamedplus')
  set clipboard=unnamedplus
else
  set clipboard=unnamed
endif

" Put backup/swap files all in one place
set backupdir=~/.vim/backup
set directory=~/.vim/backup

" Enable mouse usage in terminals
" (allows window resizing, mousewheel scrolling, proper text highlighting)
set mouse=a
if has('ttymouse')
  set ttymouse=xterm2
  " Workaround for bug in vim that breaks mouse support when in tmux.
  " bug: http://groups.google.com/group/vim_dev/browse_thread/thread/0416d81258cbb5a0?pli=1
  " workaround: https://wincent.com/blog/tweaking-command-t-and-vim-for-use-in-the-terminal-and-tmux
  if $TMUX != '' || $TERM == 'rxvt-256color'
      autocmd VimEnter * set ttymouse=xterm2
      autocmd FocusGained * set ttymouse=xterm2
      autocmd BufEnter * set ttymouse=xterm2
  endif
endif

" Show trailing whitespace and tabs as visible chars
set list
set listchars=tab:➜\ ,trail:·,extends:❱,precedes:❰

" Mark column 80, method depending on vim version
if exists('+colorcolumn')
    hi ColorColumn ctermbg=black guibg=#000000
    " set cc=80
    " let &colorcolumn=join(range(81,300), ",")
endif

if v:version >= '703'
    set undodir=~/.vim/undo
    set undofile
endif

let &showbreak = '↪ '

" make concealed chars stay concealed when I'm just moving around
set concealcursor=nc

" Remove parts of the gui, just in case I happen to run it
set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L
set guioptions-=b


" }}}
" Key mappings {{{ ------------------------------------------------------------

" easier terminal escape
tnoremap <Leader><ESC> <C-\><C-n>

" highlight last inserted text
nnoremap gV `[v`]

" quick vundle maps
nmap <leader>pi :PlugInstall<cr>
nmap <leader>pu :PlugUpdate<cr>
nmap <leader>pc :PlugClean<cr>

" toggle folds
nnoremap <cr> za
vnoremap <cr> za

" recursively open folds
nnoremap z<space> zCzO

" refocus folds
nnoremap zf zMzvzz

" delete trailing whitespace
map <leader>dtr :%s/\s\+$//e<cr>

" make Y behave like other capitals
map Y y$

" A few dvorak movement adjustments
noremap s l
noremap S L
noremap t gj
noremap n gk
noremap l n
noremap L N
noremap j J

" Keep line containing search term centered and unfold as needed
nnoremap l nzzzv
nnoremap L Nzzzv

" Unmap my dvorak movement keys in select mode so Snipmate snippets don't suck
sunmap s
sunmap S
sunmap t
sunmap n
sunmap N

" Buffer delete maps
nmap <Leader>bd :bd<CR>
nmap <Leader>BD :bd!<CR>

" No more Ex mode mapping. Do something useful instead.
vmap Q gq
nmap Q gqap

" Fast save mapping
nmap <Leader><space> :w<cr>

" Save files that you need sudo for, but didn't open as root
cmap W! silent w !sudo tee % >/dev/null

" Change regex handling
nnoremap / /\v
vnoremap / /\v

" Toggle wrapping
nnoremap <silent> <Leader>w :setlocal nowrap! nolist!<cr>

" quickly turn off search highlighting
nmap <silent> <C-l> :noh<cr>

" Load vimrc in a split window and switch to it
nmap <Leader>V :vs ~/.vimrc<cr>
" Source .vimrc
nmap <Leader>VS :so ~/.vimrc<cr>

" cd to the dir containing the current file
nmap <Leader>cd :lcd %:h<cr>

" Set up retabbing on a source file
nmap <Leader>rt :1,$retab<cr>

" In visual mode press * or # to search for the current selection
vnoremap <silent> * :call VisualSearch('f')<CR>zzzv
vnoremap <silent> # :call VisualSearch('b')<CR>zzzv

" Quickfix window maps
nmap <Leader>co :copen<cr>
nmap <Leader>cx :cclose<cr>

" Location window maps
nmap <Leader>lo :lopen<cr>
nmap <Leader>lx :lclose<cr>

" Turn off diff options
nmap <Leader>do :diffoff<cr>
nmap <Leader>do! :diffoff!<cr>

" Quick PHP syntax check (CTRL-l)
autocmd FileType php noremap <Leader>phpl :!/usr/bin/php -l %<CR>

" Create folds for objects (tags, paras, blocks, etc.)
nnoremap <Leader>ft Vatzf
nnoremap <Leader>fp Vapzf
nnoremap <Leader>fb VaBzf

" fast file format conversion
nnoremap <Leader>ffd :setlocal ff=dos<CR>
nnoremap <Leader>ffu :setlocal ff=unix<CR>


" }}}
" Plugin configs {{{ ----------------------------------------------------------

" vim-expand-region
vmap v <Plug>(expand_region_expand)
vmap <C-v> <Plug>(expand_region_shrink)

" goyo
nmap <Leader>cl :Goyo<cr>

function! s:goyo_enter()
  silent !tmux set status off
  silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  set noshowmode
  set noshowcmd
  set scrolloff=999
endfunction
function! s:goyo_leave()
  silent !tmux set status on
  silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  set showmode
  set showcmd
  set scrolloff=3
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" vim-orgmode
let g:org_heading_shade_leading_stars = 1

" vimagit
autocmd User VimagitEnterCommit startinsert

" vim-dim-inactive
let g:diminactive_use_colorcolumn = 1
let g:diminactive_use_syntax = 0
let g:diminactive_enable_focus = 1

" vim-tmux-navigator
let g:tmux_navigator_no_mappings = 1
nnoremap <silent> <C-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <C-t> :TmuxNavigateDown<cr>
nnoremap <silent> <C-n> :TmuxNavigateUp<cr>
nnoremap <silent> <C-s> :TmuxNavigateRight<cr>
" nnoremap <silent> {Previous-Mapping} :TmuxNavigatePrevious<cr>

" ansible-vim
" let g:ansible_extra_syntaxes = ''
" let g:ansible_attribute_highlight = 'ad'
let g:ansible_name_highlight = 'b'
let g:ansible_extra_keywords_highlight = 1

" fzf
nmap <Leader>o :execute 'Files' fnameescape(getcwd())<cr>
nmap <Leader>e :Buffers<cr>


" vim-sneak
nmap f <Plug>Sneak_f
nmap F <Plug>Sneak_F
xmap f <Plug>Sneak_f
xmap F <Plug>Sneak_F
omap f <Plug>Sneak_f
omap F <Plug>Sneak_F
nmap k <Plug>Sneak_t
nmap K <Plug>Sneak_T
xmap k <Plug>Sneak_t
xmap K <Plug>Sneak_T
omap k <Plug>Sneak_t
omap K <Plug>Sneak_T

nmap ? <Plug>SneakPrevious

hi link SneakPluginTarget Search

" vim-pad
let g:pad#set_mappings = 0
let g:pad#dir = "~/notes"
let g:pad#open_in_split = 0
let g:pad#search_backend = 'ag'
let g:pad#title_first_line = 1
nmap <leader>nn <Plug>(pad-new)
nmap <leader>nl <Plug>(pad-list)
nmap <leader>ns <Plug>(pad-incremental-search)

" ctrlsf
let g:ctrlsf_position = 'right'
let g:ctrlsf_winsize = '80%'
let g:ctrlsf_leading_space = '8'
nmap <Leader>a <Plug>CtrlSFPrompt
nmap <Leader>A <Plug>CtrlSFOpen
vmap a <Plug>CtrlSFVwordExec
vmap A <Plug>CtrlSFVwordPath

" vim-go
let g:go_bin_path = expand("~/bin")

" gitgutter
let g:gitgutter_sign_added = '▶'
let g:gitgutter_sign_modified = '◆'
let g:gitgutter_sign_removed = '◀'
let g:gitgutter_sign_modified_removed = '◆'

" vim-airline
let g:airline_left_sep=''
let g:airline_right_sep=''
let g:airline_theme='tomorrow'
if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif
let g:airline_symbols.branch=''
let g:airline_symbols.readonly=' '
let g:airline_section_z='%p%% %l:%c'
let g:airline#extensions#whitespace#trailing_format = '%s·'
let g:airline#extensions#whitespace#mixed_indent_format = '%s➜'
let g:airline#extensions#branch#empty_message = ''
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#tagbar#enabled = 0
call airline#parts#define_condition('ffenc', '&fenc != "utf-8" || &ff != "unix"')

" CamelCaseMotion (need this in after/plugin as well)
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
sunmap w
sunmap b
sunmap e
omap <silent> iw <Plug>CamelCaseMotion_iw
xmap <silent> iw <Plug>CamelCaseMotion_iw
omap <silent> ib <Plug>CamelCaseMotion_ib
xmap <silent> ib <Plug>CamelCaseMotion_ib
omap <silent> ie <Plug>CamelCaseMotion_ie
xmap <silent> ie <Plug>CamelCaseMotion_ie

" SudoEdit
nmap <Leader>W :SudoWrite<cr>
vmap <Leader>W :SudoWrite<cr>

" PHPCtags
if executable($HOME . "/myconfig/phpctags/phpctags")
    let g:tagbar_phpctags_bin=$HOME.'/myconfig/phpctags/phpctags'
endif

" Mark
nmap <Leader><Leader><Leader>/ <Plug>MarkSearchAnyNext
function! s:SetMarkColours()
    highlight MarkWord1 ctermbg=208 ctermfg=Black guibg=#F2891F guifg=Black
    highlight MarkWord2 ctermbg=148 ctermfg=Black guibg=#BCFA37 guifg=Black
    highlight MarkWord3 ctermbg=075 ctermfg=Black guibg=#4DD0F7 guifg=Black
    highlight MarkWord4 ctermbg=185 ctermfg=Black guibg=#EFF589 guifg=Black
    highlight MarkWord5 ctermbg=197 ctermfg=Black guibg=#F52A63 guifg=Black
    highlight MarkWord6 ctermbg=249 ctermfg=Black guibg=#E3E3E3 guifg=Black
endfunction
autocmd ColorScheme * call s:SetMarkColours()
call s:SetMarkColours()

" bufmru
let g:bufmru_switchkey = "<c-e>"

" Syntastic
let g:syntastic_auto_loc_list=2
let g:syntastic_enable_signs=1

" Tagbar
nmap <Leader>. :TagbarToggle<CR>
let g:tagbar_autoclose = 1
let g:tagbar_autofocus = 1
let g:tagbar_usearrows = 1
let g:tagbar_singleclick = 1

let g:tagbar_type_go = {
    \ 'ctagstype' : 'go',
    \ 'kinds'     : [
        \ 'p:package',
        \ 'i:imports:1',
        \ 'c:constants',
        \ 'v:variables',
        \ 't:types',
        \ 'n:interfaces',
        \ 'w:fields',
        \ 'e:embedded',
        \ 'm:methods',
        \ 'r:constructor',
        \ 'f:functions'
    \ ],
    \ 'sro' : '.',
    \ 'kind2scope' : {
        \ 't' : 'ctype',
        \ 'n' : 'ntype'
    \ },
    \ 'scope2kind' : {
        \ 'ctype' : 't',
        \ 'ntype' : 'n'
    \ },
    \ 'ctagsbin'  : 'gotags',
    \ 'ctagsargs' : '-sort -silent'
\ }

let g:tagbar_type_yaml = {
    \ 'ctagstype' : 'ansible',
    \ 'kinds'     : [
        \ 'k:tasks'
    \ ]
\ }

" dbext
let g:dbext_default_use_sep_result_buffer = 1
let g:dbext_default_DBI_list_proc_SQLAnywhere =
          \ 'SELECT p.proc_name, u.user_name '.
          \ '  FROM SYS.SYSPROCEDURE as p, '.
          \ '       SYS.SYSUSERPERM as u '.
          \ ' WHERE p.creator = u.user_id '.
          \ '   AND p.proc_name like ''dbext_replace_name%''   '.
          \ '   AND u.user_name not like ''dbext_replace_owner%''  '.
          \ ' ORDER BY proc_name'
let g:dbext_default_DBI_desc_proc_PGSQL = 'select p.* from pg_proc p, pg_language l where p.prolang = l.oid and p.proname = ''proc_percentiles'' order by p.pronargs'
"let g:dbext_default_DBI_desc_proc_PGSQL = "select p.* from pg_proc p, pg_language l where p.prolang = l.oid and p.proname = ''dbext_replace_name'' order by p.pronargs"

" Gundo
nmap <Leader>un :GundoToggle<CR>
let g:gundo_map_move_older="t"
let g:gundo_map_move_newer="n"

" Surround
" Change visual mode surround mappings so s works for movement again
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>Vsurround

" NERDTree
nmap <Leader>nt :NERDTreeToggle<CR>
nmap <Leader>nm :NERDTreeMirror<CR>
nmap <Leader>nh :NERDTreeFind<CR>
let g:NERDTreeMapOpenInTab="a"
let g:NERDTreeMapOpenInTabSilent="A"
let g:NERDTreeMapJumpFirstChild="N"
let g:NERDTreeMapJumpLastChild="T"
let g:NERDTreeMapJumpNextSibling="<C-S-T>"
let g:NERDTreeMapJumpPrevSibling="<C-S-N>"
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeAutoCenter=1
let g:NERDTreeAutoCenterThreshold=6

" fugitive (git)
nmap <Leader>gs :Gstatus<cr>
nmap <Leader>gd :Gvdiff<cr>
nmap <Leader>gg :Ggrep 
nmap <Leader>glg :Glog<cr>
nmap <Leader>gc :Gcommit<cr>
nmap <Leader>gmv :Gmove 
nmap <Leader>grm :Gremove
nmap <Leader>gpu :Git push<cr>
nmap <Leader>gt :w<cr>:bd<cr>:diffoff!<cr>
nmap <Leader>gta :Gread<cr>:w<cr>:bd<cr>:diffoff!<cr>
" clean up all those buffers fugitive leaves behind
nmap <Leader>gbd :bdelete fugitive://<C-A><cr>

" Jsbeautify
nnoremap <silent> <leader>jb :call g:Jsbeautify()<cr>


" }}}
" Custom functions and commands {{{ -------------------------------------------

function! ReloadAirline()
    execute ":AirlineTheme " . g:airline_theme
endfunction

function! SmartAlign(mode)
    let cur_line = getline('.')

    if a:mode =~ "[vV]"
        let range = "'<,'>"
    else
        let range = ''
    endif

    if cur_line =~ ":"
        execute "normal! :" . range . "Tabularize first_colon"
    elseif cur_line =~ "="
        execute "normal! :" . range . "Tabularize assignment"
    endif
endfunction


" I keep hitting :W when saving. It may as well work.
command! W :w


" Number text object. From http://vimbits.com/bits/334
onoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
xnoremap N :<c-u>call <SID>NumberTextObject(0)<cr>
onoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap aN :<c-u>call <SID>NumberTextObject(1)<cr>
onoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>
xnoremap iN :<c-u>call <SID>NumberTextObject(1)<cr>

function! s:NumberTextObject(whole)
    normal! v

    while getline('.')[col('.')] =~# '\v[0-9]'
        normal! l
    endwhile

    if a:whole
        normal! o

        while col('.') > 1 && getline('.')[col('.') - 2] =~# '\v[0-9]'
            normal! h
        endwhile
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
" highlight the current line (all the way to the right edge), but only if the 
" cursor has moved to a new line
"autocmd CursorMoved,CursorMovedI * call s:Cursor_Moved()
let g:last_screen_pos = 0
let g:last_file_pos = 0


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


" Show syntax highlighting groups for word under cursor
nmap <Leader>syn :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" }}}
" Local config settings {{{ ---------------------------------------------------

if filereadable($HOME . "/.local.vim")
    source $HOME/.local.vim
endif

" }}}
