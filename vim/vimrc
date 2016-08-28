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
Plug 'bkad/CamelCaseMotion'
Plug 'tpope/vim-fugitive'
Plug 'michaeljsmith/vim-indent-object'
" Plug 'dimasg/vim-mark'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-repeat'
Plug 'chrisbra/SudoEdit.vim'
Plug 'tpope/vim-surround'
Plug 'godlygeek/tabular'
Plug 'coderifous/textobj-word-column.vim'
Plug 'vim-scripts/bufmru.vim'
Plug 'qpkorr/vim-bufkill'

" Language/filetype-specific
Plug 'ehamberg/vim-cute-python'
Plug 'fatih/vim-go'
Plug 'sheerun/vim-polyglot'

" The Rest
Plug 'airblade/vim-gitgutter'
Plug 'Raimondi/delimitMate'
Plug 'majutsushi/tagbar'
Plug 'mattn/gist-vim'
Plug 'vim-scripts/dbext.vim'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-unimpaired'
Plug 'vim-scripts/AnsiEsc.vim'
Plug 'tpope/vim-sleuth'
" Plug 'vim-scripts/easytags.vim'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

" lightline
Plug 'itchyny/lightline.vim'
let g:lightline = {
    \ 'colorscheme': 'powerline',
    \ 'separator': { 'left': '', 'right': '' },
    \ 'subseparator': { 'left': '•', 'right': '•' },
    \ 'active': {
    \   'left': [ [ 'mode', 'paste' ],
    \             [ 'fugitive', 'filename' ],
    \   ],
    \   'right': [ [ 'syntastic' ],
    \              [ 'fileformat', 'fileencoding', 'filetype' ],
    \              [ 'lineinfo' ],
    \   ],
    \ },
    \ 'inactive': {
    \   'right': [ [ 'fileformat', 'fileencoding', 'filetype' ],
    \              [ 'lineinfo' ],
    \   ],
    \ },
    \ 'component_function': {
    \   'mode': 'LLMode',
    \   'readonly': 'LLReadonly',
    \   'fugitive': 'LLFugitive',
    \   'filename': 'LLFilename',
    \   'fileencoding': 'LLFileEncoding',
    \   'fileformat': 'LLFileFormat',
    \   'filetype': 'LLFileType',
    \   'lineinfo': 'LLLineinfo',
    \ },
\}
let g:lightline.mode_map = {
    \ 'n' : 'N',
    \ 'i' : 'I',
    \ 'R' : 'R',
    \ 'v' : 'V',
    \ 'V' : 'V-LINE',
    \ "\<C-v>": 'V-BLOCK',
    \ 'c' : 'C',
    \ 's' : 'S',
    \ 'S' : 'S-LINE',
    \ "\<C-s>": 'S-BLOCK',
    \ 't': 'TERMINAL',
\ }

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
Plug 'vimoutliner/vimoutliner'
Plug 'jceb/vim-orgmode'
Plug 'tpope/vim-speeddating'
Plug 'junegunn/goyo.vim'
Plug 'terryma/vim-expand-region'
Plug 'thirtythreeforty/lessspace.vim'
Plug 'Konfekt/FastFold'
Plug 'ktonga/vim-follow-my-lead'
let g:fml_all_sources = 1
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'dhruvasagar/vim-vinegar'
Plug 'christoomey/vim-sort-motion'
Plug 'thalesmello/vim-textobj-methodcall'
Plug 'junegunn/vim-pseudocl'
Plug 'junegunn/vim-oblique'
Plug 'tmhedberg/SimpylFold'
Plug 'wellle/tmux-complete.vim'
Plug 'szw/vim-maximizer'
let g:maximizer_set_default_mapping = 0

" Colors
Plug 'rainux/vim-desert-warm-256'
Plug 'vim-scripts/Sorcerer'
Plug 'w0ng/vim-hybrid'
Plug 'chriskempson/vim-tomorrow-theme'
Plug 'NLKNguyen/papercolor-theme'
Plug 'morhetz/gruvbox'

" Plug 'Valloric/YouCompleteMe', { 'do': './install.sh' }
function! DoRemote(arg)
    UpdateRemotePlugins
endfunction
Plug 'Shougo/deoplete.nvim', { 'do': function('DoRemote') }
Plug 'zchee/deoplete-jedi'
Plug 'Shougo/neco-vim'
Plug 'Shougo/neoinclude.vim'

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
autocmd BufWritePost *vimrc source $HOME/.vimrc | call lightline#init() | call lightline#colorscheme() | call lightline#update()
autocmd BufRead *vimrc,init.vim,*zshrc,*tmux.conf setlocal foldmethod=marker

autocmd BufRead,BufNewFile *.zsh-theme setlocal filetype=zsh
autocmd BufRead,BufNewFile *tmux.conf setlocal filetype=tmux

" Auto-delete fugitive buffers when hidden
autocmd BufHidden fugitive://* bd %

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

autocmd BufLeave,WinLeave,FocusLost * setlocal number norelativenumber
autocmd BufEnter,WinEnter,FocusGained * setlocal number relativenumber

" }}}
" Custom highlighting {{{ -----------------------------------------------------

" Highlight error logging functions (php error_log, js console.log, etc)
" Terms to be highlighted are defined in ~/.vim/after/syntax/*
hi ErrorLogFunction term=inverse,bold cterm=inverse,bold ctermfg=red ctermbg=black

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

" toggle folds
nnoremap <cr> za
vnoremap <cr> za
autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
autocmd BufReadPost *CtrlSF* nnoremap <buffer> <CR> <CR>
autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR>

" recursively open folds
nnoremap z<space> zCzO

" refocus folds
nnoremap zf zMzvzz

" make Y behave like other capitals
map Y y$

" A few dvorak movement adjustments
noremap <silent> <expr> s "@='l'<cr>"
noremap S L
noremap <silent> <expr> t (v:count == 0 ? 'gj' : "@='j'<cr>")
noremap <silent> <expr> n (v:count == 0 ? 'gk' : "@='k'<cr>")
noremap j J

" Unmap my dvorak movement keys in select mode so Snipmate snippets don't suck
sunmap s
sunmap S
sunmap t

" No more Ex mode mapping. Do something useful instead.
vmap Q gq
nmap Q gqap

" Fast save mapping
nmap <Leader><space> :w<cr>

" Save files that you need sudo for, but didn't open as root
cmap W! silent w !sudo tee % >/dev/null

" quickly turn off search highlighting
nmap <silent> <C-l> :noh<cr>

" Load vimrc in a split window and switch to it
nmap <Leader>V :vs ~/.vimrc<cr>
" Source .vimrc
nmap <Leader>VS :so ~/.vimrc<cr>

" cd to the dir containing the current file
nmap <Leader>cd :lcd %:h<cr>

" Location window maps
nmap <Leader>lo :lopen<cr>
nmap <Leader>lx :lclose<cr>

" Turn off diff options
nmap <Leader>do :diffoff<cr>
nmap <Leader>do! :diffoff!<cr>

" Create folds for objects (tags, paras, blocks, etc.)
nnoremap <Leader>ft Vatzf
nnoremap <Leader>fp Vapzf
nnoremap <Leader>fb VaBzf

let g:WindowCmdMenu = {
    \ 'title': 'Windows',
    \ 'commands': [
        \ 'abort',
        \ 'wincmd c',
        \ 'wincmd x',
        \ 'wincmd L',
        \ 'sb #',
        \ 'wincmd o',
        \ 'wincmd x',
        \ 'MaximizerToggle',
        \ 'wincmd =',
        \ ],
    \ 'options': [
        \ '&close',
        \ '&swap next',
        \ 'move &right',
        \ 's&plit',
        \ '&only',
        \ '&rotate',
        \ 'ma&ximize',
        \ 'e&qualize',
        \ ],
\ }
nmap <silent> <Leader>w :call CmdMenu(WindowCmdMenu)<cr>

let g:FormatCmdMenu = {
    \ 'title': 'Formatting',
    \ 'commands': [
        \ 'abort',
        \ 'setlocal nowrap! nolist!',
        \ '%s/\s\+$//e',
        \ 'call g:Jsbeautify()',
        \ 'setlocal ff=dos',
        \ 'setlocal ff=unix',
        \ ],
    \ 'options': [
        \ '&wrap',
        \ 'rm white&space',
        \ '&jsbeautify',
        \ 'ff2&dos',
        \ 'ff2&unix',
        \ ],
\ }
nmap <silent> <Leader>F :call CmdMenu(FormatCmdMenu)<cr>

let g:QFCmdMenu = {
    \ 'title': 'Quickfix',
    \ 'persist': 'yes',
    \ 'commands': [
        \ 'abort',
        \ 'cnext | normal zv',
        \ 'cprevious | normal zv',
        \ 'call ToggleList("Quickfix List", "c")',
        \ ],
    \ 'options': [
        \ '&next',
        \ '&previous',
        \ '&toggle',
        \ ],
\ }
nmap <silent> <Leader>q :call CmdMenu(QFCmdMenu)<cr>
" nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>


" }}}
" Plugin configs {{{ ----------------------------------------------------------

" oblique
autocmd! User Oblique
autocmd! User ObliqueStar
autocmd! User ObliqueRepeat
autocmd User Oblique       normal! zzzv
autocmd User ObliqueStar   normal! zzzv
autocmd User ObliqueRepeat normal! zzzv
" let g:oblique#prefix = '\v'
nmap l <Plug>(Oblique-n)
nmap L <Plug>(Oblique-N)

" bufkill
let g:BufKillCreateMappings = 0
let g:BufferCmdMenu = {
    \ 'title': 'Buffers',
    \ 'commands': [
        \ 'abort',
        \ 'bnext',
        \ 'bprevious',
        \ 'BD',
        \ 'bwipeout',
        \ ],
    \ 'options': [
        \ '&next',
        \ '&previous',
        \ '&delete',
        \ '&wipe',
        \ ],
\ }
nmap <silent> <Leader>b :call CmdMenu(BufferCmdMenu)<cr>

" nerdtree git plugin
let g:NERDTreeIndicatorMapCustom = {
    \ "Modified"  : "",
    \ "Staged"    : "",
    \ "Untracked" : "",
    \ "Renamed"   : "",
    \ "Unmerged"  : "",
    \ "Deleted"   : "",
    \ "Dirty"     : "✗",
    \ "Clean"     : "",
    \ "Unknown"   : "?",
\ }

" plug
let g:PlugCmdMenu = {
    \ 'title': 'Plugins',
    \ 'commands': [
        \ 'abort',
        \ 'PlugInstall',
        \ 'PlugUpdate',
        \ 'PlugClean',
        \ ],
    \ 'options': [
        \ '&install',
        \ '&update',
        \ '&clean',
        \ ],
\ }
nmap <silent> <Leader>p :call CmdMenu(PlugCmdMenu)<cr>

" deoplete
let g:deoplete#enable_at_startup = 1

" bufmru
let g:bufmru_switchkey = "<C-e>"

" tmux-complete
let g:tmuxcomplete#trigger = 'omnifunc'

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
  set noshowmode
  set showcmd
  set scrolloff=3
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

" vim-orgmode
let g:org_heading_shade_leading_stars = 1

" vimagit
" No way to disable this mapping entirely :-(
let g:magit_show_magit_mapping="'"
autocmd FileType magit nnoremap <buffer> q :BD<cr>
autocmd FileType magit nnoremap <buffer> P :execute 'normal zt' <bar> :call magit#jump_hunk('P')<cr>
autocmd FileType magit nnoremap <buffer> N :execute 'normal zt' <bar> :call magit#jump_hunk('N')<cr>
autocmd User VimagitEnterCommit startinsert

" vim-dim-inactive
let g:diminactive_use_colorcolumn = 1
let g:diminactive_use_syntax = 0
let g:diminactive_enable_focus = 1

" vim-tmux-navigator
let g:tmux_navigator_no_mappings = 1
" There's an issue with <C-h> maps and neovim described here:
" https://github.com/neovim/neovim/issues/2048#issuecomment-78045837
" The following commands fix it:
"   infocmp $TERM | sed 's/kbs=^[hH]/kbs=\\177/' > $TERM.ti
"   tic $TERM.ti
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
hi link SneakPluginTarget Search

" vim-pad
let g:pad#set_mappings = 0
let g:pad#dir = "~/notes"
let g:pad#open_in_split = 0
let g:pad#search_backend = 'ag'
let g:pad#title_first_line = 1
let g:PadCmdMenu = {
    \ 'title': 'Notes',
    \ 'commands': [
        \ 'abort',
        \ 'Pad new',
        \ 'Pad ls',
        \ 'Pad! ls',
        \ ],
    \ 'options': [
        \ '&new',
        \ '&list',
        \ 'list &all',
        \ ],
\ }
nmap <silent> <Leader>n :call CmdMenu(PadCmdMenu)<cr>

" ctrlsf
let g:ctrlsf_position = 'bottom'
let g:ctrlsf_winsize = '60%'
let g:ctrlsf_leading_space = '4'
vmap a <Plug>CtrlSFVwordExec
vmap A <Plug>CtrlSFQuickfixVwordExec
let g:ctrlsf_mapping = {
    \ "next": "N",
    \ "prev": "P",
    \ "tab": "",
    \ "tabb": "",
    \ "popen": "p",
    \ "quit": "q",
\ }
let g:SearchCmdMenu = {
    \ 'title': 'Search in Files',
    \ 'commands': [
        \ 'abort',
        \ ':CtrlSF ',
        \ 'CtrlSFUpdate',
        \ 'CtrlSFToggle',
        \ ':CtrlSFQuickfix ',
        \ ],
    \ 'options': [
        \ 'se&arch',
        \ '&update',
        \ '&toggle',
        \ '&quickfix',
        \ ],
\ }
nmap <silent> <Leader>a :call CmdMenu(SearchCmdMenu)<cr>

" vim-go
let g:go_bin_path = expand("~/bin")

" gitgutter
let g:gitgutter_map_keys = 0
let g:gitgutter_sign_added = '▶'
let g:gitgutter_sign_modified = '◆'
let g:gitgutter_sign_removed = '◀'
let g:gitgutter_sign_modified_removed = '◆'

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
let g:MarkCmdMenu = {
    \ 'title': 'Marks',
    \ 'commands': [
        \ 'abort',
        \ 'MarkSet',
        \ ],
    \ 'options': [
        \ '&new',
        \ ],
\ }
nmap <silent> <Leader>M :call CmdMenu(MarkCmdMenu)<cr>
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

" Surround
" Change visual mode surround mappings so s works for movement again
vmap <Leader>s <Plug>Vsurround
vmap <Leader>S <Plug>Vsurround

" NERDTree
let g:NERDTreeMapOpenInTab="a"
let g:NERDTreeMapOpenInTabSilent="A"
let g:NERDTreeMapJumpFirstChild="N"
let g:NERDTreeMapJumpLastChild="T"
let g:NERDTreeMapJumpNextSibling="<C-S-T>"
let g:NERDTreeMapJumpPrevSibling="<C-S-N>"
let g:NERDTreeQuitOnOpen=1
let g:NERDTreeAutoCenter=1
let g:NERDTreeAutoCenterThreshold=6
let g:NerdtreeCmdMenu = {
    \ 'title': 'NERDTree',
    \ 'commands': [
        \ 'abort',
        \ 'NERDTreeToggle',
        \ 'NERDTreeFind',
        \ ],
    \ 'options': [
        \ 'open &tree',
        \ 'open &here',
        \ ],
\ }
nmap <silent> <Leader>t :call CmdMenu(NerdtreeCmdMenu)<cr>

" fugitive (git)
let g:GitCmdMenu = {
    \ 'title': 'Git',
    \ 'commands': [
        \ 'abort',
        \ 'Gstatus',
        \ 'Gvdiff',
        \ 'MagitOnly',
        \ 'call CmdMenu(g:GitStageCmdMenu)',
        \ 'Git! diff --staged',
        \ 'Gcommit',
        \ 'Gpush',
        \ 'Gpull',
        \ ':Gmove ',
        \ ':Gremove',
        \ ],
    \ 'options': [
        \ '&status',
        \ '&diff',
        \ '&magit',
        \ 's&tage',
        \ 're&view',
        \ '&commit',
        \ '&push',
        \ 'pul&l',
        \ 'mo&ve',
        \ '&remove',
        \ ],
\ }
let g:GitStageCmdMenu = {
    \ 'title': 'Git staging',
    \ 'commands': [
        \ 'abort',
        \ 'Gwrite',
        \ "call gitgutter#stage_hunk()",
        \ ['w', 'bd', 'diffoff!'],
        \ ['Gread', 'w', 'bd', 'diffoff!'],
        \ ],
    \ 'options': [
        \ '&file',
        \ '&hunk',
        \ 's&tage',
        \ 'stage &all',
        \ ],
\ }
nmap <silent> <Leader>g :call CmdMenu(GitCmdMenu)<cr>
nmap [c <Plug>GitGutterPrevHunk
nmap ]c <Plug>GitGutterNextHunk


" }}}
" Custom functions and commands {{{ -------------------------------------------

" Handy Quickfix/Location list toggling
function! GetBufferList()
  redir =>buflist
  silent! ls!
  redir END
  return buflist
endfunction

function! ToggleList(bufname, pfx)
  let buflist = GetBufferList()
  for bufnum in map(filter(split(buflist, '\n'), 'v:val =~ "'.a:bufname.'"'), 'str2nr(matchstr(v:val, "\\d\\+"))')
    if bufwinnr(bufnum) != -1
      exec(a:pfx.'close')
      return
    endif
  endfor
  if a:pfx == 'l' && len(getloclist(0)) == 0
      echohl ErrorMsg
      echo "Location List is Empty."
      return
  endif
  let winnr = winnr()
  exec(a:pfx.'open')
  if winnr() != winnr
    wincmd p
  endif
endfunction


" Lightline functions
function! LLReadonly()
    if &filetype == "help"
        return ""
    elseif &readonly
        return ""
    else
        return ""
    endif
endfunction

function! LLModified()
    if &filetype == "help"
        return ""
    elseif &modified
        return ""
    elseif &modifiable
        return ""
    else
        return ""
    endif
endfunction

function! LLFugitive()
    if &filetype == "help"
        return ""
    else
        if exists("*fugitive#head")
            let branch = fugitive#head()
            return branch !=# '' ? ' '.branch : ''
        endif
    endif
    return ''
endfunction

function! LLFilename()
    return ('' != LLReadonly() ? LLReadonly() . ' ' : '') .
         \ ('' != expand('%:t') ? expand('%:t') : '[No Name]') .
         \ ('' != LLModified() ? ' ' . LLModified() : '')
endfunction

function! LLFileEncoding()
    return &fenc != 'utf-8' ? &fenc : ''
endfunction

function! LLFileFormat()
    return &fileformat != 'unix' ? &fileformat : ''
endfunction

function! LLFileType()
    return &filetype == "help" ? '' : &filetype
endfunction

function! LLLineinfo()
    if &filetype == "help"
        return line('.') * 100 / line('$') . '%'
    else
        return line('.') * 100 / line('$') . '%' . ' ' . line('.') . ':' . getcurpos()[2]
    endif
endfunction

function! LLMode()
    return &filetype == "help" ? '' : lightline#mode()
endfunction


if !exists("*CmdMenu")
function! CmdMenu(conf)
    " This menu function takes a dictionary like this one:
    "
    " example = {
    "     \ 'title': 'Menu Title',
    "     \ 'commands': [
    "         \ 'abort',
    "         \ 'something that `exe` can run',
    "         \ ['each', 'list', 'item', 'will', 'be', 'executed'],
    "         \ ':user will be prompted for input',
    "     \],
    "     \ 'options': [
    "         \ '&nice name for the command',
    "     \],
    " \}
    "
    " The 'options' list will be displayed as menu options. The ampersand indicates
    " the shortcut key to press for that option. The 'commands' list must be in the
    " same order as the options, as the list index is used for selection, and must
    " be full of commands that `execute` can run. If an item is a list of commands,
    " they will all be run.
    "
    " The 'abort' option should be left as a filler. Hitting escape cancels the
    " menu.
    "
    " If the argument has a key named 'persist', then it will remain active
    " until the user exits with <c-c> or <esc>. Useful for things like a
    " quickfix navigation menu.
    "
    " The idea for this came from http://marcotrosi.tumblr.com/post/134219004828/vim-confirm-function

    while 1
        let l:choice = 0
        let l:choice = confirm(a:conf.title, join(a:conf.options, "\n"))
        if l:choice != 0
            " type 1 == string
            if type(a:conf.commands[l:choice]) == 1
                if a:conf.commands[l:choice] =~ '^:.*'
                    let l:cmd = input('', a:conf.commands[l:choice])
                    execute l:cmd
                else
                    execute a:conf.commands[l:choice]
                endif
            " type 3 == list
            elseif type(a:conf.commands[l:choice]) == 3
                for l:cmd in a:conf.commands[l:choice]
                    execute l:cmd
                endfor
            endif
        endif
        " Clear and redraw to get rid of previous output on persistent menus
        silent execute 'redraw!'
        " If this is a single run menu or <esc> was pressed, break out
        if ! has_key(a:conf, 'persist') || l:choice == 0
            break
        endif
    endwhile
endfunction
endif


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
