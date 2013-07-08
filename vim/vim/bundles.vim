set nocompatible
filetype off
set rtp+=$HOME/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

" Essential
Bundle 'vim-scripts/bufmru.vim'
Bundle 'bkad/CamelCaseMotion'
Bundle 'tpope/vim-fugitive'
Bundle 'michaeljsmith/vim-indent-object'
Bundle 'dimasg/vim-mark'
Bundle 'scrooloose/nerdcommenter'
Bundle 'scrooloose/nerdtree'
Bundle 'tpope/vim-repeat'
Bundle 'chrisbra/SudoEdit.vim'
Bundle 'tpope/vim-surround'
Bundle 'godlygeek/tabular'
Bundle 'coderifous/textobj-word-column.vim'

" Language/filetype-specific
Bundle 'kchmck/vim-coffee-script'
Bundle 'Twinside/vim-haskellConceal'
Bundle 'bitc/lushtags'
Bundle 'pangloss/vim-javascript'
Bundle 'leshill/vim-json'
Bundle 'tpope/vim-markdown'
Bundle 'techlivezheng/tagbar-phpctags'
Bundle 'spf13/PIV'
Bundle 'ehamberg/vim-cute-python'

" The Rest
Bundle 'luxflux/vim-git-inline-diff'
Bundle 'Raimondi/delimitMate'
Bundle 'AndrewRadev/splitjoin.vim'
Bundle 'majutsushi/tagbar'
Bundle 'mattn/gist-vim'
Bundle 'gregsexton/gitv'
Bundle 'msanders/snipmate.vim'
Bundle 'neh/dbext.vim'
Bundle 'neh/vim-scratch'
Bundle 'scrooloose/syntastic'
Bundle 'sjl/clam.vim'
Bundle 'sjl/gundo.vim'
Bundle 'tpope/vim-abolish'
Bundle 'tpope/vim-unimpaired'
Bundle 'vim-scripts/AnsiEsc.vim'
Bundle 'tpope/vim-sleuth'
" Bundle 'vim-scripts/easytags.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/unite.vim'

" Colors
Bundle 'rainux/vim-desert-warm-256'
Bundle 'vim-scripts/Sorcerer'
Bundle 'w0ng/vim-hybrid'
Bundle 'chriskempson/vim-tomorrow-theme'
Bundle 'nanotech/jellybeans.vim'

if v:version > 703 || (v:version == 703 && has("patch584"))
    Bundle 'Valloric/YouCompleteMe'
else
    Bundle 'Shougo/neocomplcache'
endif
