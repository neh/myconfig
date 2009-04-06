set nocompatible
set fileencodings=utf-8

if has("autocmd")

	autocmd!

	filetype plugin indent on

	" python settings
	autocmd BufNewFile,BufRead *.ptl setf python
	
	" do not make backup files for arch logs
	autocmd BufNewFile,BufRead ++log* :set nobackup

	" load mail settings when editing mail from mutt
	autocmd BufRead /tmp/mutt* :source ~/.vim/mail

	autocmd BufNewFile,BufRead *.erb setf eruby

    " filetype specific settings
	autocmd FileType ruby        setlocal sw=2 ts=2 et
	autocmd FileType eruby        setlocal sw=2 ts=2 et
    autocmd FileType css         setlocal sw=2 sts=2 et
    autocmd FileType html        setlocal sw=2 sts=2 et
    autocmd FileType java        setlocal sw=4 sts=4 et
    autocmd FileType javascript  setlocal sw=4 sts=4 et
    autocmd FileType mason       setlocal sw=2 sts=2 et
    autocmd FileType ocaml       setlocal sw=2 sts=2 et
	autocmd FileType haskell     setlocal sw=2 ts=2 et
    autocmd FileType php         setlocal sw=2 sts=2 et 
    autocmd FileType perl        setlocal sw=4 sts=4 et
    autocmd FileType python      setlocal sw=4 sts=4 et tw=72
    autocmd FileType scheme      setlocal sw=2 sts=2 et
    autocmd FileType sql         setlocal et
    autocmd FileType htmlcheetah setlocal sw=2 sts=2 et
    autocmd FileType text        setlocal sw=2 sts=2 et tw=78

    " git commit diff viewing
    au BufRead,BufNewFile COMMIT_EDITMSG     setf git

	" TODO set xterm window title. supposedly.
	"autocmd BufEnter * let &titlestring = $HOSTNAME . ":" . expand("%:p:~")

	" When editing a file, always jump to the last known cursor position.
	" Don't do it when the position is invalid or when inside an event handler
	" (happens when dropping a file on gvim).
	autocmd BufReadPost *
		\ if line("'\"") > 0 && line("'\"") <= line("$") |
		\   exe "normal g`\"" |
		\ endif

	" re-read vimrc after writing it
	autocmd BufWritePost \.vimrc :source $HOME/.vimrc
    " Useful when customizing xterm
	autocmd BufWritePost \.Xdefaults :!xrdb ~/.Xdefaults
    "if has("gui_running")
        "autocmd BufWritePost \.vimrc :source $HOME/.gvimrc
        "autocmd BufWritePost \.gvimrc :source $HOME/.gvimrc
    "endif

"    augroup filetypedetect
"          autocmd! BufNewFile, BufRead

    augroup VCSCommand
        au User VCSBufferCreated silent! nmap <unique> <buffer> q: bwipeout<cr>
    augroup END

endif " has("autocmd")


let VCSCommandEnableBufferSetup=1
let mapleader = ","
syntax on					" always want syntax highlighting
set autoindent				" always set autoindenting on
set history=50				" keep 50 lines of command line history
set ruler					" show the cursor position all the time
set showcmd					" display incomplete commands
set incsearch				" do incremental searching
set hlsearch				" highlight searches
set startofline			" don't move cursor to start of line
set showmatch				" show matching brackets
set backspace=indent,eol,start
set title
set titlestring=%<%{hostname()}:%F\ %(%m\ %)[%l/%L\ %P]\ %y\ VIM
set laststatus=2
set statusline=%F%m%r%h%w\ [%l,%v\ %p%%\ %L]\ [%{&ff}]

function! GitStatusLine()
    let gitinfo=VCSCommandGetStatusLine()
    let dirty=system("git status 2> /dev/null | tail -n1")
    if dirty =~ 'nothing to commit*'
        return l:gitinfo
    else
        return "%#ErrorMsg#gitinfo#StatusLine#"
    endif
endfunction

set tabstop=4
set shiftwidth=4
set expandtab

set visualbell t_vb= 		" no bells for me

set formatoptions+=tcroqnw

let loaded_vimspell = 1

" Some NERDTree mappings for dvorak
let g:NERDTreeMapOpenInTab="A"
let g:NERDTreeMapOpenInTabSilent="a"
let g:NERDTreeHighlightCursorline=1

" TODO need a better key for this
map <silent> <F3> :call BufferList()<CR>

" keyword completion for perl
"set iskeyword+=:
" keyword completion for python (and ruby?)
"set iskeyword+=.

set background=dark
if has("gui_running")
    colo mint
    set guioptions=aAim    " don't want a toolbar or menu
    set guifont=Bitstream\ Vera\ Sans\ Mono\ 14
    set mousehide
endif

if $TERM =~ '^screen-bce'
        set t_Co=256 
        colo gardener
elseif $TERM =~ '^rxvt-256'
        set t_Co=256 
        colo gardener
elseif $TERM =~ '^rxvt'
        set t_Co=88
        colo inkpot
elseif $TERM =~ '^linux'
        set t_Co=8
else
        set t_Co=16
endif

let g:no_html_toolbar = 1
let g:html_tag_case = 'lower'
let g:html_template = '$HOME/.vim/html_template'
let g:html_authorname = 'Nathan Howell'
let g:html_authoremail = 'nath@nhowell.net'

" A few dvorak adjustments
noremap s l
noremap S L
noremap t gj
noremap T }
noremap n gk
noremap N {
noremap l n
noremap L N
noremap j J

runtime ftplugin/man.vim


" Mappings
map <C-J> <C-W>j<C-W>_     " move down one window and maximize
map <C-K> <C-W>k<C-W>_     " move up one window and maximize
map <C-H> <C-W>h           " move left one window
map <C-L> <C-W>l           " move right one window
map <C-M-J> <C-W>-         " shrink a window vertically
map <C-M-K> <C-W>+         " grow a window vertically
map <C-M-H> <C-W><         " shrink a window horizontally
map <C-M-L> <C-W>>         " grow a window horizontally
nnoremap <silent> <Leader>tl :Tlist<CR>		" open taglist window
map <Leader>h :set hls!<bar>set hls?<CR>	" toggle hlsearch
map <Leader>fc :silent ToggleFoldColumn<CR> " toggle foldcolumn
map <Leader>pt :set paste!<bar>set paste?<CR>	" toggle paste
set pastetoggle=<f11>
map <F10> :NERDTreeToggle<CR>

:nmap <C-o> <PageUp>
:nmap <C-e> <PageDown>
:map <C-o> <PageUp>
:map <C-e> <PageDown>
:imap <C-o> <PageUp>
:imap <C-e> <PageDown>

:nmap <C-n> :tabprevious<cr>
:nmap <C-t> :tabnext<cr>
:map <C-n> :tabprevious<cr>
:map <C-t> :tabnext<cr>
:imap <C-n> <ESC>:tabprevious<cr>i
:imap <C-t> <ESC>:tabnext<cr>i
":nmap <C-t> :tabnew<cr>
":imap <C-t> <ESC>:tabnew<cr> 
nmap <tab> :bn<cr>
nmap <s-tab> :bp<cr>

" TODO: set up a keymapping for par
map <Leader>par :!par<CR>

" Variables
let Tlist_Ctags_Cmd = "/usr/bin/exuberant-ctags"

" toggle foldcolumn
function! ToggleFoldColumn()
	if &foldcolumn == 0
		let &foldcolumn = 2
	elseif &foldcolumn > 0
		let &foldcolumn = 0
	endif
endfunction
command! ToggleFoldColumn call ToggleFoldColumn()

" DnD saving for Rox
function! Save()
	let tmpname = tempname()
	let fname = expand('%')
	if fname == ''
		let fname = 'TextFile'
	endif
	exec 'write !savebox ' . fname . ' > ' . tmpname
	let newname = system('cat ' . tmpname)
	let tmp = system('rm ' . tmpname)
	if tmpname != ''
		exec 'file ' . escape(newname, ' ')
		set nomodified
	endif
endfunction
command! Save call Save()
map <Leader>rs :silent Save<CR>				" DnD Rox savebox


" Google code search in vim
" via http://www.jukie.net/~bart/blog/codesearch-from-vim
function! OnlineDoc()
    let s:browser = "/home/nathan/ff3/firefox/firefox"
    let s:wordUnderCursor = expand("<cword>")

    if &ft == "cpp" || &ft == "c" || &ft == "ruby" || &ft == "php" || &ft == "python"
        let s:url = "http://www.google.com/codesearch?q=".s:wordUnderCursor."+lang:".&ft
    elseif &ft == "vim"
        let s:url = "http://www.google.com/codesearch?q=".s:wordUnderCursor
    else
        return
    endif

    let s:cmd = "silent !" . s:browser . " \"" . s:url . "\""
    execute  s:cmd
    redraw!
endfunction

" online doc search
map <Leader>k :call OnlineDoc()<CR>


" search for a pattern in the current git branch
function! GitGrep(...)
  let save = &grepprg
  set grepprg=git\ grep\ -n\ $*
  let s = 'grep'
  for i in a:000
    let s = s . ' ' . i
  endfor
  exe s
  let &grepprg = save
endfun
command! -nargs=? G call GitGrep(<f-args>)
" run GitGrep on the word under the cursor
function! GitGrepWord()
  normal! "zyiw
  call GitGrep('-w -e ', getreg('z'))
endf
nmap <C-x>G :call GitGrepWord()<CR>
