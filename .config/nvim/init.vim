map f i

map j <left>
map i <up>
map k <down>
map l <right>

map ; $
map h 0

map p f<space><escape>
nnoremap o e
nnoremap u b
nnoremap r de
nnoremap e db

nnoremap v p
nnoremap y U
nnoremap <ctrl>y <ctrl>r

nnoremap <backspace> dh

let mapleader = ' '
nnoremap <leader>dk i()
nnoremap <leader>; :w<enter>

set exrc
set secure

set nocompatible
filetype plugin on

syntax on
set background=dark

" Completion
set path+=**
set wildmenu
set omnifunc=syntaxcomplete#Complete
command Maketags !ctags -R .

" Navigation {{{
set relativenumber
set number

if &t_Co > 255
	set cursorline
	set termguicolors
	let g:gruvbox_contrast_dark = 'medium'
	autocmd vimenter * ++nested colorscheme gruvbox
	colorscheme gruvbox
else
	colorscheme delek
endif
" }}}

" Cursor
let &t_SI = "\<Esc>[5 q"
let &t_SR = "\<Esc>[3 q"
let &t_EI = "\<Esc>[1 q"

" Space indentation
function Spaces(num)
	setlocal expandtab
	let &l:shiftwidth=a:num
	let &l:tabstop=a:num
	setlocal autoindent
	setlocal smartindent
endfunction

" Tab indentation
function Tabs(num)
	setlocal noexpandtab
	let &l:shiftwidth=a:num
	let &l:tabstop=a:num
	setlocal autoindent
	setlocal smartindent
endfunction
call Spaces(4)

" DelimitMate
let delimitMate_expand_cr = 1
let delimitMate_expand_space = 1

" Paren highlighting {{{
let g:rainbow_active = 1
let g:rainbow_conf = {
\	'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick'],
\	'ctermfgs': ['lightblue', 'lightyellow', 'lightcyan', 'lightmagenta'],
\	'guis': [''],
\	'cterms': [''],
\	'operators': '_,_',
\	'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/{/ end=/}/ fold'],
\	'separately': {
\		'*': {},
\		'markdown': {
\			'parentheses_options': 'containedin=markdownCode contained',
\		},
\		'lisp': {
\			'guifgs': ['royalblue3', 'darkorange3', 'seagreen3', 'firebrick', 'darkorchid3'],
\		},
\		'haskell': {
\			'parentheses': ['start=/(/ end=/)/ fold', 'start=/\[/ end=/\]/ fold', 'start=/\v\{\ze[^-]/ end=/}/ fold'],
\		},
\		'vim': {
\			'parentheses_options': 'containedin=vimFuncBody',
\		},
\		'perl': {
\			'syn_name_prefix': 'perlBlockFoldRainbow',
\		},
\		'stylus': {
\			'parentheses': ['start=/{/ end=/}/ fold contains=@colorableGroup'],
\		},
\		'pascal': {
\			'parentheses': ['start=/\[/ end=/\]/ fold', 'start=/(/ end=/)/ fold'],
\		},
\		'css': 0,
\	}
\}
" }}}

" Characters
set list
set listchars=tab:│\ ,trail:·
set fileformat=unix
set encoding=utf-8

" Statusline
set laststatus=2
set statusline=%.50F\ »\ %m%h%r%=line\ %l/%L,\ col\ %c
set showcmd

" Movement {{{
onoremap H ^
onoremap L $
" }}}

" Folds
nnoremap <space>k za
nnoremap <space>j za
set foldmethod=indent
set foldlevel=99

" Sourcing and editing vimrc
nnoremap <leader>sv :source $HOME/.config/nvim/init.vim<cr>
nnoremap <leader>ev :edit $HOME/.config/nvim/init.vim<cr>

" Split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set noswapfile

set backspace=indent,eol,start
set pastetoggle=<F2>
set lazyredraw

" Search
set hlsearch      " Highlight all search results
set incsearch     " Incrementally search
set ignorecase    " Gets overwritten by smartcase if the search
                  " contains uppercase characters
set smartcase     " Enable smart-case search
nnoremap // :noh<return>

" --- language-specific --- "

" Lisp {{{
augroup filetype_lisp
	autocmd!
	autocmd FileType lisp,scheme call Spaces(2)
	autocmd FileType lisp,scheme let b:delimitMate_quotes = "\""
augroup end
" }}}

" HTML FileType {{{
augroup filetype_html
	autocmd!
	autocmd FileType html call Spaces(2)
	autocmd FileType html nnoremap <buffer> <leader>html :-1read $HOME/.config/nvim/skeleton.html<cr>4jwf>a
	autocmd FileType html iabbrev <buffer> lorem <esc>:read $HOME/.config/nvim/lorem<cr>A
augroup END
" }}}

" Vimscript file settings {{{
augroup filetype_vim
	autocmd!
	autocmd FileType vim setlocal foldmethod=marker
	autocmd FileType vim call Tabs(4)
augroup END
" }}}

" Pascal {{{
augroup filetype_pascal
	autocmd!
	autocmd FileType pascal call Spaces(2)
augroup END
" }}}

" C++ {{{
augroup filetype_cpp
	autocmd!
	autocmd FileType cpp setlocal equalprg=clang-format\ --style=file
augroup END
" }}}

" C {{{
augroup filteype_c
	autocmd!
	autocmd BufEnter *.c,*.h call Tabs(4)
augroup END
" }}}
