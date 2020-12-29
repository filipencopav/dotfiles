set nocompatible
filetype plugin on

syntax on
if &t_Co > 255
	colorscheme base16-gruvbox-dark-medium
endif

" Completion
set path+=**
set wildmenu
set omnifunc=syntaxcomplete#Complete
command Maketags !ctags -R .

" Navigation {{{
set relativenumber
if &t_Co >  255
	set cursorline
endif
" }}}

" Cursor
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

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
call Tabs(4)

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
nnoremap j gj
nnoremap k gk
nnoremap J <c-d>
nnoremap K <c-u>
nnoremap H ^
nnoremap L $

onoremap H ^
onoremap L $
" }}}

" Folds
nnoremap <space>k za
nnoremap <space>j za
set foldmethod=indent
set foldlevel=99

" Sourcing and editing vimrc
let mapleader = ','
nnoremap <leader>sv :source $HOME/.vimrc<cr>
nnoremap <leader>ev :edit $HOME/.config/vimrc<cr>

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
augroup end
" }}}

" HTML FileType {{{
augroup filetype_html
	autocmd!
	autocmd FileType html call SetTabOptions()
	autocmd FileType html nnoremap <buffer> <leader>html :-1read $HOME/.vim/skeleton.html<cr>4jwf>a
	autocmd FileType html iabbrev <buffer> lorem Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce quis laoreet ipsum. Donec mattis convallis tellus in fermentum. Fusce gravida odio ac nibh tincidunt, at mollis nunc porttitor. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Vestibulum bibendum nunc at odio elementum dapibus. Aliquam maximus ipsum sem, quis sollicitudin elit imperdiet sed. Duis condimentum bibendum odio, scelerisque tincidunt orci sollicitudin quis. Maecenas mollis elit quis lorem fringilla, at egestas erat vestibulum. Nullam non odio sit amet turpis iaculis molestie ac sit amet sapien. Maecenas orci odio, imperdiet eu convallis ultricies, posuere vel arcu. Phasellus auctor hendrerit purus at semper. Fusce ultricies malesuada magna, sit amet fringilla nisl aliquet ut. Vestibulum ac sollicitudin sapien, et lobortis tellus.
augroup END
" }}}

" Vimscript file settings {{{
augroup filetype_vim
	autocmd!
	autocmd FileType vim setlocal foldmethod=marker
	autocmd FileType vim call Tabs(4)
augroup END
" }}}

" Rust filetype settings {{{
augroup rust_filetype
	autocmd!
	autocmd FileType rust call Tabs(4)
augroup END
" }}}

" Pascal filetype settings {{{
augroup pascal_filetype
	autocmd!
	autocmd FileType pascal call Spaces(2)
augroup END
" }}}

" C filetype {{{
augroup c_filetype
	autocmd!
	autocmd FileType c call Tabs(4)
augroup END
" }}}

" Fennel {{{
augroup fennel_filetype
	autocmd!
	autocmd FileType fennel call Spaces(2)
augroup END
" }}}
