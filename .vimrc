" {{{ # dein

if &compatible
  set nocompatible
endif
" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')
  call dein#add('neomake/neomake')

  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')

  call dein#add('jiangmiao/auto-pairs')

  call dein#add('Shougo/unite.vim')
  call dein#add('Shougo/vimfiler')

  call dein#add('w0rp/ale')

  call dein#add('sjl/badwolf')
  call dein#add('vim-airline/vim-airline')

  call dein#add('lervag/vimtex')

  " toml
  call dein#add('cespare/vim-toml')

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
" }}}

" {{{ # Editor
set fenc=utf-8
set noswapfile
set autoread
set hidden
set showcmd

" visual
set number
set laststatus=2
set wildmode=list:longest
set visualbell " visualize bell

" Tab configuration
set list listchars=tab:\▸\-
set expandtab
set tabstop=2
set shiftwidth=2
set smartindent

" search configuration
set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
nmap <Esc><Esc> :nohlsearch<CR><Esc>

" brackets
set showmatch

" cursor
set whichwrap=b,s,h,l,<,>,[,],~
set number
set cursorline

" backspace
set backspace=indent,eol,start

" command mode
set wildmenu " コマンドモードの補完
set history=5000 " 保存するコマンド履歴の数

" folding comments
au FileType vim setlocal foldmethod=marker
" }}}

" {{{ # color
syntax on
colorscheme badwolf
highlight Normal ctermbg=none
let g:badwolf_darkgutter = 1
" }}}

" {{{ # VimFiler
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_ignore_pattern = '^\%(.git\|.DS_Store\)$'

noremap <C-X><C-T> :VimFiler -split -simple -winwidth=30 -toggle -no-quit<ENTER>
autocmd FileType vimfiler nmap <buffer> <CR> <Plug>(vimfiler_expand_or_edit)
" Automatically open vimfiler on start
" autocmd VimEnter * VimFiler -split -simple -winwidth=25 -toggle -no-quit
" If no files are specified, open vimfiler
autocmd VimEnter * if !argc() | VimFiler -split -simple -winwidth=25 -toggle -no-quit | endif
" }}}

" {{{ # Syntax checking
" Enable completion where available.
" This setting must be set before ALE is loaded.
let g:ale_completion_enabled = 1
" }}}

" {{{ # Auto completion
" call neomake#configure#automake('rw', 1000)
" Full config: when writing or reading a buffer, and on changes in insert and
let g:neomake_open_list = 2
" }}}

" {{{ # Snippet
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
"imap <expr><TAB>
" \ pumvisible() ? "\<C-n>" :
" \ neosnippet#expandable_or_jumpable() ?
" \    "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" For conceal markers.
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif
" }}}

" {{{ # Latex
let g:vimtex_compiler_latexmk = {
      \ 'background': 1,
      \ 'build_dir': '',
      \ 'continuous': 1,
      \ 'options': [
      \    '-pdfdvi',
      \    '-verbose',
      \    '-file-line-error',
      \    '-synctex=1',
      \    '-interaction=nonstopmode',
      \],
      \}

let g:vimtex_view_general_viewer
      \ = '/Applications/Skim.app/Contents/SharedSupport/displayline'
let g:vimtex_view_general_options = '-r @line @pdf @tex'
" }}}

" {{{ # OCaml
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
execute "helptags " . substitute(system('opam config var share'),'\n$','','''') .  "/merlin/vim/doc"
" }}}
