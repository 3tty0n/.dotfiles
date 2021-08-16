" {{{ # dein

if has('vim_starting')
  set nocompatible               " Be iMproved
endif

if &compatible
  set nocompatible
endif

" Add the dein installation directory into runtimepath
set runtimepath+=~/.cache/dein/repos/github.com/Shougo/dein.vim

let dein_exists=expand('~/.cache/dein')

if !isdirectory(dein_exists)
  echo "Installing dein.vim..."
  echo ""
  silent !\curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh | bash -s ~/.cache/dein
endif

if dein#load_state('~/.cache/dein')
  call dein#begin('~/.cache/dein')

  call dein#add('~/.cache/dein/repos/github.com/Shougo/dein.vim')

  call dein#add('farmergreg/vim-lastplace')

  call dein#add('Shougo/neosnippet.vim')
  call dein#add('Shougo/neosnippet-snippets')

  call dein#add('jiangmiao/auto-pairs')

  call dein#add('Shougo/unite.vim')
  call dein#add('Shougo/vimfiler')

  call dein#add('w0rp/ale')

  " color scheme
  call dein#add('flazz/vim-colorschemes')
  call dein#add('vim-airline/vim-airline')
  call dein#add('liuchengxu/space-vim-dark')
  call dein#add('jacoborus/tender.vim')
  call dein#add('agreco/vim-citylights')
  call dein#add('morhetz/gruvbox')

  call dein#add('frazrepo/vim-rainbow')
  call dein#add('airblade/vim-gitgutter')

  " skk
  call dein#add('tyru/eskk.vim')

  " lsp
  call dein#add('prabirshrestha/vim-lsp')
  call dein#add('mattn/vim-lsp-settings')

  call dein#add('Shougo/deoplete.nvim')
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif
let g:deoplete#enable_at_startup = 1
  call dein#add('lighttiger2505/deoplete-vim-lsp')

  " latex
  " call dein#add('lervag/vimtex')
  call dein#add('vim-latex/vim-latex')

  " org-mode
  call dein#add('jceb/vim-orgmode')

  " toml
  call dein#add('cespare/vim-toml')

  call dein#end()
  call dein#save_state()
endif

if dein#check_install()
  call dein#install()
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
au FileType vim setlocal foldmethod=marker foldlevel=0 foldcolumn=3

set nocompatible
filetype plugin indent on

au FileType sh let g:sh_fold_enabled=5
au FileType sh let g:is_bash=1
au FileType sh set foldmethod=syntax
syntax enable

" remove unused whitespaces automatically
autocmd BufWritePre * %s/\s\+$//e

" VimFiler
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_ignore_pattern = '^\%(.git\|.DS_Store\)$'

noremap <C-X><C-T> :VimFiler -split -simple -winwidth=30 -toggle -no-quit<ENTER>
autocmd FileType vimfiler nmap <buffer> <CR> <Plug>(vimfiler_expand_or_edit)
" Automatically open vimfiler on start
" autocmd VimEnter * VimFiler -split -simple -winwidth=25 -toggle -no-quit
" If no files are specified, open vimfiler
" autocmd VimEnter * if !argc() | VimFiler -split -simple -winwidth=25 -toggle -no-quit | endif

" vim-lastplace
let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"
let g:lastplace_open_folds = 0

" }}}

" {{{ # Keybindings
" }}}

" {{{ # Color scheme
syntax on
if (has("termguicolors"))
  set termguicolors
endif
set background=dark
colorscheme space-vim-dark  "space-vim-dark badwolf tender citylights

" }}}

" {{{ # Syntax checking
" Enable completion where available.
" This setting must be set before ALE is loaded.
let g:ale_completion_enabled = 1
set omnifunc=ale#completion#OmniFunc
let g:ale_completion_autoimport = 1
" }}}

" {{{ # Snippets
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

" {{{ # Configurations for programming language


" LaTeX
let g:vimtex_compiler_latexmk = {
      \ 'background': 1,
      \ 'build_dir': '',
      \ 'continuous': 1,
      \ 'options': [
      \    '-pdf',
      \    '-verbose',
      \    '-file-line-error',
      \    '-synctex=1',
      \    '-interaction=nonstopmode',
      \],
      \}

let g:vimtex_view_general_viewer = 'xdg-open'
let g:vimtex_view_general_options = '-r @line @pdf @tex'
let g:tex_flavor = 'latex'
" }}}

" {{{ # LSP

" OCaml-lsp
if executable('ocaml-language-server')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'ocaml-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'opam config exec -- ocaml-language-server --stdio']},
        \ 'allowlist': ['reason', 'ocaml'],
        \ })
else
  " merlin
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  execute "set rtp+=" . g:opamshare . "/merlin/vim"
endif

"
" }}}

" {{{ # Input/Output method
" vim.skk
let g:eskk#large_dictionary = {
\	'path': "~/.SKK-JISYO.L",
\	'sorted': 1,
\	'encoding': 'euc-jp',
\}
" }}}

" {{{ # Magic comments
" vim: filetype=vim foldmethod=marker
" }}}
