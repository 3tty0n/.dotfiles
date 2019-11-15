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

  call dein#add('flazz/vim-colorschemes')
  call dein#add('vim-airline/vim-airline')

  " skk
  call dein#add('tyru/eskk.vim')

  " lsp
  call dein#add('prabirshrestha/async.vim')
  call dein#add('prabirshrestha/vim-lsp')
  call dein#add('prabirshrestha/asyncomplete.vim')
  call dein#add('prabirshrestha/asyncomplete-lsp.vim')
  call dein#add('yami-beta/asyncomplete-omni.vim')

  " latex
  call dein#add('lervag/vimtex')

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
" au FileType vim setlocal foldmethod=marker
set nocompatible
filetype plugin indent on
set foldenable
set foldmethod=marker
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
autocmd VimEnter * if !argc() | VimFiler -split -simple -winwidth=25 -toggle -no-quit | endif

" vim-lastplace
let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"
let g:lastplace_open_folds = 0

" }}}

" {{{ # Color scheme
syntax on
colorscheme badwolf
set t_Co=256
set background=dark
highlight Normal ctermbg=NONE
highlight nonText ctermbg=NONE
" }}}

" {{{ # Syntax checking
" Enable completion where available.
" This setting must be set before ALE is loaded.
let g:ale_completion_enabled = 1
" }}}

" {{{ # Auto completion

" asyncomplete
let g:asyncomplete_remove_duplicates = 1

let g:asyncomplete_smart_completion = 1
let g:asyncomplete_auto_popup = 1

set completeopt+=preview

autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

" omni completion
call asyncomplete#register_source(asyncomplete#sources#omni#get_source_options({
\ 'name': 'omni',
\ 'whitelist': ['*'],
\ 'blacklist': ['c', 'cpp', 'html'],
\ 'completor': function('asyncomplete#sources#omni#completor')
\  }))
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
" lsp
if executable('ocaml-language-server')
  au User lsp_setup call lsp#register_server({
        \ 'name': 'ocaml-language-server',
        \ 'cmd': {server_info->[&shell, &shellcmdflag, 'opam config exec -- ocaml-language-server --stdio']},
        \ 'whitelist': ['reason', 'ocaml'],
        \ })
else
  " merlin
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  execute "set rtp+=" . g:opamshare . "/merlin/vim"
endif
" }}}

" {{{ # vim.skk
let g:eskk#large_dictionary = {
\	'path': "~/.SKK-JISYO.L",
\	'sorted': 1,
\	'encoding': 'euc-jp',
\}

" }}}
