" {{{ # Vim Plug
let vimplug_exists=expand('~/.vim/autoload/plug.vim')
call plug#begin('~/.vim/plugged')

" Editor

Plug 'farmergreg/vim-lastplace'

Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

Plug 'jiangmiao/auto-pairs'

Plug 'Shougo/unite.vim'
Plug 'ap/vim-buftabline'

Plug 'lambdalisue/vim-fern'
Plug 'yuki-yano/fern-preview.vim'

Plug 'w0rp/ale'

" Color scheme
Plug 'flazz/vim-colorschemes'
Plug 'vim-airline/vim-airline'
Plug 'liuchengxu/space-vim-dark'
Plug 'colepeters/spacemacs-theme.vim'
Plug 'jacoborus/tender.vim'
Plug 'agreco/vim-citylights'
Plug 'morhetz/gruvbox'
Plug 'embark-theme/vim'
Plug 'saltdotac/citylights.vim'
Plug 'girishji/vimcomplete'

Plug 'frazrepo/vim-rainbow'

" skk
Plug 'tyru/eskk.vim'

" lsp
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'

" latex
" Plug 'lervag/vimtex'
Plug 'vim-latex/vim-latex'

" org-mode
Plug 'jceb/vim-orgmode'

" toml
Plug 'cespare/vim-toml'

call plug#end()

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
" set list listchars=tab:\▸\-
set expandtab
set tabstop=4
set shiftwidth=4
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
" au FileType vim setlocal foldmethod=marker foldlevel=0 foldcolumn=3

set nocompatible
filetype plugin indent on

" au FileType sh let g:sh_fold_enabled=5
" au FileType sh let g:is_bash=1
" au FileType sh set foldmethod=syntax

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

" vim bufftabline
set hidden
nnoremap <C-N> :bnext<CR>
nnoremap <C-P> :bprev<CR>

" vim-lastplace
let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"
let g:lastplace_open_folds = 0

" vim-signify
set updatetime=100

" vim-fern
let g:fern#default_hidden=1
" let g:fern#renderer = 'nerdfont'
let g:fern#renderer#nerdfont#indent_markers = 1
nnoremap <silent> <ESC>n :Fern . -drawer -toggle -reveal=%<CR>


function! s:fern_settings() abort
  nmap <silent> <buffer> p     <Plug>(fern-action-preview:toggle)
  nmap <silent> <buffer> <C-p> <Plug>(fern-action-preview:auto:toggle)
  nmap <silent> <buffer> <C-d> <Plug>(fern-action-preview:scroll:down:half)
  nmap <silent> <buffer> <C-u> <Plug>(fern-action-preview:scroll:up:half)
endfunction

augroup fern-settings
  autocmd!
  autocmd FileType fern call s:fern_settings()
augroup END

"
" }}}

" {{{ # Keybindings
" }}}

" {{{ # Color scheme
syntax on
if (has("termguicolors"))
  set termguicolors
endif
set background=dark
colorscheme badwolf  " citylights embark space-vim-dark badwolf tender

" }}}

" {{{ # Syntax checking and Completion
" Enable completion where available.
" This setting must be set before ALE is loaded.
let g:ale_completion_enabled = 1
set omnifunc=ale#completion#OmniFunc
let g:ale_completion_autoimport = 1

set omnifunc=syntaxcomplete#Complete

set completeopt=menuone,noinsert

" 補完表示時のEnterで改行をしない
inoremap <expr><CR>  pumvisible() ? "<C-y>" : "<CR>"
inoremap <expr><C-n> pumvisible() ? "<Down>" : "<C-n>"
inoremap <expr><C-p> pumvisible() ? "<Up>" : "<C-p>"

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
