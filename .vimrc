" {{{ # Vim Plug
let vimplug_exists=expand('~/.vim/autoload/plug.vim')
call plug#begin('~/.vim/plugged')

Plug 'farmergreg/vim-lastplace'

Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

Plug 'jiangmiao/auto-pairs'

Plug 'ap/vim-buftabline'

Plug 'lambdalisue/vim-fern'
Plug 'yuki-yano/fern-preview.vim'

Plug 'w0rp/ale'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'

" ALE: popup diagnostics (must be set before plug#end loads the plugin)
let g:ale_disable_lsp = 'all'
let g:ale_virtualtext_cursor = 'disabled'
let g:ale_echo_cursor = 0
let g:ale_cursor_detail = 1
let g:ale_floating_preview = 1
" ALE border order: [left, top, topleft, topright, bottomright, bottomleft, right, bottom]
let g:ale_floating_window_border = ['|', '-', '+', '+', '+', '+', '|', '-']
let g:ale_floating_preview_popup_opts = {'padding': [0, 0, 0, 0]}
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '⚠'

" vim-lsp: popup diagnostics (must be set before plug#end loads the plugin)
let g:lsp_diagnostics_virtual_text_enabled = 0
let g:lsp_diagnostics_echo_cursor = 0
let g:lsp_diagnostics_float_cursor = 1
let g:lsp_preview_float = 1
let g:lsp_hover_ui = 'float'
" Vim popup order: [top, right, bottom, left, topleft, topright, botright, botleft]
let g:lsp_popup_borderchars = ['-', '|', '-', '|', '+', '+', '+', '+']

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

Plug 'honza/vim-snippets'
Plug 'tpope/vim-surround'

Plug 'vim-skk/eskk.vim'
Plug 'vim-skk/skkdict.vim'

Plug 'vim-latex/vim-latex'

Plug 'jceb/vim-orgmode'
Plug 'cespare/vim-toml'

call plug#end()
" }}}

" {{{ # Editor
set nocompatible
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis
set fenc=utf-8
set noswapfile
set undofile
set autoread
set hidden
set showcmd
set signcolumn=number
set numberwidth=6
set scrolloff=4
set splitbelow
set splitright
set mouse=a
set confirm
set display=lastline
set nowrap

set number
set cursorline
set laststatus=2
set wildmenu
set wildmode=longest:full,full
set visualbell
set timeoutlen=300

set expandtab
set tabstop=4
set shiftwidth=4
set smartindent
set backspace=indent,eol,start
set whichwrap=b,s,h,l,<,>,[,],~
set history=5000

set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
nnoremap <silent> <Esc><Esc> :nohlsearch<CR>

set showmatch
set updatetime=100

filetype plugin indent on

autocmd BufWritePre * %s/\s\+$//e
" }}}

" {{{ # Buffers and file tree
nnoremap <silent> <C-N> :bnext<CR>
nnoremap <silent> <C-P> :bprev<CR>

let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"
let g:lastplace_open_folds = 0

let g:fern#default_hidden=1
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
" }}}

" {{{ # Color scheme
syntax on
if has('termguicolors')
  set termguicolors
endif
set background=dark
colorscheme citylights
highlight link PopupBorder Pmenu
highlight link PopupTitle PmenuSel
" }}}

" {{{ # Linting and completion (ALE)
let g:ale_completion_enabled = 1
let g:ale_completion_autoimport = 1
set omnifunc=ale#completion#OmniFunc
set completeopt=menuone,popup,noselect,noinsert

nnoremap <silent> K :ALEDetail<CR>

inoremap <expr><CR>  pumvisible() ? "\<C-y>" : "\<CR>"
inoremap <expr><C-n> pumvisible() ? "\<Down>" : "\<C-n>"
inoremap <expr><C-p> pumvisible() ? "\<Up>" : "\<C-p>"
" }}}

" {{{ # Snippets
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
xmap <C-k> <Plug>(neosnippet_expand_target)
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"

" Neosnippet uses conceal markers; keep LaTeX buffers on raw source (see below).
if has('conceal')
  set conceallevel=2 concealcursor=niv
endif
" }}}

" {{{ # LaTeX
" Show command source literally instead of rendered math/symbols (α, ∑, etc.).
let g:tex_flavor = 'latex'
let g:tex_conceal = ''

augroup dotfiles_tex
  autocmd!
  autocmd FileType tex,plaintex,bib setlocal conceallevel=0 concealcursor=
augroup END
" }}}

" {{{ # Language Server Protocol (vim-lsp)
function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=number
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  let b:ale_enabled = 0

  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [g <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]g <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
endfunction

augroup lsp_install
  autocmd!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
" }}}

" {{{ # OCaml (merlin fallback when ocaml-language-server is unavailable)
if executable('ocaml-language-server')
  " ocaml-lsp is registered by vim-lsp-settings.
elseif executable('opam')
  let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
  if isdirectory(g:opamshare . '/merlin/vim')
    execute 'set rtp+=' . g:opamshare . '/merlin/vim'
  endif
endif
" }}}

" {{{ # Input method (SKK)
let g:eskk#large_dictionary = {
\ 'path': "~/.SKK-JISYO.L",
\ 'sorted': 1,
\ 'encoding': 'euc-jp',
\}
" }}}
