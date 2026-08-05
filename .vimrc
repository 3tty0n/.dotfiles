" {{{ # Vim Plug
let s:vimplug = expand('~/.vim/autoload/plug.vim')
if empty(glob(s:vimplug))
  silent execute '!curl -fLo ' . s:vimplug . ' --create-dirs '
        \ . 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" citylights targets Neovim ('None', TermOpen); normalize for Vim
function! s:fix_citylights(...) abort
  let l:file = expand('~/.vim/plugged/citylights.vim/colors/citylights.vim')
  if !filereadable(l:file)
    return
  endif
  let l:text = join(readfile(l:file), "\n")
  let l:text = substitute(l:text, '=\zsNone\>', 'NONE', 'g')
  let l:text = substitute(l:text, 'guifg=#fff\>', 'guifg=#ffffff', 'g')
  let l:text = substitute(l:text,
        \ '\n\zsautocmd TermOpen \* setlocal winhighlight=Normal:Terminal\ze\n',
        \ "if has(\"nvim\")\n  autocmd TermOpen * setlocal winhighlight=Normal:Terminal\nendif", '')
  call writefile(split(l:text, "\n", 1), l:file)
endfunction

call plug#begin('~/.vim/plugged')

Plug 'farmergreg/vim-lastplace'

Plug 'Shougo/neosnippet.vim'
Plug 'Shougo/neosnippet-snippets'

Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-rsi'

Plug 'ap/vim-buftabline'

Plug 'lambdalisue/vim-fern'
Plug 'yuki-yano/fern-preview.vim'

" Lint + LSP (ALE LSP disabled; vim-lsp owns language servers)
Plug 'dense-analysis/ale'
Plug 'prabirshrestha/vim-lsp'
Plug 'mattn/vim-lsp-settings'

" ALE: popup diagnostics (must be set before plug#end loads the plugin)
let g:ale_disable_lsp = 'all'
let g:ale_completion_enabled = 0
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
" Don't open diagnostic floats while typing — mid-edit syntax errors (esp. Python)
" would otherwise steal focus and interrupt insert mode.
let g:lsp_diagnostics_float_insert_mode_enabled = 0
let g:lsp_preview_float = 1
let g:lsp_hover_ui = 'float'
" Vim popup order: [top, right, bottom, left, topleft, topright, botright, botleft]
let g:lsp_popup_borderchars = ['-', '|', '-', '|', '+', '+', '+', '+']

Plug 'vim-airline/vim-airline'
Plug 'saltdotac/citylights.vim', { 'do': { -> s:fix_citylights() } }

Plug 'frazrepo/vim-rainbow'
Plug 'airblade/vim-gitgutter'

" Fuzzy find (expects `fzf` on PATH, e.g. ~/.zsh/plugins/fzf/bin)
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

Plug 'vim-skk/eskk.vim'
Plug 'vim-skk/skkdict.vim'

Plug 'vim-latex/vim-latex'
Plug 'jceb/vim-orgmode'

call plug#end()

" Built-in Vim 9 packages (no plug install needed)
silent! packadd comment
silent! packadd matchit
silent! packadd editorconfig
silent! packadd hlyank
" }}}

" {{{ # Editor
set nocompatible
set encoding=utf-8
set fileencodings=utf-8,iso-2022-jp,euc-jp,sjis
set fenc=utf-8

let mapleader = "\<Space>"
let maplocalleader = "\\"

" Persistent undo / no clutter in cwd
set noswapfile
set nobackup
set nowritebackup
set undofile
if !isdirectory(expand('~/.vim/undo'))
  call mkdir(expand('~/.vim/undo'), 'p', 0700)
endif
set undodir=~/.vim/undo

set autoread
set hidden
set showcmd
set signcolumn=yes
set numberwidth=4
set scrolloff=5
set sidescrolloff=8
set splitbelow
set splitright
set mouse=a
if has('mouse_sgr')
  set ttymouse=sgr
endif
set confirm
set display=lastline
set nowrap
set linebreak
set breakindent
set smoothscroll

set number
set cursorline
set laststatus=2
set wildmenu
" noselect/lastused need a newer 9.1 than Debian's current package
try
  set wildmode=noselect:lastused,full
catch /^Vim\%((\a\+)\)\=:E474:/
endtry
set wildoptions=pum,fuzzy
set pumheight=12
set visualbell
set t_vb=
set timeoutlen=400
set ttimeoutlen=50
set shortmess+=cC
set belloff=all

set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set smartindent
set backspace=indent,eol,start
set whichwrap=b,s,h,l,<,>,[,],~
set history=10000
set sessionoptions-=options
set viewoptions-=options

set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
nnoremap <silent> <Esc><Esc> :nohlsearch<CR>

set showmatch
set matchtime=1
set updatetime=100
" Apple's Normal vim lacks internal xdiff; ignore if unsupported
try
  set diffopt+=internal,algorithm:histogram,indent-heuristic
catch /^Vim\%((\a\+)\)\=:E474:/
endtry

set list
set listchars=tab:»\ ,trail:·,extends:›,precedes:‹,nbsp:␣

" Native insert-mode autocomplete (Vim 9.1+); o = omnifunc (LSP when attached)
set autocomplete
set complete=.^5,w^5,b^5,u^5,o^10
" Native insert-mode autocomplete (Vim 9.1+).
" Do not include 'o' (omnifunc): vim-lsp's lsp#complete calls complete() via a
" timer/wait, which hits E565 under autocomplete's textlock (common while a
" Python file is mid-edit / syntactically invalid). Use <C-x><C-o> for LSP.
set autocomplete
set complete=.^5,w^5,b^5,u^5
set completeopt=menuone,popup,noselect,noinsert
set infercase

filetype plugin indent on
syntax enable

function! s:trim_trailing_whitespace() abort
  if &binary || &filetype =~# 'diff\|mail'
    return
  endif
  let l:view = winsaveview()
  keeppatterns %s/\s\+$//e
  call winrestview(l:view)
endfunction

augroup dotfiles_editor
  autocmd!
  autocmd BufWritePre * call s:trim_trailing_whitespace()
  " Command-line / search popup completion (wildtrigger is newer 9.1)
  if exists('*wildtrigger')
    autocmd CmdlineChanged [:\/\?] call wildtrigger()
  endif
  autocmd CmdlineEnter [\/\?] set pumheight=8
  autocmd CmdlineLeave [\/\?] set pumheight=12
augroup END

" Keep history keys working while wildmenu is open
cnoremap <expr> <Up>   wildmenumode() ? "\<C-E>\<Up>"   : "\<Up>"
cnoremap <expr> <Down> wildmenumode() ? "\<C-E>\<Down>" : "\<Down>"

" Completion popup navigation (Tab/S-Tab; C-n/C-p stay Emacs line motion)
inoremap <silent><expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"
" }}}

" {{{ # Buffers, files, and fuzzy find
nnoremap <silent> <C-N> :bnext<CR>
nnoremap <silent> <C-P> :bprev<CR>
nnoremap <silent> <leader>bd :bdelete<CR>

let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_ignore_buftype = "quickfix,nofile,help"
let g:lastplace_open_folds = 0

let g:fern#default_hidden = 1
nnoremap <silent> <leader>e :Fern . -drawer -toggle -reveal=%<CR>
nnoremap <silent> <Esc>n :Fern . -drawer -toggle -reveal=%<CR>

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

nnoremap <silent> <leader>f :Files<CR>
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <leader>/ :Rg<CR>
nnoremap <silent> <leader>g :GFiles?<CR>
" }}}

" {{{ # Color scheme
if has('termguicolors')
  set termguicolors
endif
set background=dark
call s:fix_citylights()
colorscheme citylights
highlight link PopupBorder Pmenu
highlight link PopupTitle PmenuSel

let g:airline_powerline_fonts = 0
let g:airline#extensions#tabline#enabled = 0
let g:gitgutter_sign_added = '+'
let g:gitgutter_sign_modified = '~'
let g:gitgutter_sign_removed = '_'
" }}}

" {{{ # Emacs / readline (insert + cmdline)
" C-a/C-e/C-b/C-d and Meta word keys come from vim-rsi; these fill the rest.
inoremap <C-f> <Right>
cnoremap <C-f> <Right>
inoremap <C-n> <Down>
cnoremap <C-n> <Down>
inoremap <C-p> <Up>
cnoremap <C-p> <Up>
inoremap <C-j> <CR>
" Kill to end of line (at EOL in insert: join with next line)
inoremap <expr> <C-k> col('.') == col('$') ? "\<C-o>gJ" : "\<C-o>D"
cnoremap <C-k> <C-\>e strpart(getcmdline(), 0, getcmdpos() - 1)<CR>
" }}}

" {{{ # Snippets
" Tab expands/jumps when a snippet is available; otherwise normal Tab / pum next.
imap <expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)" : "\<Tab>"
smap <expr> <Tab> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)" : "\<Tab>"
xmap <Tab> <Plug>(neosnippet_expand_target)

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
  setlocal signcolumn=yes
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
  nmap <buffer> <leader>ca <plug>(lsp-code-action)
  nmap <buffer> <leader>lf <plug>(lsp-document-format)
endfunction

augroup lsp_install
  autocmd!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" Fallback detail when LSP is not attached
nnoremap <silent> <leader>K :ALEDetail<CR>
" }}}

" {{{ # OCaml (merlin fallback when ocaml-language-server is unavailable)
if executable('ocaml-language-server')
  " ocaml-lsp is registered by vim-lsp-settings.
elseif executable('opam')
  let g:opamshare = substitute(system('opam var share 2>/dev/null'), '\n$', '', '')
  if empty(g:opamshare)
    let g:opamshare = substitute(system('opam config var share'), '\n$', '', '')
  endif
  if isdirectory(g:opamshare . '/merlin/vim')
    execute 'set rtp+=' . fnameescape(g:opamshare . '/merlin/vim')
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
