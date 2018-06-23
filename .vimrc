" {{{ # vim plug
call plug#begin('~/.vim/plugged')

Plug 'junegunn/vim-easy-align'
Plug 'itchyny/lightline.vim'

Plug 'scrooloose/nerdtree'

Plug 'thinca/vim-quickrun'
Plug 'tomtom/tcomment_vim'
Plug 'Shougo/vimproc', { 'do' : 'make' }
Plug 'vim-scripts/vim-auto-save'

" auto close
Plug 'cohama/lexima.vim'

" neocomplete and neosnippet
Plug 'Shougo/neocomplcache'
Plug 'Shougo/neosnippet'
Plug 'Shougo/neosnippet-snippets'
Plug 'Shougo/neocomplete'

" syntex check
Plug 'w0rp/ale'
" Plug 'scrooloose/syntastic'

" search
Plug 'wsdjeg/FlyGrep.vim'

" fzf
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" git
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'

" color theme
Plug 'tomasr/molokai'

" Unite.vim
Plug 'Shougo/unite.vim'
Plug 'Shougo/neomru.vim'

" vim filer
Plug 'Shougo/vimfiler.vim'

" vim copy as rtf
Plug 'zerowidth/vim-copy-as-rtf'

"=== language ===

" ocaml
Plug 'def-lkb/ocp-indent-vim'

" scala
Plug 'derekwyatt/vim-scala'
Plug 'ensime/ensime-vim', { 'do': ':UpdateRemotePlugins' }
Plug '$HOME/src/github.com/google/ijaas/vim'

" python
Plug 'davidhalter/jedi-vim'

" go
Plug 'fatih/vim-go'
Plug 'nsf/gocode', { 'rtp': 'vim', 'do': '~/.vim/plugged/gocode/vim/symlink.sh' }

" racket
Plug 'wlangstroth/vim-racket'

" markdown
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" Initialize plugin system
call plug#end()
" }}}

" {{{ # colortheme
syntax on
colorscheme molokai
highlight Normal ctermbg=none
" }}}

" {{{ # general settings
" =============================================================================
syntax on
set nocompatible            " use Vim in more useful way"
set clipboard+=unnamed      " share clipboard with other systems
set clipboard+=autoselect
let mapleader=","           " Lead with <Space>
nnoremap <space>v :<C-u>edit $VIM_ROOT/vimrc<CR>   " vimファイルを開く
nnoremap <space>s :<C-u>source $VIM_ROOT/vimrc<CR>:source $VIM_ROOT/gvimrc<CR>     " vimファイルを反映する
nnoremap <space>b :<C-u>edit $VIM_ROOT/vim/bundle_list.vim<CR> " bundle list ファイルを開く
"" Character encoding
set encoding=utf-8          " Use utf-8
set termencoding=utf-8      " ..
set fileencodings=utf-8     " ..
" Automatic end-of-file format detection
set fileformats=unix,mac,dos

" ファイルを開いたとき、最後にカーソルがあった場所に移動する
augroup vimrcEx
  au BufRead * if line("'\"") > 0 && line("'\"") <= line("$") |
  \ exe "normal g`\"" | endif
augroup END

" vim airline
set timeout timeoutlen=50
" }}}
" {{{ # text editting
" =============================================================================

set autoindent              " 改行時に自動インデント
set smartindent             " {}などを入力時に同じ行にインデント
set wrap                    " テキストを改行して表示

" タブ関連
set tabstop=2              " Tabが対応する空白の数 
set expandtab               " Tabをスペースに変換
set nosmarttab              " fuck tabs
set softtabstop=2           " Tab入力時の表示幅
set shiftwidth=2
set shiftround              " インデントをshiftwidthの倍数に丸める

set infercase               " 補完時に大文字小文字を区別しない
set formatoptions+=n        " テキスト整形時に番号付きリストをサポート
set wrapmargin=0
set virtualedit=block       " allow virtual edit in visual block ..
set listchars=tab:▸\ ,eol:¬,extends:»,precedes:«,nbsp:%
set nolist
set ambiwidth=double        " 全角文字をASCIIの2倍の幅で表示する

set textwidth=0             " 改行が入らないようにする
if v:version >= 703
  set colorcolumn=80        " 80字の部分でラインが表示されるように
endif
set foldmethod=marker       " マーカーで折りたたみを行えるように

" }}}

" {{{ # UI
" =============================================================================

set ruler                   " カーソル位置を表示する
" 移動中はカーソルラインを表示しないように
augroup vimrc-auto-cursorline
  autocmd!
  autocmd CursorMoved,CursorMovedI * call s:auto_cursorline('CursorMoved')
  autocmd CursorHold,CursorHoldI * call s:auto_cursorline('CursorHold')
  autocmd WinEnter * call s:auto_cursorline('WinEnter')
  autocmd WinLeave * call s:auto_cursorline('WinLeave')

  let s:cursorline_lock = 0
  function! s:auto_cursorline(event)
    if a:event ==# 'WinEnter'
      setlocal cursorline
      let s:cursorline_lock = 2
    elseif a:event ==# 'WinLeave'
      setlocal nocursorline
    elseif a:event ==# 'CursorMoved'
      if s:cursorline_lock
        if 1 < s:cursorline_lock
          let s:cursorline_lock = 1
        else
          setlocal nocursorline
          let s:cursorline_lock = 0
        endif
      endif
    elseif a:event ==# 'CursorHold'
      setlocal cursorline
      let s:cursorline_lock = 1
    endif
  endfunction
augroup END

set showcmd                     " コマンドの一部を画面下に表示
set number                      " 行番号の表示
set nolazyredraw                " don't redraw while executing macros
set wildmenu                    " turn on wild menu
set wildmode=list:longest,full
set cmdheight=1                 " コマンドライン行を1行に
" カーソルを左右させるキーのうち、ここで指定したものでは、
" カーソルが行頭／末にあるときに前／次行に移動できるようになる。
set whichwrap=b,s,h,l,<,>,[,]
" インサートモードですべて消す
set backspace=indent,eol,start
set shortmess=filtoOA           " shorten messages
set report=0                    " tell us about changes
set nostartofline               " don't jump to the start of line when scrolling
set showmatch                   " brackets/braces that is
set matchtime=3                 " duration to show matching brace (1/10 sec)
set laststatus=2                " The last window always have status line
set scrolloff=5             " Keep at least 5 lines above and below the cursor
set visualbell t_vb=        " No beep sound
" Treat octal and hexadecimal number as decimal number
" octal  Numbers that start with a zero will be considered to be octal
"        Example: Using CTRL-A on "007" results in "010"
" hex    Numbers starting with "0x" or "0X" will be considered to be hexadecimal
"        Example: Using CTRL-X on "0x100" results in "0x0ff"
set nrformats-=octal,hex,alpha
if has("mouse") " Enable the use of the mouse in all modes
  set mouse=a
endif

" フォーカスが移った場合に常にredraw
augroup Redraw
    autocmd!
    autocmd FocusGained * redraw!
augroup END

" tmux を使っている時で、インサートモードの時に形状を変更する
let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
" }}}

" {{{ # window settings
" =============================================================================
" 横分割時は下へ､ 縦分割時は右へ新しいウィンドウが開くようにする
set splitbelow
set splitright
" }}}

" {{{ # hightlight settings
" =============================================================================
syntax on

" 行末のスペースと全角文字をハイライト
if has('syntax')
  function! s:match_illegal_space()
    if !(exists("w:trailing_space"))
      highlight ZenkakuSpace ctermbg=green guibg=SeaGreen
      highlight WhiteSpaceEOL term=underline ctermbg=red gui=undercurl guisp=red
      let w:trailing_space = matchadd("WhiteSpaceEOL", '\s\+$')
      let w:zenkaku_space = matchadd("ZenkakuSpace", '　')
    endif
  endfunction
  function! s:clear_highlight_if()
    if &filetype ==# 'help' || &filetype ==# 'vimshell' ||
          \ &filetype ==# 'unite' || &filetype ==# 'vimfiler' ||
          \ &filetype ==# 'startify' || &filetype ==# ''
      call clearmatches()
    endif
  endfunction
  augroup HighlightInvisibleChars
    autocmd!
    autocmd VimEnter,BufEnter * call s:match_illegal_space()
    autocmd FileType help,vimshell,unite,vimfiler,startify call clearmatches()
    autocmd VimEnter,BufEnter * call s:clear_highlight_if()
  augroup END
endif
" }}}

" {{{ # backup and swap file
set nobackup
set noswapfile
" }}}

" {{{ # ocaml

" merlin
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute 'set rtp+=' . g:opamshare . '/merlin/vim'

" ocp-indent
execute 'set rtp^=' . g:opamshare . '/ocp-indent/vim'
function! s:ocaml_format()
    let now_line = line('.')
    exec ':%! ocp-indent'
    exec ':' . now_line
endfunction

augroup ocaml_format
    autocmd!
    autocmd BufWrite,FileWritePre,FileAppendPre *.mli\= call s:ocaml_format()
augroup END
" }}}

" {{{ # python
" let g:jedi#use_tabs_not_buffers = 1
" }}}

" {{{ # racket
let g:syntastic_enable_racket_racket_checker = 1
" }}}

" {{{ # syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

hi SyntasticErrorSign ctermfg=160
hi SyntasticWarningSign ctermfg=220

let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
let g:syntastic_mode_map = {'mode': 'active'}

" syntastic ocaml
let g:syntastic_ocaml_checkers = ['merlin']
let g:syntastic_ocaml_use_ocamlc = 1

let g:syntastic_error_symbol = "✗"
" }}}

" # quickrun {{{
let g:quickrun_config = {}

" vim proc でコマンドを実行する
let g:quickrun_config['_'] = {
      \ 'runner': 'vimproc',
      \ 'runner/vimproc/updatetime' : 100
      \ }
" }}}

" {{{ # neocomplete with cache
"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Enable heavy features.
" Use camel case completion.
"let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
"let g:neocomplcache_enable_underbar_completion = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplcache#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()
" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? neocomplcache#close_popup() : "\<Space>"

" For cursor moving in insert mode(Not recommended)
"inoremap <expr><Left>  neocomplcache#close_popup() . "\<Left>"
"inoremap <expr><Right> neocomplcache#close_popup() . "\<Right>"
"inoremap <expr><Up>    neocomplcache#close_popup() . "\<Up>"
"inoremap <expr><Down>  neocomplcache#close_popup() . "\<Down>"
" Or set this.
"let g:neocomplcache_enable_cursor_hold_i = 1
" Or set this.
"let g:neocomplcache_enable_insert_char_pre = 1

" AutoComplPop like behavior.
"let g:neocomplcache_enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplcache_enable_auto_select = 1
"let g:neocomplcache_disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplcache_force_omni_patterns')
  let g:neocomplcache_force_omni_patterns = {}
endif
let g:neocomplcache_force_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_force_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_force_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplcache_force_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'
" }}}

" {{{ # neosnippet
" Plugin key-mappings.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets behavior.
" Note: It must be "imap" and "smap".  It uses <Plug> mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
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

" {{{ # fzf settings
" This is the default extra key bindings
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" Default fzf layout
" - down / up / left / right
let g:fzf_layout = { 'down': '~40%' }

" In Neovim, you can set up fzf window using a Vim command
let g:fzf_layout = { 'window': 'enew' }
let g:fzf_layout = { 'window': '-tabnew' }

" Customize fzf colors to match your color scheme
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }

" Enable per-command history.
" CTRL-N and CTRL-P will be automatically bound to next-history and
" previous-history instead of down and up. If you don't like the change,
" explicitly bind the keys to down and up in your $FZF_DEFAULT_OPTS.
let g:fzf_history_dir = '~/.local/share/fzf-history'
" }}}

" {{{ # nerdtree settings
" 不可視ファイルを表示する
let NERDTreeShowHidden = 1

" ツリーと編集領域を移動する
nmap <Leader><Tab> <C-w>w
" }}}

" {{{ # scala
autocmd BufWritePost *.scala silent :EnTypeCheck
nnoremap <localleader>t :EnType<CR>
" }}}

" {{{ # unite settings
let g:unite_enable_start_insert=1
let g:unite_source_history_yank_enable =1
let g:unite_source_file_mru_limit = 200
nnoremap <silent> ,uy :<C-u>Unite history/yank<CR>
nnoremap <silent> ,ub :<C-u>Unite buffer<CR>
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file<CR>
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register<CR>
nnoremap <silent> ,uu :<C-u>Unite file_mru buffer<CR>
" }}}

" {{{ # vim filer
let g:vimfiler_as_default_explorer = 1
" }}}

" # vim autosave {{{
let g:auto_save = 0
let g:auto_save_in_insert_mode = 0 " insert mode のとき自動保存しない
let g:auto_save_silent = 1  " do not display the auto-save notification
" }}}

" {{{ # flygrep
nnoremap <Space>sgG :FlyGrep<cr>
" }}}

" {{{ # opam-user-settings
" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line
" }}}
