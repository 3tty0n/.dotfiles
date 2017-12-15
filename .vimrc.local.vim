" {{{ vim autosave
Plug 'vim-scripts/vim-auto-save'

let g:auto_save = 1
let g:auto_save_in_insert_mode = 0  " do not save while in insert mode
let g:auto_save_silent = 1
" }}}

" {{{ ocaml
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
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

" {{{ scala
Plug 'derekwyatt/vim-scala'
" }}}

" {{{ latex
Plug 'lervag/vimtex'
" }}}
