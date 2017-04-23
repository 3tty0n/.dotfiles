" Plugin key-mappings.  " <C-k>でsnippetの展開
imap <C-k> <Plug>(neosnippet_expand_or_jump)
smap <C-k> <Plug>(neosnippet_expand_or_jump)
" Where my snippets are in
let g:neosnippet#snippets_directory = '~/.vim/snippets/'
