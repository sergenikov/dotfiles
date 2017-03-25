""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Key and command mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set leader key
nnoremap <Space>  :noh<CR>
"quickly inserts ; at the end of the line
inoremap <C-e> <esc>A;<esc>
"insert comments
nnoremap <C-_> mq^i//<esc>`q
nnoremap <A-/> mq^i//<esc>`q
"switching buffers
nnoremap <leader>h :bprevious<CR>
nnoremap <leader>l :bnext<CR>
nnoremap <leader>s :w<CR>
"map save :w operation
nnoremap <C-s> :w<CR>
inoremap <C-s> <esc>:w<CR>
" jj to <esc>
inoremap jj <esc>

"Golang
nnoremap <C-j> :GoDef<CR>

"Commands
command! SolLight call SetSolarized("light")
command! SolDark call SetSolarized("dark")
command! Comment normal mq^i//<esc>`q
command! Uncomment normal mq^2x<esc>`q
command! Hd1 normal 80i=<esc>
command! Hd2 normal 80i-<esc>
command! FH normal 80/-<esc>

"Emacs like binding for some commands
inoremap <C-f> <Right>
inoremap <C-b> <Left>
"inoremap <C-BS> <C-w>

" brackets autoclose
" inoremap {<CR> {<CR>}<C-o>O}

" Function keys
nnoremap <F2>  :set hlsearch!<CR>
nnoremap <F3>  :set cursorline!<CR>
nnoremap <F5> :NERDTreeToggle<CR>
" tagbar toggle - show file structure, functions and stuff
nnoremap <F6> :TagbarToggle<CR>
nnoremap <F7> :set paste!<CR>
nnoremap <F8> :echo expand('%:p')<CR>

"Folding setup
"mapping of fold toggle to F9
inoremap <F9> <C-O>za
nnoremap <F9> za
onoremap <F9> <C-C>za
vnoremap <F9> zf

nnoremap <F10> :SyntasticCheck<CR>
nnoremap <F11> :lclose<CR>

nnoremap <A-Left> :tabprevious<CR>
nnoremap <A-Right> :tabnext<CR>
nnoremap <C-Left> :bprevious<CR>
nnoremap <C-Right> :bnext<CR>
"nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
"nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>


" vanilla bracket autocompletion
" DID NOT TEST, JUST COPIED
" http://vim.wikia.com/wiki/Automatically_append_closing_characters
"ino " "<left>
"ino ' '<left>
"ino ( ()<left>
"ino [ []<left>
"ino { {}<left>
"ino {<CR> {<CR>}<ESC>O}}])'"

map <C-L> 10zl " Scroll 20 characters to the right
map <C-H> 10zh " Scroll 20 characters to the left
