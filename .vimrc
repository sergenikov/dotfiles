execute pathogen#infect()
filetype indent plugin on

syntax on

let mapleader = ","
set nu
set rnu
set tabpagemax=7
set showtabline=2
"correct backspace issues - 2 makes it always on
set backspace=2
set linebreak
" change vim directory current file directory
"set autochdir
set hlsearch
set incsearch
set hidden

"mouse
"mouse can't select text, just position
set mouse=nicr
"this mouse settings makes copy-pasting on mac easier with SHIFT pressed

" set tab and autoindent width to 2 spaces
set shiftwidth=2
"set softtabstop=2

" configs below thanks to: http://nvie.com/posts/how-i-boosted-my-vim/
set nowrap                      "don't wrap lines
set tabstop=4                   "tab is four spaces
set backspace=indent,eol,start  "allow backspacing over everthing in insert mode
set autoindent                  "always set autoindenting on
set copyindent                  "copy previous indentation on autoindenting
set shiftwidth=4                "set number of spaces on autoindenting
set shiftround                  "use multiple of shiftwidth when indenting with < or >
"set showmatch                   "show matching parentheses
set ignorecase                  "ignore case when searching
set smartcase                   "ignore case if search pattern is all lowercase, 
                                "case sensitive otherwise
set smarttab                    "insert tabs on the start of a line according to
                                "shiftwidth, not tabstop

set history=1000                "remember 1000 commands and search history lines
set undolevels=1000             "more undo levels
set title                       "change terminal title
"set visualbell                  "don't beep
"set noerrorbells                "don't beep

" Useful for <c-x> <c-l> and similar commands
" Search down into subfolders
" Provides tab-completion for all file-related tasks
set path+=**

" Display all matching files when we tab complete
set wildmenu

"clipboard settings - shared cp with system
"set clipboard=unnamed

" terminal gui options - remove gui
set guioptions-=m
set guioptions-=T

set showcmd

" Changed for git-gutter plugin
set updatetime=1000

"indent folding
set foldmethod=indent
"set foldcolumn=1
set foldlevel=99
"syntax folding
"set foldmethod=syntax


filetype plugin on

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntastic configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_javascript_checkers = ['./node_modules/eslint/bin/eslint.js']

"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_javascript_checkers = ['eslint']

let g:syntastic_error_symbol = '❌'
let g:syntastic_style_error_symbol = '⁉️'
let g:syntastic_warning_symbol = '⚠️'
let g:syntastic_style_warning_symbol = '💩'

highlight link SyntasticErrorSign SignColumn
highlight link SyntasticWarningSign SignColumn
highlight link SyntasticStyleErrorSign SignColumn
highlight link SyntasticStyleWarningSign SignColumn

"let g:syntastic_debug = 3

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-go configuration
" Go syntax highlight in vim 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set tabstop=4 for python and add python highlighting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"autocmd Filetype python setlocal tabstop=4
let python_highlight_all = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Setup delimitMate
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
so $HOME/.vim/bundle/delimitMate/test/_setup.vim
let delimitMate_expand_cr = 1
filetype indent plugin on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"CtrlP setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"/set runtimepath^=~/.vim/bundle/ctrlp.vim
if exists("g:ctrlp_user_command")
	unlet g:ctrlp_user_command
endif

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip 
nnoremap <leader>b :CtrlPBuffer<CR>
nnoremap <leader>m :CtrlPMRU<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctrlp ignore settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_custom_ignore = { 'dir': 'node_modules' }
let g:ctrlp_custom_ignore = { 'dir': 'build$\|node_modules$' }
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
"let g:ctrlp_custom_ignore = {
"    \ 'dir':  '\.git$\|\.hg$\|\.svn$\|bower_components$\|dist$\|node_modules$\|project_files$\|test$',
"    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

"let g:ctrlp_user_command = 'find %s -type f'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctags and ctrp setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" :CtrlPTag function searches in the tags file. Function is defined in CtrlP
nnoremap <leader>. :CtrlPTag<CR>
"for tags file first search current dir, then parent dir and so on
set tags=./tags,tags;$HOME

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" javascript
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:javascript_plugin_jsdoc = 1

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
nnoremap <F4>  :noh<CR>
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Color term settings for different terminals
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set t_Co=256




""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" color schemes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"this helps fix solarized on some terminals
"let g:solarized_termcolors=256
"set background=light
"set background=dark

" monokai looks good on all terminals in 256 colors.
colorscheme atom-dark-256 


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"set color for 80 characters
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"everything beyond 120 chars will be highlighted in red
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%121v.\+/
function! Set120(toggle)
    if a:toggle == 1
        highlight OverLength ctermbg=red ctermfg=white guibg=#592929
        match OverLength /\%121v.\+/
    else
        highlight OverLength ctermbg=red ctermfg=white guibg=#592929
        match OverLength /\%121v.\+/
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"NOTE set autocomplete for html on HTML file type open
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
"set smartindent
"set shiftwidth=4
"set expandtab



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Different cursor lines
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
":hi CursorLine   cterm=NONE ctermbg=darkgrey ctermfg=white guibg=darkgrey guifg=white
":hi CursorLine   cterm=NONE ctermbg=234 guibg=234 
":hi CursorColumn cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
":nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
"autocmd VimEnter * set cursorline!


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configure font for gvim and mvim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" hide gui labels for gvim
"set guioptions=agim
if has("gui_running")
    "set guifont = Monospace\ 9
    if has("gui_mac") || has("gui_macvim")
        colorscheme atom-dark-256
        set transparency=3
        set guioptions-=r
        colorscheme molokai
        " turn bell off
        set vb t_vb=
    endif
    if has("gui_gtk2")
        colorscheme molokai
        set guioptions-=r
        set guitablabel=%t
    endif
endif

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configure airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
"let g:airline#extensions#tabline#enabled=1
" Just show the filename (no path) in the tab
let g:airline#extensions#tabline#fnamemod = ':t'
" configure special symbols
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

" unicode symbols
let g:airline_left_sep = '»'
let g:airline_left_sep = '▶'
let g:airline_right_sep = '«'
let g:airline_right_sep = '◀'
let g:airline_symbols.linenr = '␊'
let g:airline_symbols.linenr = '␤'
let g:airline_symbols.linenr = '¶'
let g:airline_symbols.branch = '⎇'
let g:airline_symbols.paste = 'ρ'
let g:airline_symbols.paste = 'Þ'
let g:airline_symbols.paste = '∥'
let g:airline_symbols.whitespace = 'Ξ'
" airline - done



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" my function
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Add/remove comments in current line
" Does not work as needed right now
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! CommentOut(c)
    "add comment if 0
    if a:c == 0
        execute "normal mq^i//\<esc>`q"
        echom "hello"
    "remove comment if 
    else
        execute "normal mq^xx\<esc>`q"
    endif
endfunction


"Function for commenting a block of Visually selected text
function! Comment(fl, ll)
  let i=a:fl
  let comment="//"
  while i<=a:ll
    let cl=getline(i)
    let cl2=comment.cl
    call setline(i, cl2)
    let i=i+1
  endwhile
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setting solarized color scheme takes more than one command
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! SetSolarized(color)
    let g:solarized_termcolors=256
    if a:color == "light"
        set background=light
    elseif a:color == "dark"
        set background=dark
    else
        echom "Wrong argument. Try light or dark"
    endif
    colorscheme solarized
endfunction
