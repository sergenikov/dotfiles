execute pathogen#infect()
filetype indent plugin on

set nu
set rnu
set tabstop=2
set tabpagemax=7
set showtabline=2
" hide gui labels for gvim
set guitablabel=%t
set guioptions=agim
"correct backspace issues - 2 makes it always on
set backspace=2
set linebreak
" change vim directory current file directory
set autochdir
set hlsearch
set incsearch
set hidden
" mouse can't select text, just position
set mouse=nicr
" set tab and autoindent width to 2 spaces
set shiftwidth=2
set softtabstop=2

syntax on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" turn on NERDcommenter
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
filetype plugin on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-go configuration
" Go syntax highlight in vim 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"set bold comments in GUI and term
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
highlight Comment gui=bold
highlight Comment cterm=bold

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set tabstop=4 for python and add python highlighting
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd Filetype python setlocal tabstop=4
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
set runtimepath^=~/.vim/bundle/ctrlp.vim
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
set wildignore+=*/tmp/*,*.so,*.swp,*.zip 
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" custom ctrlp ignore settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
unlet g:ctrlp_custom_ignore
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\.git$\|\.hg$\|\.svn$\|bower_components$\|dist$\|node_modules$\|project_files$\|test$',
    \ 'file': '\.exe$\|\.so$\|\.dll$\|\.pyc$' }

let g:ctrlp_user_command = 'find %s -type f'
""" CtrlP setup done


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctags and ctrp setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" :CtrlPTag function searches in the tags file. Function is defined in CtrlP
nnoremap <leader>. :CtrlPTag<CR>
"for tags file first search current dir, then parent dir and so on
set tags=tags;


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Key and command mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" set leader key
let mapleader = ","

nnoremap <F2>  :set hlsearch!
nnoremap <F3>  :set cursorline!
nnoremap <F4>  :noh<CR>
"quickly inserts ; at the end of the line
inoremap <C-e> <esc>A;<esc>
"insert comments
nnoremap <C-_> mq0wi//<esc>`q
nnoremap <A-_> mq0wi//<esc>`q
"switching buffers
nnoremap <leader>h :bprevious<CR>
nnoremap <leader>l :bnext<CR>
"map save :w operation
nnoremap <C-s> :w<CR>
inoremap <C-s> <esc>:w<CR>

nnoremap <F5> :NERDTreeToggle<CR>
" tagbar toggle - show file structure, functions and stuff
nnoremap <F6> :TagbarToggle<CR>
" jj to <esc>
inoremap jj <esc>


command SolLight call SetSolarized("light")
command SolDark call SetSolarized("dark")

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Color term settings for different terminals
" TODO this config is not right, but it is working for the time being.
" Dealing with 256 colors over ssh on remote university machines is tricky.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if $COLORTERM == 'gnome-terminal'
      set t_Co=256
endif

if $TERM == 'xterm-256color'
    set t_Co=256
endif

if $COLORTERM == 'xterm'
      set t_Co=256
endif



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"folding setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"mapping of fold toggle to F9
inoremap <F9> <C-O>za
nnoremap <F9> za
onoremap <F9> <C-C>za
vnoremap <F9> zf

nnoremap <C-Left> :tabprevious<CR>
nnoremap <C-Right> :tabnext<CR>
nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"===indent folding
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set foldmethod=indent
"set foldcolumn=1
set foldlevel=99

"===syntax folding
"set foldmethod=syntax


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" color schemes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"let g:solarized_termcolors=256
"set background=light
"set background=dark
colorscheme molokai


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"set color for 80 characters
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"everything beyond 80 chars will be highlighted in red
"highlight OverLength ctermbg=red ctermfg=white guibg=#592929
"match OverLength /\%81v.\+/


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"NOTE set autocomplete for html on HTML file type open
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
set smartindent
set shiftwidth=4
set expandtab



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Different cursor lines
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
":hi CursorLine   cterm=NONE ctermbg=darkgrey ctermfg=white guibg=darkgrey guifg=white
":hi CursorLine   cterm=NONE ctermbg=234 guibg=234 
":hi CursorColumn cterm=NONE ctermbg=darkred ctermfg=white guibg=darkred guifg=white
":nnoremap <Leader>c :set cursorline! cursorcolumn!<CR>
"autocmd VimEnter * set cursorline!


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configure font for gvim
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if has("gui_running")
    if has("gui_gtk2")
        set guifont=Monospace\ 9
        colorscheme solarized
    endif
endif
" gui configuration done


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" configure airline
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set laststatus=2
let g:airline#extensions#tabline#enabled=1
" Just show the filename (no path) in the tab
let g:airline#extensions#tabline#fnamemod = ':t'
" airline - done



""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" my function
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Add/remove comments in current line
" Does not work as needed right now
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function CommentOut(c)
    "add comment if 0
    if a:c == 0
        execute "normal mq0wi//\<esc>`q"
    "remove comment if 
    else
        execute "normal mq0wxx\<esc>`q"
    endif
endfunction


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setting solarized color scheme takes more than one command
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function SetSolarized(color)
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" execute SetSolarized("light") to set bright solarized background.
" Function has to be defined before calling it. SolLight command is 
" bound to this function.
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"execute "SolLight"
