filetype plugin on

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"  GUI configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" hide gui labels for gvim
"set guioptions=agim
if has("gui_running")
  call VimOneColor()

  set guioptions-=r

  " turn bell off
  set vb t_vb= 

  if has("gui_mac") || has("gui_macvim")
    set guifont=Menlo:h12
    set guioptions=

  elseif has("gui_gtk2")
    set guitablabel=%t
  endif
else
  colo monokai
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" syntastic configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:syntastic_javascript_checkers = ['./node_modules/eslint/bin/eslint.js']
let g:syntastic_javascript_eslint_exe='$(npm bin)/eslint'

"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

let g:syntastic_always_populate_loc_list = 0
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 1
let g:syntastic_javascript_checkers = ['eslint']

"let g:syntastic_error_symbol = '❌'
"let g:syntastic_style_error_symbol = '⁉️'
"let g:syntastic_warning_symbol = '⚠️'
"let g:syntastic_style_warning_symbol = '💩'

let g:syntastic_check_on_open=0

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
let g:ctrlp_working_path_mode = 'a'
" set wildignore+=*/tmp/*,*.so,*.swp,*.zip 
nnoremap <C-b> :CtrlPBuffer<CR>
" nnoremap <leader>m :CtrlPMRU<CR>

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" ctrlp ignore settings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:ctrlp_custom_ignore = { 'dir': 'node_modules' }
let g:ctrlp_custom_ignore = { 'dir': 'build$\|node_modules$' }
"let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
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

" command-t
let g:CommandTInputDebound = 2
let g:CommandTMaxFiles = 200000
let g:CommandTFileScanner = "git"
let g:CommandTIgnoreCase = 1
