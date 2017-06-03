" file contains my tests with functions. Just trying things out.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" open NERDTree and run rnu in it 
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! Tree()
  NERDTreeToggle
  set rnu
endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-one
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
function! VimOneColor()
  colorscheme one
  set background=dark
  let g:airline_theme='one'
endfunction

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
