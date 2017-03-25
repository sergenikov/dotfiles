set nocompatible
filetype off

"set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'ctrlpvim/ctrlp.vim.git'
Plugin 'Raimondi/delimitMate.git'
Plugin 'hdima/python-syntax.git'
Plugin 'scrooloose/syntastic.git'
Plugin 'majutsushi/tagbar.git'
Plugin 'bling/vim-airline.git'
Plugin 'fatih/vim-go.git'
Plugin 'tpope/vim-surround.git'
Plugin 'justinmk/vim-syntax-extra.git'
Plugin 'tpope/vim-fugitive.git'
Plugin 'scrooloose/nerdcommenter.git'
Plugin 'vim-airline/vim-airline-themes.git'
Plugin 'scrooloose/nerdtree.git'
Plugin 'airblade/vim-gitgutter.git'
Plugin 'powerline/powerline.git'
Plugin 'pangloss/vim-javascript.git'
Plugin 'mxw/vim-jsx.git'
Plugin 'rakr/vim-one.git'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

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

"set softtabstop=2

" configs below thanks to: http://nvie.com/posts/how-i-boosted-my-vim/
set nowrap                      "don't wrap lines
set tabstop=4                   "tab is four spaces
set backspace=indent,eol,start  "allow backspacing over everthing in insert mode
set autoindent                  "always set autoindenting on
set copyindent                  "copy previous indentation on autoindenting
set shiftwidth=2                "set number of spaces on autoindenting
set expandtab					"expand tab into spaces
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

source $HOME/dotfiles/.functions.vim
source $HOME/dotfiles/.config.vim
source $HOME/dotfiles/.keys.vim
