map <S-h> gT
map <S-l> gt
set nu 



""set nocompatible              " be iMproved, required
""filetype off                  " required
" 
"" set the runtime path to include Vundle and initialize
""set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()
"" alternatively, pass a path where Vundle should install plugins
""call vundle#begin('~/some/path/here')
"  [M h;
"" let Vundle manage Vundle, required
""Plugin 'gmarik/Vundle.vim'
"   
"" The following are examples of different formats supported.
"" Keep Plugin commands between vundle#begin/end.
"
"
""Plugin 'tpope/vim-rails'
"
"    
"" All of your Plugins must be added before the following line
""call vundle#end()            " required
""filetype plugin indent on    " required

" Set encryption to blowfish.
"set cm=blowfish

"set rtp+=$GOROOT/misc/vim
filetype plugin indent on
"syntax on

"filetype plugin on
let g:sparkupArgs = '--no-html5-self-closing'

setlocal spell spelllang=en_us
set background=dark
"set background=light
"colorscheme solarized
"let g:solarized_termcolors=256

" Fixes arrow keys in Mac terminal.
set nocp

" Turn on syntax and search term highlighting. "
if &t_Co > 1
    syntax enable
    set hlsearch
endif

" Only do this part when compiled with support for autocommands
"if has("autocmd")
  " In text files, always limit the width of text to 78 characters
"  autocmd BufRead *.txt set tw=78
  " When editing a file, always jump to the last cursor position
"  autocmd BufReadPost *
"  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
"  \   exe "normal! g'\"" |
"  \ endif
"endif

if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

set smartindent
set tabstop=2
set shiftwidth=2
set expandtab

"colorscheme gotan

"au Filetype html,xml,xsl source ~/.vim/scripts/closetag.vim
