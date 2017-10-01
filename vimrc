"
" Use :Tex to open file tree browser and select file for new tab.
" 

" Change tabs using Shift+h and Shift+l.
map <S-h> gT
map <S-l> gt

" Enable syntax processing.
syntax enable

" Set color scheme
" colorscheme badwolf	" https://github.com/sjl/badwolf 
" colorscheme solarized	" 
" set background=dark

set tabstop=4      " Number of visual spaces per tab.
set softtabstop=4  " Number of spaces in tab when editing.
set expandtab	   " Tabs are spaces.

set nu             " Show line numbers.
" set relativenumber " Show relative line numbers.
set showcmd        " Show command in bottom bar.
set cursorline     " Highlight current line.

filetype indent on " Load filetype-specific indent files.

set wildmenu       " Visual autocomplete for command menu.
set lazyredraw     " Redraw the screen only when we need to.
set showmatch      " Highlight matching [{()}]]

set incsearch      " Search as characters are entered.
set hlsearch       " Highlight search matches.


set runtimepath^=~/.vim/bundle/ctrlp.vim  " Enable ctrlp.vim.
" Change the default open file to new tab.
let g:ctrlp_prompt_mappings = {
\'AcceptSelection("e")': ['<c-t>'],
\'AcceptSelection("t")': ['<cr>', '<2-LeftMouse'],
\}


" Allows cursor change in tmux mode.
if exists('$TMUX')
        let &t_SI = "\<Esc>Ptmux;\<ESC>\<ESC>]50;CursorShape=1\x7\<ESC>\\"
        let &t_EI = "\<Esc>Ptmux;\<ESC>\<ESC>]50;CursorShape=0\x7\<ESC>\\"
else
        let &t_SI = "\<Esc>]50;CursorShape=1\x7"
        let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif
