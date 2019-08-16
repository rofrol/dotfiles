set nocompatible

if empty(glob('~/.config/nvim/autoload/plug.vim'))
	silent execute "!curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
	autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload')
Plug 'tpope/vim-fugitive'
Plug 'theJian/elm.vim'
Plug 'chaoren/vim-wordmotion'
Plug 'ayu-theme/ayu-vim'
Plug 'pangloss/vim-javascript'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'neovimhaskell/haskell-vim'
Plug 'wincent/terminus'
Plug 'elzr/vim-json'
Plug 'haya14busa/incsearch.vim'
Plug 'Raimondi/delimitMate'
Plug 'junegunn/rainbow_parentheses.vim'
Plug 'tpope/vim-characterize'
Plug 'vim-scripts/VisIncr'
Plug 'sbdchd/neoformat'
Plug 'sunaku/vim-dasht'
Plug 'gerw/vim-HiLinkTrace'
Plug 'djoshea/vim-autoread'
Plug 'tmhedberg/matchit'
Plug 'tpope/vim-surround'
Plug 'roman/golden-ratio'
call plug#end()

filetype plugin indent on
syntax enable

set termguicolors
set background=light
let ayucolor="light"
"color ayu
"highlight Normal guifg=white guibg=black

set shell=/bin/bash

set hidden

nnoremap ' `

set wildmenu
set wildmode=list:longest

set encoding=utf8

map K <Nop> " no thanks

set diffopt+=vertical " diff is vertical and not horizontal

set wrap            " wraps long lines
set nolinebreak     " but doesn't wait for the whole word

" how many lines to the cursor when scrolling
set so=5

"set number          " number lines

set showmode        " shows INSERT or VISUAL mode
set showcmd         " shows unfinished commands in statusbar
set cmdheight=1 " 2 - dont show so many "Press Enter" prompts

set nojoinspaces    " WTF, Americans?

" https://stackoverflow.com/questions/7406814/in-vim-how-do-you-scroll-a-buffer-so-the-cursor-location-is-centered-in-the-scr
"set scrolloff=999   " scrolling

set ts=2            " tabulator width 2
set sw=2            " indentation width 2

" read-only files? no problem
cmap w!! w !sudo tee % >/dev/null

set expandtab       " <TAB> does spaces, really

set ignorecase
set incsearch       " shows search results as you write
set hlsearch        " highlights all the matches
nohlsearch

set lazyredraw

set showmatch

set noerrorbells visualbell t_vb=
if has('autocmd')
  autocmd GUIEnter * set visualbell t_vb=
endif

set nosmartindent   " because of python comments ... bah :)

set autoread        " automatically load files on change
set autowriteall    " automatically write all

set history=10000
set undolevels=10000
set nobackup
set nowb
set noswapfile

" start on the line we last ended on
autocmd BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif

map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

let mapleader=","

map <Leader>b  :buffers<cr>
map <silent> <Leader>n :nohl<cr>
map     <Leader>w  :w!<cr>
cabbrev W          w!
cabbrev Wq         wq
cabbrev E          e
cabbrev Q          q

" window shortcuts
" maximize
nmap <C-w>m <C-w>_<C-w>\|

" fugitive
map <Leader>gs :Gstatus<cr><C-w>H<C-n>

let g:Gitv_OpenHorizontal = 'auto'

"map <Leader>t  :Unite -start-insert file_rec/git:--cached:--others:--exclude-standard<cr>
"map <Leader><Leader>c <Plug>(unite_redraw)

" copy all, copy, paste
map     <Leader>a  ggVG"+y
vmap    <Leader>y  "+y
map     <Leader>p  "+p

" agh
nnoremap <C-c> <Esc>
vnoremap <C-c> <Esc>gV
onoremap <C-c> <Esc>
inoremap <C-c> <Esc>`^

" nbsp
inoremap   <Space>

" so we can backspace anything
set backspace=indent,eol,start

" where to do temporary files?
set directory=/tmp

" j and k scroll on the visual rows, not the real-file ones
noremap k gk
noremap j gj

" when J-ing, don't jump with cursor
nnoremap J mzJ`z

" annoyances
nnoremap <F1> <nop>
nnoremap Q <nop>
nnoremap K <nop>
vnoremap K <nop>

" yank to EOL
nnoremap Y y$

" no startup message
set shortmess+=atI

" visual mode - no spaces? no problem!
set virtualedit=block

" intuitive splits
set splitbelow
set splitright

" regexes
nnoremap / /\v
vnoremap / /\v

augroup rainbow_lisp
  autocmd!
  autocmd FileType lisp,clojure,scheme RainbowParentheses
augroup END

" matching paren
hi MatchParen ctermbg=7 guibg=#c0c0c0

" searching visually selected text
vnorem * y/<c-r>"<cr>

autocmd BufEnter * set mouse=a

let g:bookmark_auto_save = 1

"highlight SignColumn ctermbg=8
"highlight EndOfBuffer ctermfg=8 ctermbg=8

let g:elm_detailed_complete = 1
let g:elm_format_autosave = 1
let g:elm_setup_keybindings = 0
let g:elm_make_show_warnings = 0
let g:elm_jump_to_error = 0
let g:elm_format_fail_silently = 0

let g:ycm_semantic_triggers = {
     \ 'elm' : ['.'],
     \}

if has("autocmd")
  au BufNewFile,BufReadPost *.ell set filetype=scheme
  au BufNewFile,BufReadPost *.es6 set filetype=javascript
  au BufNewFile,BufReadPost *.neon set filetype=yaml
endif

autocmd Filetype elm setlocal ts=4 sw=4 sts=4 expandtab nowrap

autocmd Filetype html setlocal ts=4 sw=4 sts=0 expandtab


let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal = 0
let g:vim_json_syntax_conceal = 0

let g:mta_use_matchparen_group = 0
let g:mta_set_default_matchtag_color = 0
highlight MatchTag ctermfg=2 ctermbg=4

autocmd Filetype vala setlocal ts=4 sw=4 sts=0 expandtab

nnoremap <BS> {
onoremap <BS> {
vnoremap <BS> {

nnoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
onoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
vnoremap <CR> }

function! FzyCommand(list_command) abort
    let l:callback = {
                \ 'window_id': win_getid(),
                \ 'filename': tempname()
                \ }
    let l:fzy_command = 'fzy'

    function! l:callback.on_exit(job_id, data, event) abort
        bdelete!
        call win_gotoid(self.window_id)
        if filereadable(self.filename)
            try
                let l:selected_filename = readfile(self.filename)[0]
                exec ':e ' . l:selected_filename
            catch /E684/
            endtry
            call delete(self.filename)
        endif
    endfunction

    execute 'botright 10 new'
    let l:term_command = a:list_command . '|' . l:fzy_command . '>' .
                \ l:callback.filename
    let l:term_job_id = termopen(l:term_command, l:callback)
    setlocal nonumber norelativenumber
    startinsert
endfunction

nnoremap <leader>e :call FzyCommand("rg . -l -g ''")<cr>

augroup fmt
  autocmd!
  autocmd BufWritePre *.elm undojoin | Neoformat
augroup END

nnoremap <Leader>k :Dasht!<Space>
nnoremap <silent> <Leader>K :call Dasht([expand('<cword>'), expand('<cWORD>')], '!')<Return>
vnoremap <silent> <Leader>K y:<C-U>call Dasht(getreg(0), '!')<Return>

" Vertical split
set fillchars+=vert:\
hi VertSplit guibg=#353C48