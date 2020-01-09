set nocompatible

if empty(glob('~/.config/nvim/autoload/plug.vim'))
	silent execute "!curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
	autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/autoload')
Plug 'tpope/vim-fugitive'
Plug 'theJian/elm.vim'
"Plug 'chaoren/vim-wordmotion'
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
"Plug 'roman/golden-ratio'
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
"Plug 'ap/vim-buftabline'
Plug 'zefei/vim-wintabs'
Plug 'lambdalisue/suda.vim'
Plug 'qpkorr/vim-bufkill'

" =============================================================================
" Rust
" =============================================================================
" The version of rust.vim bundled with vim is very outdated, and needs updating. (Among other things, formatting no longer works with current rustfmt.) https://github.com/neovim/neovim/issues/11219#issue-506329283
"Plug 'rust-lang/rust.vim'
" https://github.com/rust-lang/rust.vim/issues/89#issuecomment-571216745
" interesting https://github.com/rust-lang/rust.vim/pull/369/files#diff-620e97ff3ec1f7b01a2d1f60373196cdR99
" does this work? https://github.com/whamcloud/integrated-manager-for-lustre/pull/1407
Plug 'rust-lang/rust.vim', { 'for': [ 'rust' ], 'do': 'rustup component add rustfmt' }
let g:rustfmt_autosave = 1
" or do `echo 'edition = "2018"' > rustfmt.toml` for every project https://github.com/rust-lang/rust.vim/issues/368#issue-520085064
let g:rustfmt_options = '--edition 2018'

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

" Does not work in neovim https://github.com/neovim/neovim/issues/8527#issuecomment-406841041
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

" Does not work
nmap <Backspace> :noh<CR>

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

" https://superuser.com/questions/404333/how-do-i-only-dp-or-do-just-the-lines-not-the-entire-block-in-vim-diff
"nnoremap <silent> <leader>dp V:diffput<cr>
nnoremap <silent> <F4> V:diffput<cr>
"nnoremap <silent> <leader>dg V:diffget<cr>
nnoremap <silent> <F5> V:diffget<cr>

" This will allow you to undo a typo or unwanted change on the other file/window, because :undo of just u will only undo a change in the present window.
nmap <silent> <leader>du :wincmd w<cr>:normal u<cr>:wincmd w<cr>

set nu

" use system clipboard
" https://gist.github.com/jdhao/d592ba03a8862628f31cba5144ea04c2#file-options-vim-L20
" https://stackoverflow.com/questions/30691466/what-is-difference-between-vims-clipboard-unnamed-and-unnamedplus-settings/30691754#30691754
set clipboard^=unnamed,unnamedplus

" The way to show the result of substitution in real time for preview
" https://gist.github.com/jdhao/d592ba03a8862628f31cba5144ea04c2#file-options-vim-L77
set inccommand=nosplit

" ============================================================================
" Terminal
" =============================================================================

" https://www.reddit.com/r/neovim/comments/7i2k6u/neovim_terminal_one_week_without_tmux/

" :20sp term://bash
" :80vs term://bash
" https://stackoverflow.com/questions/1388252/specifying-width-for-vsplit-in-vim/1388268#1388268
" :20split | term
" :split | resize 20 | term
" https://github.com/neovim/neovim/issues/5073#issuecomment-427493209

" https://stackoverflow.com/questions/57899527/neovim-vim-resize-window-based-on-type-of-buffer/57904110#57904110 
" https://vi.stackexchange.com/questions/3670/how-to-enter-insert-mode-when-entering-neovim-terminal-pane/3765#3765
" https://stackoverflow.com/questions/57899527/neovim-vim-resize-window-based-on-type-of-buffer/57904110#57904110
" https://github.com/neovim/neovim/issues/9483#issuecomment-569417862
" https://vi.stackexchange.com/questions/22307/neovim-go-into-insert-mode-when-clicking-in-a-terminal-in-a-pane/22327#22327
" https://gist.github.com/jdhao/d592ba03a8862628f31cba5144ea04c2#file-autocommands-vim-L21

if has('nvim')
    augroup terminal_setup | au!
        au TermOpen * if &buftype ==# 'terminal' | startinsert | setlocal nonumber | endif
        au TermOpen * nnoremap <buffer><LeftRelease> <LeftRelease>i
        " stopinsert first, so when I come from file buffer, it is not in insert file mode when I can click and change cursor on whatever line
        au BufEnter * if &buftype ==# 'terminal' | stopinsert | startinsert | endif
        "au BufLeave * if &buftype ==# 'terminal' | stopinsert | endif
    augroup end
endif

" https://www.reddit.com/r/vim/comments/8n5bzs/using_neovim_is_there_a_way_to_display_a_terminal/dzt3fix/
" Terminal Function
let g:term_buf = 0
let g:term_win = 0
function! TermToggle(height)
    if win_gotoid(g:term_win)
        hide
    else
        botright new
        exec "resize " . a:height
        try
            exec "buffer " . g:term_buf
        catch
            call termopen($SHELL, {"detach": 0})
            let g:term_buf = bufnr("")
            set nonumber
            set norelativenumber
            set signcolumn=no
        endtry
        startinsert!
        let g:term_win = win_getid()
    endif
endfunction

" Toggle terminal on/off (neovim)
nnoremap <A-t> :call TermToggle(12)<CR>
inoremap <A-t> <Esc>:call TermToggle(12)<CR>
tnoremap <A-t> <C-\><C-n>:call TermToggle(12)<CR>

" Terminal go back to normal mode
tnoremap <Esc> <C-\><C-n>
tnoremap :q! <C-\><C-n>:q!<CR>

" https://www.reddit.com/r/neovim/comments/6mkvo3/builtin_terminals_or_tmux/dk2db7p/
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
inoremap <C-h> <Esc><C-w>h
inoremap <C-j> <Esc><C-w>j
inoremap <C-k> <Esc><C-w>k
inoremap <C-l> <Esc><C-w>l

" Quickly create a new terminal in a new tab
tnoremap <Leader>c <C-\><C-n>:tab new<CR>:term<CR>
noremap <Leader>c :tab new<CR>:term<CR>
inoremap <Leader>c <Esc>:tab new<CR>:term<CR>

" Quickly create a new terminal in a vertical split
tnoremap <Leader>% <C-\><C-n>:vs<CR><C-w><C-w>:term<CR>
noremap <Leader>% :vs<CR><C-w><C-w>:term<CR>
inoremap <Leader>% <Esc>:vs<CR><C-w><C-w>:term<CR>

" Quickly create a new terminal in a horizontal split
tnoremap <Leader>" <C-\><C-n>:sp<CR><C-w><C-w>:term<CR>
noremap <Leader>" :sp<CR><C-w><C-w>:term<CR>
inoremap <Leader>" <Esc>:sp<CR><C-w><C-w>:term<CR>

"nnoremap <C-N> :bnext<CR>
"nnoremap <C-P> :bprev<CR>
" https://www.reddit.com/r/neovim/comments/ac1i8w/whats_your_buffermanagement_setup/ed5097b/
nmap <silent> <Tab> :bn<CR>
nmap <silent> <S-Tab> :bp<CR> 

highlight TermCursor ctermfg=red guifg=red

" https://bluz71.github.io/2018/12/04/fuzzy-finding-in-vim-with-fzf.html
" C-x to open horizontally, C-v to open vertically, C-t to open in new tab
nnoremap <silent> <Leader><Space> :GFiles<CR>
nnoremap <silent> <Leader>f :Files<CR>
" open file in the same dir
nnoremap <silent> <Leader>f. :Files <C-r>=expand("%:h")<CR>/<CR>
nnoremap <silent> <Leader>fb :Buffers<CR>

" https://github.com/rkruk/neovim-dotfiles/blob/8b8594ea05e94e25d627f4f32f8d382afca69fcc/config.vim#L38
set title          " Set the title of the window in the terminal to the file

" https://www.reddit.com/r/neovim/comments/8fu8u9/for_those_using_neovim_as_a_tmux_replacement/dyfhfsw/
function! VisualMapper(commands, ...)
  let common_prefix = "" | let key_count = 1
  if a:0 > 0 | let common_prefix = a:1 | endif
  if a:0 > 1 | let key_count = a:2 | endif
  let key_and_cmd = {}
  for [key_and_name, cmd] in a:commands
    echo key_and_name
    let key_and_cmd[split(key_and_name, "\\s\\+>")[0]] = cmd
  endfor
  echo "> "
  let KeyReader = {-> nr2char(getchar())}
  call feedkeys(common_prefix . key_and_cmd[join(map(range(key_count), KeyReader), '')])
  call feedkeys("i\<Cr>")
endfunction

let g:commander = [
\  ["C > List Connections",        "inmcli -p c show --active"],
\  ["d > Show current directory",  "ipwd\n"],
\  ["D > List Disks",              "ifdisk -l"],
\  ["g > Grep",                    "igrep -r --color --exclude-dir=\".git\" \""],
\  ["i > Grep Including ",         "igrep -r --color --exclude-dir=\".git\" --include=\"*."],
\  ["t > Show running processes",  "itop -u $USER"]]

tnoremap <A-s> <C-\><C-n>:call VisualMapper(g:commander)<cr>


" ============================================================================
" zefei/vim-wintabs
" =============================================================================

map <C-H> <Plug>(wintabs_previous)
map <C-L> <Plug>(wintabs_next)
map <C-T>c <Plug>(wintabs_close)
map <C-T>u <Plug>(wintabs_undo)
map <C-T>o <Plug>(wintabs_only)
map <C-W>c <Plug>(wintabs_close_window)
map <C-W>o <Plug>(wintabs_only_window)
command! Tabc WintabsCloseVimtab
command! Tabo WintabsOnlyVimtab

" https://stackoverflow.com/questions/290465/how-to-paste-over-without-overwriting-register/290723#290723
" I haven't found how to hide this function (yet)
function! RestoreRegister()
  let @" = s:restore_reg
  return ''
endfunction

function! s:Repl()
    let s:restore_reg = @"
    return "p@=RestoreRegister()\<cr>"
endfunction

" NB: this supports "rp that replaces the selection by the contents of @r
vnoremap <silent> <expr> p <sid>Repl()

" ============================================================================
" Workspaces
" =============================================================================

function! DefaultWorkspace()
    " Rough num columns to decide between laptop and big monitor screens
    let numcol = 2
    if winwidth(0) >= 220
        let numcol = 3
    endif

    if numcol == 3
        e term://zsh
        file Shell\ Two
        vnew
    endif

    vsp term://~/Programs/golang/context
    file Context
    sp term://zsh
    file Shell\ One
    wincmd k
    resize 4
    wincmd h
endfunction
command! -register DefaultWorkspace call DefaultWorkspace()

" https://medium.com/@garoth/neovim-terminal-usecases-tricks-8961e5ac19b9#.wewpz5kgy
" look at ~/.config/nvim/.nvim_session.example.vim

" reading with `nvim -S .nvim_session.vim` is not good, as cursor position
" will not be saved etc.
" https://stackoverflow.com/questions/456792/vim-apply-settings-on-files-in-directory/13192721#13192721
if filereadable(".nvim_session.vim")
    source .nvim_session.vim
endif
" Above command should be the last

" TODO
" - paste over selection should not replace clipboard with selection
" - when openning file with fzf, then `:sp|term`, I need to run `reset` to see characters
