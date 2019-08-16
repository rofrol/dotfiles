" https://github.com/thaerkh/vim-workspace

"let g:plug_url_format = 'git@github.com:%s.git'
"let g:plug_url_format = 'git+ssh://github.com/%s.git'

if empty(glob('~/.config/nvim/autoload/plug.vim'))
	silent execute "!curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
	autocmd VimEnter * PlugInstall | source $MYVIMRC
endif


call plug#begin('~/.config/nvim/autoload')
" Plugins directory https://vimawesome.com
Plug 'elmcast/elm-vim'

Plug 'airblade/vim-rooter'
let g:rooter_silent_chdir = 1 " https://github.com/airblade/vim-rooter/issues/84#issuecomment-435807763

Plug 'Shougo/denite.nvim'
Plug 'raghur/fruzzy', {'do': { -> fruzzy#install()}}

" https://github.com/justinmk/config/blob/9b5e06bf5a85865dcbf793178342cfc4201cb752/.config/nvim/init.vim#L123
Plug 'tpope/vim-obsession'
let g:obsession_no_bufenter = 1  " https://github.com/tpope/vim-obsession/issues/40

" no need to select line to comment it like in vim-commentary, just <leader>cc to toggle
Plug 'scrooloose/nerdcommenter'
map <leader>c <plug>NERDCommenterToggle

Plug 'rust-lang/rust.vim'
let g:rustfmt_autosave = 1
let g:rustfmt_command = 'rustup run nightly rustfmt'

Plug 'qpkorr/vim-bufkill'

Plug 'Soares/base16.nvim'

Plug 'owickstrom/vim-colors-paramount'

" needed to use full url
" https://github.com/junegunn/vim-plug/issues/133#issuecomment-65886124
" commenting out - does not cycle colors installed with Soares/base16.nvim
"Plug 'https://github.com/codingluke/setcolors.vim'

" no good schemes with light bg
"Plug 'rafi/awesome-vim-colorschemes'
" Plug 'flazz/vim-colorschemes'

Plug 'christoomey/vim-tmux-navigator'

" Disabling for now, no async and maybe errors, also w0rp/ale has linter also
" https://github.com/sbdchd/neoformat/issues/106
"Plug 'sbdchd/neoformat'
"" haskell: use brittany as used by https://github.com/haskell/haskell-ide-engine#features
"
"augroup fmt
"  autocmd!
"  " autocmd BufWritePre * undojoin | Neoformat
"  " There are some error showing when I do `:w` after `elm make` file in the
"  " console, maybe this will be better
"  " https://github.com/sbdchd/neoformat/issues/134#issuecomment-347180213
"  au BufWritePre * try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | finally | silent Neoformat | endtry
"augroup END

Plug 'w0rp/ale'
" You need to make sure that the linter binary is executable and is in PATH 
" https://www.reddit.com/r/neovim/comments/a8mc29/linting_of_jinja_template_files_in_neovim_with/ecdh8c6/
" Set the format of the echo msg so we know which thing is complaining
" https://github.com/w0rp/ale/issues/1960
" TODO:
" Open a list window with linting errors
" https://stackoverflow.com/questions/49784158/show-multiline-flow-errors-in-vim-status-line/49915382#49915382
let g:ale_echo_msg_format = '%linter% says %s'
let g:ale_fix_on_save = 1
" keep the sign gutter open at all times
let g:ale_sign_column_always = 1
let g:ale_fixers = {'elm': ['elm-format']}
" Linter works, :ALEInfo shows that linter 'make' is enabled

" https://stackoverflow.com/questions/1205286/renaming-the-current-file-in-vim/25053688#25053688 
Plug 'tpope/vim-eunuch'

call plug#end()

" https://github.com/kristijanhusak/neovim-config/blob/52e9e886dd256c5c267c70d2afa72796f3390a92/init.vim#L48 
" https://stackoverflow.com/questions/10389205/show-current-leader-key-setting
let mapleader = ','

" global clipboard
" https://www.reddit.com/r/neovim/comments/3fricd/easiest_way_to_copy_from_neovim_to_system/
" https://github.com/neovim/neovim/issues/4706#issuecomment-325284385
set clipboard+=unnamedplus


" Paste with middle mouse click
vmap <LeftRelease> "*ygv

" Paste with <Shift> + <Insert>
"https://github.com/equalsraf/neovim-qt/issues/327#issuecomment-325660764
imap <S-Insert> <C-R>*
cmap <S-Insert> <C-R>*

" Quickly edit/reload this configuration file
" https://superuser.com/questions/132029/how-do-you-reload-your-vimrc-file-without-restarting-vim/1120318#1120318
nnoremap gev :e $MYVIMRC<CR>
nnoremap gsv :so $MYVIMRC<CR>

" quick search and replace
nmap <Leader>s :%s//g<Left><Left>

" Somehow keybindings are not set, even with `let g:elm_setup_keybindings = 1`
nmap <Leader>m :ElmMake<CR>
nmap <Leader>b :ElmMakeMain<CR>

set splitbelow
set splitright

nmap <Backspace> :noh<CR>

" https://stackoverflow.com/questions/9511253/how-to-effectively-use-vim-wildmenu#comment58670078_9528037
" http://www.reddit.com/r/vim/comments/19izuz/whats_your_wildmode/
" Wildmenu
set wildmenu " Show list instead of just completing
"set wildmode=longest,list
"set wildmode=longest:list,full
"set wildmode=longest:full,full
set wildmode=list:longest,full " Command <Tab> completion, list matches, then longest common part, then all

set noswapfile
set nonu

" disabling bc it is better to map Caps Lock to Esc. Sometimes jk does not
" work, and with mapping I am disabling Caps Lock, so it is win-win situation. 
" https://www.reddit.com/r/vim/comments/5c9dxg/using_jk_as_escaping_to_normal_mode/
" https://gist.github.com/rayaxiom/2689488#gistcomment-2186451
" Works in Ubuntu WSL
"inoremap jk <Esc>

"do not enable as it causes problems in visual mode
"vnoremap jk <Esc>

" better to map Caps Lock as additional Esc, so now Caps Lock is pressed
" and it will work in visual mode
" https://askubuntu.com/questions/574208/deactivate-caps-lock-in-14-04/574253#574253

" set Frame title
" https://github.com/equalsraf/neovim-qt/issues/95#issuecomment-304661632
set title

" On Ubuntu WSL you need this https://github.com/neovim/neovim/wiki/FAQ#how-to-use-the-windows-clipboard-from-wsl
" select with mouse
set mouse=a

" http://vim.wikia.com/wiki/Alternative_tab_navigation
nmap <C-1> 1gt
nmap <C-2> 2gt
nmap <C-3> 3gt
nmap <C-4> 4gt
nmap <C-5> 5gt
nmap <C-6> 6gt
nmap <C-7> 7gt
nmap <C-8> 8gt
nmap <C-9> 9gt
nmap <C-0> 10gt
imap <C-1> <Esc> 1gt
imap <C-2> <Esc> 2gt
imap <C-3> <Esc> 3gt
imap <C-4> <Esc> 4gt
imap <C-5> <Esc> 5gt
imap <C-6> <Esc> 6gt
imap <C-7> <Esc> 7gt
imap <C-8> <Esc> 8gt
imap <C-9> <Esc> 9gt
imap <C-0> <Esc> 10g

set shada=!,'100,<50,s10,h,%

" Preparation for manual session saving
"set sessionoptions-=options
"set sessionoptions-=empty
" tabs are not opened correctly vim vim-rooter, file paths saved
" as relative, probably bc of session option sesdir
" https://www.reddit.com/r/neovim/comments/9lkz9v/nvimqtexe_what_you_do_to_automatically_restore/
"set sessionoptions-=curdir
"set sessionoptions-=buffers
"set sessionoptions-=word          " save and restore ~
"set sessionoptions-=blank         " empty windows
"set sessionoptions-=buffers       " hidden and unloaded buffers, not just those in windows
"set sessionoptions-=curdir        " the current directory
"set sessionoptions-=folds         " manually created folds, opened/closed folds and local
"                                  " fold options
""set sessionoptions-=globals       " global variables that start with an uppercase letter
"                                  " and contain at least one lowercase letter.  Only
"                                  " String and Number types are stored.
"set sessionoptions-=help          " the help window
""set sessionoptions-=localoptions  " options and mappings local to a window or buffer (not
"                                  " global values for local options)
""set sessionoptions-=options       " all options and mappings (also global values for local
"                                  " options)
"set sessionoptions-=resize        " size of the Vim window: 'lines' and 'columns'
"set sessionoptions-=sesdir        " the directory in which the session file is located
"                                  " will become the current directory (useful with
"                                  " projects accessed over a network from different
"                                  " systems)
"set sessionoptions-=slash         " backslashes in file names replaced with forward
"                                  " slashes
"set sessionoptions-=tabpages      " all tab pages; without this only the current tab page
"                                  " is restored, so that you can make a session for each
"                                  " tab page separately
"set sessionoptions-=unix          " with Unix end-of-line format (single <NL>), even when
"                                  " on Windows or DOS
"set sessionoptions-=winpos        " position of the whole Vim window
"set sessionoptions-=winsize       " window sizes
set sessionoptions=globals
" https://www.reddit.com/r/neovim/comments/9lkz9v/nvimqtexe_what_you_do_to_automatically_restore/e77hafy/
command! Session if filereadable(stdpath('config').'/session.vim') | exe 'source '.stdpath('config').'/session.vim'
      \ | else | exe 'Obsession '.stdpath('config').'/session.vim' | endif

" not needed when shell set to cmd on Windows
" Problem with elm format from elm-vim
" https://github.com/ElmCast/elm-vim/issues/80#issuecomment-427222915
" Not needed when w0rp/ale is used
"let g:elm_format_autosave = 1
" https://stackoverflow.com/questions/51272435/vim-autocommand-on-write-pass-full-file-path
" https://vi.stackexchange.com/questions/3060/suppress-output-from-a-vim-autocomand
"autocmd BufWritePost *.elm silent! !elm-format --yes %:p


" https://www.reddit.com/r/neovim/comments/9jc0yl/fruzzy_a_freaky_fast_fuzzy_finder_for_neovim/
" optional - but recommended - see below
let g:fruzzy#usenative = 1

" tell denite to use this matcher by default for all sources
"call denite#custom#source('_', 'matchers', ['matcher/fruzzy'])

" autocmd bc of this https://www.reddit.com/r/neovim/comments/9lkz9v/nvimqtexe_what_you_do_to_automatically_restore/e77jc25/
" Autocommands http://learnvimscriptthehardway.stevelosh.com/chapters/12.html
" https://stackoverflow.com/questions/9281438/syntax-highlighting-doesnt-work-after-restore-a-previous-vim-session
autocmd VimEnter * nested silent! :Session

" <silent> does not work here, need to add silent in command: `silent exe` 
"nmap <Leader>s :Session<CR>

" ignore elm-stuff for tabfind etc.
" https://stackoverflow.com/questions/4296201/vim-ignore-special-path-in-search
" https://stackoverflow.com/questions/30171512/how-to-set-the-root-of-git-repository-to-vi-vim-find-path
" https://vi.stackexchange.com/questions/6508/how-can-i-get-path-from-vim-rooters-function-and-run-it-with-quickruns-makeprg
set wildignore+=**/elm-stuff/**
set wildignore+=**/node_modules/**
" hangs neovim
"let &path .= FindRootDirectory() . "/**"

" Do not replace clipboard with replaced text
" https://stackoverflow.com/questions/54255/in-vim-is-there-a-way-to-delete-without-putting-text-in-the-register/28726374#28726374
" http://vim.wikia.com/wiki/Replace_a_word_with_yanked_text
xnoremap p "_dP

" Ctrl click links
" https://github.com/equalsraf/neovim-qt/issues/345#issuecomment-340215127
nmap <c-leftmouse> gx

" not needed when shell set to cmd on Windows
" https://stackoverflow.com/questions/10969366/vim-automatically-formatting-golang-source-code-when-saving/10969574#10969574
"autocmd FileType rust autocmd BufWritePre <buffer> RustFmt

" does not work in alacritty
" https://neovim.io/doc/user/scroll.html
"map <ScrollWheelUp> <C-Y>
"map <S-ScrollWheelUp> <C-U>
"map <ScrollWheelDown> <C-E>
"map <S-ScrollWheelDown> <C-D>

" needed for rustfmt and elm-format to work on save
" https://github.com/junegunn/vim-plug/issues/539#issuecomment-321412761
"set shell=cmd.exe noshellslash shellquote&vim shellxquote&vim

"hi StatusLine ctermfg=White ctermbg=Black

" not needed?
"set termguicolors

set background=light
"colo summerfruit
colo paramount
" Disable italic in paramount colorscheme
" This could be alternative for any colorscheme https://www.reddit.com/r/vim/comments/5iop1e/disable_all_italic_highlights/dba3kqd/
hi Special ctermfg=243 gui=NONE guifg=#767676
hi Comment ctermfg=243 gui=NONE guifg=#767676

" not needed now, do not know why
" https://vi.stackexchange.com/questions/5567/non-default-colorschemes-cant-be-loaded-setting-them-manually-after-startup-wo
"autocmd VimEnter * colo summerfruit

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" http://vim.wikia.com/wiki/Switch_color_schemes
" https://github.com/xolox/vim-colorscheme-switcher
" https://stackoverflow.com/questions/7331940/how-to-get-the-list-of-all-installed-color-schemes-in-vim

if v:version < 700 || exists('loaded_switchcolor') || &cp
	finish
endif

let loaded_switchcolor = 1

let paths = split(globpath(&runtimepath, 'colors/*.vim'), "\n")

let s:swcolors = map(paths, 'fnamemodify(v:val, ":t:r")')
let s:swskip = ['bespin', 'codeschool', 'darktooth', 'embers', 'gooey', 'greenscreen', 'marrakesh', 'redscreen', 'royal', 'solarized', 'blue', 'darkblue', 'desert', 'elflord', 'evening', 'industry', 'koehler', 'murphy', 'pablo', 'peachpuff', 'ron', 'slate', 'torte', 'apathy'] + ['chalk']
let s:swindex = 0

function! SwitchColor(swinc)
	let s:swindex += a:swinc
	let i = s:swindex % len(s:swcolors)

	" in skip list
	if (index(s:swskip, s:swcolors[i]) == -1)
	      execute "colorscheme " . s:swcolors[i]
	      set background=light
	      set termguicolors
	else
		return SwitchColor(a:swinc)
	endif

	" show current name on screen. :h :echo-redraw
	redraw
	execute "colorscheme"
endfunction

 map <F8>        :call SwitchColor(1)<CR>
imap <F8>   <Esc>:call SwitchColor(1)<CR>

"does not work
" map <F7>      :call SwitchColor(-1)<CR>
"imap <F9> <Esc>:call SwitchColor(-1)<CR>

