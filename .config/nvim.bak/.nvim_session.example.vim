" https://medium.com/@garoth/neovim-terminal-usecases-tricks-8961e5ac19b9#.wewpz5kgy
nnoremap <silent> <Leader><Space> :GFiles frontend<CR>
"16sp term://bash
16sp | term
"vs term://bash
vs | term
" https://thoughtbot.com/upcase/videos/neovim-sending-commands-to-a-terminal-buffer
call jobsend(b:terminal_job_id, "killTcpListen 1234; rm -rf build; make run env=Local name=App port=1234\n")
" go to the end https://stackoverflow.com/questions/17012308/move-cursor-to-end-of-file-in-vim/25401651#25401651
$
" How to scroll to the bottom?
" call feedkeys("G")
wincmd k
"e frontend/src/Mapdid/App.elm
e src/Mapdid/App.elm
" https://stackoverflow.com/questions/9445273/how-do-i-emulate-a-keypress-inside-a-vim-function/9445742#9445742
call feedkeys("\<Esc>")
