e src/main.rs
16sp | term
vs | term
" When using `--` in `call`, run `call` after opening `main.rs`
call jobsend(b:terminal_job_id, "cargo watch -x 'test tests::test_almostIncreasingSequence -- --nocapture --color always --exact'\n")
" Go to the end https://stackoverflow.com/questions/17012308/move-cursor-to-end-of-file-in-vim/25401651#25401651
$
wincmd k
stopinsert
