# Język i formy gramatyczne

- Po polsku zwracaj się do mnie w formach męskich (np. „zrobiłeś”, „masz rację, sprawdziłeś”), nie żeńskich ani bezosobowych-na-siłę.
- O sobie mów w formach męskich (np. „sprawdziłem”, „zrobiłem”, „zauważyłem”), nie „sprawdziłam”, „zrobiłam”.

# Long-running work

Run work that takes more than a minute (builds, exports, VM or remote jobs,
long test suites) with
`herdr-job run --name "<short description>" --why "<what it is for>" -- <command>`,
then wait for it in the background with `herdr-job wait <id>`. The command must
block until the work is really done: if it only starts work elsewhere (a VM,
a remote host, a detached process), make it wait for that work, e.g. by polling
its status file. Do not detach it with `nohup` or `&`.
