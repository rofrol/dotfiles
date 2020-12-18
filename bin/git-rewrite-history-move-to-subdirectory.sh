#!/bin/bash

# https://stackoverflow.com/questions/4042816/how-can-i-rewrite-history-so-that-all-files-except-the-ones-i-already-moved-ar/4042965#4042965
# https://unix.stackexchange.com/questions/280217/how-to-replay-git-repository-history-into-subdirectory
# There is also git-filter-repo https://github.com/newren/git-filter-repo https://stackoverflow.com/questions/61420660/how-to-use-git-filter-repo-to-merge-one-repo-as-subdirectory-into-another but I could not move root dir to subdir

git filter-branch -f --prune-empty --tree-filter '
dir="subdir_name"
if [ ! -e "$dir" ]
then
    mkdir -p "$dir"
    git ls-tree --name-only $GIT_COMMIT | xargs -I files mv files "$dir"
fi'
