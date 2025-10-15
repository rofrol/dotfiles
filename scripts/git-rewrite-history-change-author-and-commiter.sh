#!/bin/bash

# https://stackoverflow.com/questions/750172/how-to-change-the-author-and-committer-name-and-e-mail-of-multiple-commits-in-gi/750182#750182

git filter-branch --env-filter '
OLD_EMAIL="your-old-email@example.com"
CORRECT_NAME="Your Correct Name"
CORRECT_EMAIL="your-correct-email@example.com"
if [ "$GIT_COMMITTER_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_COMMITTER_NAME="$CORRECT_NAME"
    export GIT_COMMITTER_EMAIL="$CORRECT_EMAIL"
fi
if [ "$GIT_AUTHOR_EMAIL" = "$OLD_EMAIL" ]
then
    export GIT_AUTHOR_NAME="$CORRECT_NAME"
    export GIT_AUTHOR_EMAIL="$CORRECT_EMAIL"
fi
' --tag-name-filter cat -- --branches --tags

# to get all authors and commiters
# https://stackoverflow.com/questions/9597410/list-all-developers-on-a-project-in-git/36325643#36325643
git log --pretty="%an %ae%n%cn %ce" | sort | uniq

# to remove backup
# git for-each-ref --format="%(refname)" refs/original/ | xargs -n 1 git update-ref -d
# https://gist.github.com/schnell18/c8fbf8fcd268e0d120d2