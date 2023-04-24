#!/bin/bash
# https://stackoverflow.com/questions/2787253/show-number-of-changed-lines-per-author-in-git/53296520#53296520
# https://gist.github.com/adzenith/fad626b625770771d551c6a34e7d74c7

usage() {
  echo "Usage: $0 BRANCH NUM_DAYS"
  exit 1
}

ds() {
  if [[ "$OSTYPE" == "darwin"* ]]; then # Mac OS
    date -v -$1d +%Y-%m-%d
  else
    date --date="$1 days ago" +%Y-%m-%d
  fi
}

if [ $# -lt 2 ] ; then
  usage
fi

BRANCH=$1
NUM_DAYS=$2
AUTHOR=rofrol

if ! git rev-parse --verify $BRANCH ; then
  echo "Branch with name '$BRANCH' does not exist"
  echo
  usage
fi

echo "Date,LinesAdded,LinesDeleted"
for day in $(seq 1 $NUM_DAYS)
do
  #echo $day
  SINCE=$(ds $day)
  UNTIL=$(ds $(($day - 1)))
  #echo $SINCE
  #git log --since=$SINCE --until=$UNTIL --numstat --pretty="%as,%an <%ae>,%H" | rg -v 'yarn.lock' | sed 's/@.*//g' | awk '{ if (NF == 1){ name = $1}; if(NF == 3) {plus[name] += $1; minus[name] += $2}} END { for (name in plus) {print name": +"plus[name]" -"minus[name]}}' | sort -k2 -gr
  git log --no-merges --since=$SINCE --until=$UNTIL --numstat --pretty="%as,%an <%ae>,%H" | rg -v 'yarn.lock'
done
