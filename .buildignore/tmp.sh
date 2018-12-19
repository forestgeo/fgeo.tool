git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Prune."

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"
