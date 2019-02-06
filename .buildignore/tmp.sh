git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

git branch -D release

message="New add_gxgy()"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

