git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Refactor"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

mv ./R/name_dfs.R ../fgeo.misc/R/name_dfs.R
mv ./tests/testthat/test-name_dfs.R ../fgeo.misc/tests/testthat/test-name_dfs.R
