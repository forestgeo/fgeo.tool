git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Refactor"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

mv ./R/drop_twice_dead.R ../fgeo.misc/R/drop_twice_dead.R
mv ./tests/testthat/test-drop_twice_dead.R ../fgeo.misc/tests/testthat/test-drop_twice_dead.R
