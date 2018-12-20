git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Refactor"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

# mv ./R/fill_na.R ../fgeo.misc/R/fill_na.R
# mv ./tests/testthat/test-fill_na.R ../fgeo.misc/tests/testthat/test-fill_na.R
