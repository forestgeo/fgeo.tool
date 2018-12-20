git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Refactor"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

# mv ./R/flag_if_group.R ../fgeo.analyze/R/flag_if_group.R
# mv ./tests/testthat/test-flag_if_group.R ../fgeo.analyze/tests/testthat/test-flag_if_group.R
