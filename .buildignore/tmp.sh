git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Refactor"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

mv ./R/suffix_match.R ../fgeo.map/R/suffix_match.R
mv ./tests/testthat/test-suffix_match.R ../fgeo.map/tests/testthat/test-suffix_match.R
