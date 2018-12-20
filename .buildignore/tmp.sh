git checkout master
git fetch upstream
git reset --hard upstream/master

git remote prune origin

message="Refactor"

git add .
git commit -m "$message"

git add .
git commit --amend -m "$message"

mv ./R/suffix_edge_tag.R ../fgeo.misc/R/suffix_edge_tag.R
mv ./tests/testthat/test-suffix_edge_tag.R ../fgeo.misc/tests/testthat/test-suffix_edge_tag.R
