use_file_from_template <- function(template, package, git_ignore, build_ignore) {
  usethis::use_template(template = template, package = package)
  if (git_ignore) {
    usethis::use_git_ignore(template = template)
  }
  if (build_ignore) {
    usethis::use_build_ignore(template = template)
  }
}

use_tmp_r <- function() {
  use_file_from_template(
    "tmp.R", package = "fgeo.templates", git_ignore = TRUE, build_ignore = TRUE
  )
}
use_tmp_rmd <- function() {
  use_file_from_template(
    "tmp.Rmd", 
    package = "fgeo.templates", 
    git_ignore = TRUE, 
    build_ignore = TRUE
  )
}
