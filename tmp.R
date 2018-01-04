library(handy)
library(ctfs)
library(DependenciesGraphs)
write_fun_dependencies("package:ctfs", "gxgy.to.hectindex", "./hectindex.R")
explore_dependencies("ctfs", "gxgy.to.hectindex")




gxgy_to_hectindex(1, 1)














library(base)
pkg <- "package:base"
fun <- "sum"
file_nm <- "./dependencies_of_base-sum.R"
write_fun_dependencies(pkg, fun, file_nm)




# add_var(), examples:
library(dplyr)
x <- tibble(
  gx = c(0, 500, 987),
  gy = c(10, 233, 490)
)
add_var(x, "lxly", "gxgy")
add_var(x, "index", "gxgy")



gxgy_to_index(1, 1, 20, c(1000, 500))
gxgy_to_index(1, 1)
gxgy_to_index(1, 1, 20)
