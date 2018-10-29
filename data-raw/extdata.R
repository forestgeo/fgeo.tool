luquillo_tree5_random <- fgeo.data::luquillo_tree5_random
save(luquillo_tree5_random, file = "inst/extdata/rdata/tree5.RData")

luquillo_tree6_random <- fgeo.data::luquillo_tree6_random
save(luquillo_tree6_random, file = "inst/extdata/rdata/tree6.RData")

readr::write_rds(luquillo_tree5_random, path = "inst/extdata/rds/tree5.rds")
readr::write_rds(luquillo_tree6_random, path = "inst/extdata/rds/tree6.rds")
