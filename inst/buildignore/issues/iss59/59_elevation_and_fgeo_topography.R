library(fgeo.tool)

tian_tong_url <- "https://gist.githubusercontent.com/maurolepore/915fd7905e07ecc67dac5585f2c142c4/raw/db9bc25078f07072b50c561f2fe589cf394fa867/TianTong_Elev.txt"
elev <- read.table(tian_tong_url, header = TRUE)

# Fails
fgeo_topography(elev, gridsize = 20, xdim = 500, ydim = 400)

# Passes
fgeo_topography(elev, gridsize = 20, xdim = 500, ydim = 400, edgecorrect = FALSE)




#> tst <- read.table("TianTong_Elev.txt", header=TRUE)
#> tst <- tst[order(tst$x, tst$y),]
#> elev_df <- fgeo_elevation(tst)
#> fgeo_topography(elev_df, gridsize = 20, xdim = 500, ydim = 400)
#> Error in convex[quad.index] <- midelev - meanelev[quad.index] :
#>   replacement has length zero

library(fgeo.tool)
load_all()

path <- here::here("inst/issues/iss59/TianTong_Elev.txt")


# Fails
fgeo_topography(tst, gridsize = 20, xdim = 500, ydim = 400)

# Passes
fgeo_topography(tst, gridsize = 20, xdim = 500, ydim = 400, edgecorrect = F)

