library(fgeo.tool)
library(fs)
library(tidyverse, warn.conflicts = FALSE)


input_dir <- dirname(example_path("first_census/census.xlsx"))
output_dir <- tempdir()

# Process one or more workbooks (stored in input_dir)
dir(input_dir, "xlsx")
xl_sheets_to_csv(input_dir, output_dir, first_census = TRUE)
# You get one .csv per workbook
dir(output_dir, "csv")

# Read all .csv into a single dataframe
combo <- csv_to_df(output_dir)
combo

# Read data of tree positions
where_dir <- example_path("first_census/position.csv")
where <- read_csv(where_dir)

# Compare
where
select(combo, quadrat, tag)

# Create a variable that we can later use to merge the two datasets
combo <- mutate(combo, PtID = paste0(quadrat, tag))
combo2 <- left_join(combo, where)
# Reorganize columns for easier visualization
combo2 <- select(combo2, PtID, East, North, date, everything())
combo2

set_brk <- function(max, by) seq(0, max, by)
set_lim <- function(max) c(0, max)

xmax <- 560
ymax <- 360
ggplot(combo2, aes(East, North, color = date)) + 
  geom_point() +
  coord_equal() +
  scale_x_continuous(minor_breaks = set_brk(xmax, 20), limits = set_lim(xmax)) +
  scale_y_continuous(minor_breaks = set_brk(ymax, 20), limits = set_lim(ymax)) +
  theme_bw()

filter(combo2, is.na(date))
