library(tidyverse)
library(fgeo.habitat)
library(fgeo.tool)


cns <- luquillo_top3_sp
spp <- unique(cns$sp)
hab <- luquillo_habitat

tt_lst <- tt_test(cns, spp, hab)
tt_df <- to_df(tt_lst)


# Similar to Daniel's but includes counts
tt_df %>% 
  select(habitat, sp, distribution, stem_count) %>% 
  mutate(habitat  = glue::glue("habitat_{habitat}")) %>% 
  spread(habitat, stem_count, fill = 0) %>% 
  arrange(sp, distribution)

# My favorite
tt_df %>% 
  select(habitat, distribution, sp, stem_count) %>% 
  arrange(habitat, distribution, sp, stem_count)








library(tidyverse)
library(rlang)
library(fgeo.habitat)
library(fgeo.tool)

cns <- luquillo_tree6_random
spp <- unique(cns$sp)
hab <- luquillo_habitat

tt_lst <- tt_test(cns, spp, hab)
tt_df <- to_df(tt_lst)

species <- NULL
# species <- c("SCHMOR", "ALCLAT", "FAROCC")

# Defalut species to NULL to select all species.
species <- species %||% unique(tt_df$sp)
# If species is a character vector, then select only those species.
selected <- tt_df %>% filter(sp %in% species)

selected %>% 
  select(habitat, sp, distribution, stem_count) %>% 
  group_by(distribution, habitat) %>% 
  arrange(habitat, distribution, desc(stem_count)) %>% 
  mutate(rank = row_number()) %>% 
  ggplot(aes(rank, stem_count)) +
  geom_col(aes(fill = sp)) +
  geom_text(aes(y = 0, label = sp), angle = 90, hjust = 0, size = 2) +
  facet_grid(vars(habitat), vars(distribution)) +
  guides(fill = "none")








# TODO:
# * Remove pick_woods()? Or simplify it to be filter(pick_main_stem(.data))
# Check documentation of funs downstream of pick_large_hom_dbh() in fgeo.abundance


# * Make test-fgeo_habitat run faster.






* warn_ and detect_duplicated_treeid: Simplify and test:
  * not use stemid
  * not deal with groups
  * use lowercase treeid in the message or substitute()

# TODO --------------------------------------------------------------------

# xxx cont here

# * remove nms_restore_newvar()



# * Replace bci data by luquillo data in map (top1quad by vft_1quad, ...)
# * Remove internal data functions_priority
# * Rename input_to_output() to begin with from_*(), e.g. from_df_csv

# * Improve interface of input_to_output(from, to, ...)
# Table input / output and find missing pieces.
# input/output:
# * xlsheet
# * xlsheets
# * xlbook
# * xlbooks
# * df
# * dfs
# from/to:
# * file
# * directory
# * df
# * dfs




# Families ----------------------------------------------------------------

#' @family functions for ForestGEO data.
#' @family functions for fgeo census.
#' @family functions for fgeo vft.
#' @family functions to add columns to dataframes.

# SPECIFIC TO FGEO --------------------------------------------------------

# fgeo.tool: SPECIFIC TO FGEO, DEPENDENT ----------------------------------

# add_*(): Add columns to a dataframe
add_col_row
add_col_row2
add_hectindex
add_index
add_lxly
add_quad
add_qxqy
add_subquad

add_status_tree



# Add columns to dataframes
recode_subquad



# Construct fgeo datasets
fgeo_elevation
fgeo_habitat



# Filter (pick and drop) rows of dataframes
drop_twice_dead 
filter_status
drop_dead_stem
drop_dead_tree



# Read or write data
read_fgeo
type_fgeo
type_taxa
type_vft


# Datasets

# dependencies. GENERAL -- NOT SPECIFIC TO FGEO -----------------------------

# Read or write
files_to_df
csv_to_df
csv_to_dfs
xl_to_df
xl_to_dfs
dfs_to_csv
dfs_to_df
xlsheets_to_dfs

xlff_to_output
xlff_to_csv
xlff_to_dfs
xlff_to_xl

# Filter dataframes
pick_recensus  ## renamed from to_recensus
pick_top

# Edit dataframes or vectors

lookup
replace_null
convert_unit_at
convert_unit



# dependencies, developers ------------------------------------------------

# TODO: Move to its own package

# search objects by names
# depends on purrr only
# TODO: Make it base only?
hide_data_of_class
show_data_of_class

# work with namesnames
# Depends on purrr and rlang
nms_*()

# Maybe use modify_if() instead and/or don't export
type_ensure

