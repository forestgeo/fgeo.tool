# TODO --------------------------------------------------------------------

# xxx cont here
# * Replace bci data by luquillo data in map (top1quad by vft_1quad, ...)

# Families ----------------------------------------------------------------

#' @family functions for ForestGEO data.
#' @family functions for fgeo census.
#' @family functions for fgeo vft.
#' @family functions to add columns to dataframes.

# SPECIFIC TO FGEO --------------------------------------------------------

# fgeo.tool: SPECIFIC TO FGEO, DEPENDENT ----------------------------------

# add_*(): Add columns to a dataframe
# Add col, row, hectindex, index, lx, ly, quad, qx, qy, subquad, status_tree.
# Add columns to a ForestGEO dataframe.
# georeference: position stems in a forest plot
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
# TODO: Rename all of these functions to the format input_to_output().
# Find common suffix?
# replace _lst by s? e.g. csv_to_df_lst() by csv_to_dfs()
# replace _ls by _lst? 
csv_to_df



csv_to_df_lst
files_to_df

# TODO: rename to end with output, something like df_to_csv() or dfs_to_csvs()
ls_csv_df
ls_join_df
ls_list_spreadsheets
xlff_to_csv
xlff_to_df
xlff_to_output
xlff_to_xl
xl_to_df
xl_to_df_lst

# Filter dataframes
pick_recensus  ## renamed from to_recensus
pick_top

# Edit dataframes or vectors

lookup
replace_null
conv_unit_at
conv_unit



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

