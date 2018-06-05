# xxx cont here
# FIXME: Fix _pkgdown: 
#   * Review funs clasification.
#   * fgeo_* appears in multiple places.



# SPECIFIC TO FGEO --------------------------------------------------------

# fgeo.tool: SPECIFIC TO FGEO, DEPENDENT ----------------------------------

# add_*(): Add columns to a dataframe
# georeference: position stems in a forest plot
# TODO: Maybe change to add_plot_*()
add_var  # TODO: Don't export
add_col_row
add_col_row2
add_hectindex
add_index
add_lxly
add_quad
add_qxqy
add_subquad
add_status_tree

# Construct fgeo datasets
fgeo_elevation
fgeo_habitat



# Mutate/transform dataframe
recode_subquad





# Operate on rows (filter/keep/discard/subset rows)
row_collapse_censusid  ## Can move to base

row_discard_twice_dead  ## Depends on dplyr

# Maybe remove or not export



# Filter dataframes
filter_status
drop_dead_stem
drop_dead_tree

## -- rename keep to pick: use pick and drop instead of keep and drop.

# Operate on rows, Miscellaneas
collapse_censusid()



# read fgeo data
# Operate on column types
# TODO: Add more compleate read_*() functions.
# TODO: Maybe rename to col_types_*() or types_*()
type_fgeo
type_taxa
type_vft
read_fgeo



# dependencies. GENERAL -- NOT SPECIFIC TO FGEO -----------------------------------------

# Read/write; inputs and outputs; to/from file/data
# Rename all of these functions to the format input_to_output().
# Find common suffix?
# replace _lst by s? e.g. csv_to_df_lst() by csv_to_dfs()
# replace _ls by _lst? 
csv_to_df
csv_to_df_lst
files_to_df
# rename to end with output, something like df_to_csv() or dfs_to_csvs()
ls_csv_df
ls_join_df
ls_list_spreadsheets
xl_sheets_to_csv
xl_sheets_to_df
xl_sheets_to_output
xl_sheets_to_xl
xl_to_df
xl_to_df_lst






# dataframe mutate/transform/edit/sanitize
# Depends on rlang
lookup
# Depends on purrr
replace_null


# Filter dataframes
pick_recensus  ## renamed from to_recensus
pick_top



# dependencies, developers ------------------------------------------------

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











# REMOVE
datasets
top1quad
top4quad

