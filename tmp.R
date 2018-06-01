# xxx cont here
# FIXME: Run examples
# replace restructure_elev() by fgeo_elev() in fgeo.map
# TODO:
# Check guidelines in adv-r
# Do the same with create_habitat


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



# Construct/structure data/dataframe
# TODO: rename and refactor to as_*() or as_fgeo_*().
create_habitat
fgeo.tool::restructure_elev()




# Mutate dataframe
recode_subquad





# Maybe remove or not export
# filter status
row_discard_twice_dead
row_filter_status
row_keep_alive_stem
row_keep_alive_tree

# Sanitize

# Maybe pull_elev or use_elev
restructure_elev


# read fgeo data
# TODO: Add more compleate read_*() functions.
# TODO: Maybe rename to col_types_*() or types_*()
type_fgeo
type_taxa
type_vft











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






# Sanitize/edit/mutate dataframe
# Depends on rlang
lookup
# Depends on purrr
replace_null








# maybe remove and reexport anti_join()?
to_recensus


# filter other
row_*
row_top







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

