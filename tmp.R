# xxx cont here
count_duplicated



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

# Add columns to a dataframe
# stts_*(): Determine status
add_status_tree

# Maybe remove or not export
# filter status
row_discard_twice_dead
row_filter_status
row_keep_alive_stem
row_keep_alive_tree

# Sanitize
recode_subquad
# Maybe pull_elev or use_elev
restructure_elev


# read fgeo data
# TODO: Add more compleate read_*() functions.
# TODO: Maybe rename to col_types_*() or types_*()
type_fgeo
type_taxa
type_vft


# Create stuff
create_habitat

# Maybe put on hold, don't export right now?
# Fieldforms
fieldforms_header
fieldforms_output
fieldforms_prepare



# GENERAL -- NOT SPECIFIC TO FGEO -----------------------------------------


# maybe remove and reexport anti_join()?
to_recensus


# filter other
row_*
row_top



# fgeo.dev or fgeo.base
# Output condition: Called for side effects: Trow messages, warnings
# Maybe make it s3 (check_unique.numeric, check_unique.data.frame, etc.)
# No external dependencies. Maybe move to fgeo.base
check_unique
check_unique_vector
# output logical
exists_in_pkg


# fgeo.dev or fgeo.base
# help work with fgeo packages, functions, datasets.
example_path
# search objects by names
# TODO: Make it base only
hide_data_of_class
show_data_of_class





# Maybe remove

# Maybe use modify_if() instead and/or don't export
type_ensure

# Sanitize
# Move to fgeo.base?
fill_na

lookup
replace_null

# Work on names
# replace with fgeo.base::name_df_lst()
ls_name_df



# Read/write; inputs and outputs; to/from file/data
csv_to_df
csv_to_df_lst
files_to_df
ls_csv_df
ls_join_df
ls_list_spreadsheets
xl_sheets_to_csv
xl_sheets_to_df
xl_sheets_to_output
xl_sheets_to_xl
xl_to_df
xl_to_df_lst






# REMOVE
datasets
top1quad
top4quad

