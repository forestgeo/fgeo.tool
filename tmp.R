# SPECIFIC TO FGEO --------------------------------------------------------

# Add columns to a dataframe
# georeference: position stems in a forest plot
# TODO: Maybe change to add_plot_*()
add_var  # TODO: Change title to reflect it belongs to thig group of functions
add_col_row
add_col_row2
add_hectindex
add_index
add_lxly
add_quad
add_qxqy
add_subquad
# xxx what's missing here?
# add_censusid?
# add_plotid?
# add_plotid?

# Add columns to a dataframe
# Determine status
add_status_tree
# xxx what's missing here?
# TODO: Transform Condit's algorithm into a function that determines status
#       it may be a case_when
to_recensus

# filter status
row_discard_twice_dead
row_filter_status
row_keep_alive_stem
row_keep_alive_tree

# filter other
row_*
row_top

# Sanitize
recode_subquad
restructure_elev

# read fgeo data
type_fgeo
type_taxa
type_vft
# TODO: Add more compleate read_*() functions.

# Create stuff
create_habitat
# Fieldforms
fieldforms_header
fieldforms_output
fieldforms_prepare



# GENERAL -- NOT SPECIFIC TO FGEO -----------------------------------------

# dev
# Output condition: Called for side effects: Trow messages, warnings
check_unique
check_unique_vector
# output logical
exists_in_pkg

# help work with fgeo packages, functions, datasets.
example_path
# search objects by names
find_data_of_class
hide_data_of_class

type_ensure

# summarize
count_duplicated

# Sanitize
fill_na
lookup
replace_null

# Work on names
ls_name_df
nms_*


# Read/write; inputs and outputs; to/from file/data
csv_to_df
csv_to_df_lst
files_to_df
ls_csv_to_df
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

