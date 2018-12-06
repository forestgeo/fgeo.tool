# TODO:

# Remove useless roxygen complexity?
#' @rdname flag_if
#' @keywords internal
#' @noRd



# https://github.com/forestgeo/fgeo.tool/issues/96
# * Export functions that are used more than once
# check_crucial_names
# flag_if
# guess_plotdim
# name_dfs
# suffix_match

# * Move functions that are used exactly once.
all other



# Stuff -------------------------------------------------------------------



# TODO:
# Address TODO and FIXME.
# Integrate with fgeo.demography

# In read_censuses():
# TODO: Add checks
# * stop if not all dataframes have same structure.
# * warn if not all dataframes have names that match tree or stem tables



# Appveyor badge ----------------------------------------------------------

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/forestgeo/fgeo.tool?branch=master&svg=true)](https://ci.appveyor.com/project/forestgeo/fgeo.tool)



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
# * list
# from/to:
# * file
# * directory
# * df
# * list




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
csv_df
csv_list
xl_df
xl_list
list_csv
list_df
xlsheets_list

xlff_to_output
xlff_to_csv
xlff_to_list
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

