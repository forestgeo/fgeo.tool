# TODO: 1. DONE. Create vectors containig column names for each key sheet.
# TODO: 2. DONE Create function that creates a 0-row dataframe given its column name
# TODO: 3. DONE Add missing sheets to imported sheets' list.

# input_dir <- here::here("inst/issues/iss33/1")
# output_dir <- tempdir()
input_dir <- here::here("inst/issues/iss33/2")
output_dir <- here::here("inst/issues/iss33/out")
xl_sheets_to_csv(input_dir, output_dir)


# Ask Jess ----------------------------------------------------------------

# Sheets with the same names can have different column names. Is this ok
