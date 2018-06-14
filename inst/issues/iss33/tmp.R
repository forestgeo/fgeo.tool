# TODO: 1. DONE. Create vectors containig column names for each key sheet.
# TODO: 2. DONE Create function that creates a 0-row dataframe given its column name
# TODO: 3. DONE Add missing sheets to imported sheets' list.

library(fs)
library(readr)
library(readxl)

# NOT A FIRST CENSUS
# Path to the folder I want to read excel files from
input_dir <- here::here("inst/issues/iss33/1")
input_dir

# Path to the folder I want to write .csv files to
output_dir <- tempdir()

# Output a csv file
xl_sheets_to_csv(input_dir, output_dir)


# Ask Jess ----------------------------------------------------------------

# Sheets with the same names can have different column names. Is this ok
