# TODO: 1. DONE. Create vectors containig column names for each key sheet.
# TODO: 2. DONE Create function that creates a 0-row dataframe given its column name

.key_rcns

.rcns_new_secondary_stems, .rcns_original_stems, .rcns_recruits, .rcns_root


key_recensus_nms <- list(
  .rcns_new_secondary_stems, .rcns_original_stems, .rcns_recruits, .rcns_root
) 
setNames(key_recensus_nms, sort(.key_rcns))
# TODO: Map function created in 2. over all names of all key sheets.



# Ask Jess ----------------------------------------------------------------

# Sheets with the same names can have different column names. Is this ok
