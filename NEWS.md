# fgeo.tool 0.0.0.9003

## `xlff_to_csv()`, `xlff_to_xl()` and `xlff_to_dfs()`

* `xlff_to_*()` replace `xl_sheets_to_*()` to make it clear that input is not a generic **xl** file but a specific **xl** file from the **f**ast **f**ield forms software. Also, the ending `_dfs()` replaces `_df()` to more clearly indicate that the output of `xlff_to_dfs()` is not a simple dataframe but a list of dataframes.

* These functions now accept input with missing key-sheets with a warning (#33; David Orwin; @jess-sue).

# fgeo.tool 0.0.0.9002

* Refactor multiple functions (remove, rename, and move to __fgeo.base__).
* `xl_sheets_to_*()` gains the argument `first_census` (Sabrina Russo, #31).
* Organize functions with prefixes (e.g. `add_*()`, `nms_*()).

# fgeo.tool 0.0.0.9001

* Rename to __fgeo.tool__

# fgeo.utils 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
