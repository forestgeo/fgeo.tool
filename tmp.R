# xxx test
# xxx check why last example in tmp.R is wrong
# xxx write an s3 generic that checks the class of each table (vft, cns, and expects variables accordingly)
# xxx document
# xxx write a helper to convert from A and D to alive and dead
# xxx use wood = tree with census tables
# xxx use wood = stem with vft
# xxx remove stem_status


library(fgeo.utils)


# cns stem
x <- bciex::bci12s7mini
unique(x$status)

result <- filter_status(x, wood = "stem", .status = c("A", "G"))
unique(result$status)

result <- filter_status(x, wood = "stem", .status = "D", exclude = TRUE)
unique(result$status)
# Shortcut
result <- stem_not_dead(x)
unique(result$status)



# cns tree
x <- bciex::bci12t7mini

result <- filter_status(x, wood = "tree", .status = "alive")
unique(result$status_tree)

# # Fails if the .status provided isn't present in the data (here no tree is dead)
# \dontrun{
# result <- filter_status(x, wood = "tree", .status = "dead")
# unique(result$status_tree)
# }
# Shortcut
result <- tree_not_dead(x)
unique(result$status_tree)



# vft stem
x <- bciex::bci12vft_mini
result <- filter_status(x, wood = "tree", .status = "dead")
unique(result$status_tree)
result[1:3, "status_tree"]

# vft tree
result <- filter_status(x, wood = "tree", .status = "dead", exclude = TRUE)
unique(result$status_tree)
result[1:3, "status_tree"]
# Shortcut
result <- tree_not_dead(x)
unique(result$status_tree)
result[1:3, "status_tree"]











# previous ----------------------------------------------------------------

x <- tibble::tibble(status = LETTERS[1:4])
filter_status(x, wood = "stem", .status = c("B", "C"))
filter_status(x, wood = "stem", .status = "D")
filter_status(x, wood = "stem", .status = "D", exclude = TRUE)
# Shortcut
stem_not_dead(x)

x <- bciex::bci12vft_mini
result <- filter_status(x, wood = "tree", .status = "dead")
result[1:3, "status_tree"]

result <- filter_status(x, wood = "tree", .status = "dead", exclude = TRUE)
result[1:3, "status_tree"]

# Shortcut
result <- tree_not_dead(x)
result[1:3, "status_tree"]
