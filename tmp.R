# xxx test
# xxx check why last example in tmp.R is wrong
# xxx write an s3 generic that checks the class of each table (vft, cns, and expects variables accordingly)
# xxx document
# xxx write a helper to convert from A and D to alive and dead
# xxx use wood = tree with census tables
# xxx use wood = stem with vft
# xxx remove stem_status



library(dplyr)
library(fgeo.utils)

# Filter by the status of each stem (wood = "stem") -----------------------

# Notice that the variable "status" (or Status) refers the each stem, not tree.

# CENSUS TABLE: STEM TABLE

x <- bciex::bci12s7mini
unique(x$status)

result <- filter_status(x, wood = "stem", .status = "D")
unique(result$status)

result <- filter_status(x, wood = "stem", .status = "D", exclude = TRUE)
unique(result$status)
# Shortcut
result <- stem_not_dead(x)
unique(result$status)

# Warns
result <- filter_status(x, wood = "stem", .status = c("A", "wrong-status"))
unique(result$status)

# CENSUS TABLE: TREE TABLE

# Works exactly in the same way
x <- bciex::bci12t7mini
unique(x$status)

result <- filter_status(x, wood = "stem", .status = "D")
unique(result$status)

# Shortcut
result <- stem_not_dead(x)
unique(result$status)

# VIEWFULL TABLE

# Works exactly in the same way, but notice the following:
# * The variable Status starts with capital S;
# * The values of Status are not, say "A" or "D", but "alive" or "dead".
x <- bciex::bci12vft_mini
unique(x$Status)

result <- filter_status(x, wood = "stem", .status = "alive")
unique(result$Status)

result <- stem_not_dead(x)
result <- stem_not_dead(x, .status = "dead")
unique(result$Status)



# Filter by the status of each tree (wood = "tree") -----------------------

# CENSUS TABLE: STEM TABLE

x <- bciex::bci12s7mini

# Add the variable status_tree, which gives the status of each tree, not stem
x <- add_status_tree(x)
unique(x$status_tree)
# This data has no dead tree, Introducing some.
x$status_tree <- sample(c("dead", "alive"), nrow(x), replace = TRUE)
table(x$status_tree)

result <- filter_status(x, wood = "tree", .status = "alive")
unique(result$status_tree)

result <- filter_status(x, wood = "tree", .status = "dead", exclude = TRUE)
unique(result$status_tree)
# Shortcut
result <- tree_not_dead(x)
unique(result$status_tree)
result[1:5, c("status", "status_tree")]



# CENSUS TABLE: TREE TABLE
# For a tree census-table, each stem represents a tree, so the variable `status`
# already gives the status of the tree, so we don't need to calculate it with
# add_status_tree().

x <- bciex::bci12t7mini
x$status <- sample(c("D", "A"), replace = TRUE)
x <- add_status_tree(x)
table(x$status, x$status_tree)

x %>% filter(status == "A", status_tree == "alive")
x %>% filter(status == "A", status_tree == "dead")
x %>% filter(status == "D", status_tree == "dead")

# Add the variable status_tree, which gives the status of each tree, not stem
x <- add_status_tree(x)
unique(x$status_tree)

# This data has no dead tree, Introducing some.
x$status <- sample(c("D", "D", "D", "A"), nrow(x), replace = TRUE)
x <- add_status_tree(x)
table(x$status_tree)

result <- filter_status(x, wood = "tree", .status = "alive")
unique(result$status_tree)

result <- filter_status(x, wood = "tree", .status = "dead", exclude = TRUE)
unique(result$status_tree)
# Shortcut
result <- tree_not_dead(x)
unique(result$status_tree)
arrange(result, tag, stemID) %>%
  select(tag, stemID, status, status_tree)

[1:5, c("tag", "stemID", "status", "status_tree")]







# CENSUS TABLE: TREE TABLE

x <- bciex::bci12s7mini

# Add the variable status_tree, which gives the status of each tree, not stem
x <- add_status_tree(x)
unique(x$status_tree)

result <- filter_status(x, wood = "tree", .status = "alive")
unique(result$status_tree)

result <- filter_status(x, wood = "tree", .status = "D", exclude = TRUE)
unique(result$status_tree)
# Shortcut
# The data may have no dead tree
result <- tree_not_dead(x)
unique(result$status_tree)















