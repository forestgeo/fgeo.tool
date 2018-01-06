# stem status
viewfull_table <- bciex::bci12vft_mini
unique(stem_not_dead(viewfull_table, .status = "dead")$Status)
tree_table <- bciex::bci12t7mini
unique(stem_not_dead(tree_table, .status = "D")$status)
stem_table <- bciex::bci12s7mini
unique(stem_not_dead(x, .status = "D")$status)

# tree status
viewfull_table <- bciex::bci12vft_mini
unique(tree_not_dead(viewfull_table, .status = "dead")$Status)

tree_table <- bciex::bci12t7mini
tree_table <- dplyr::mutate(
  tree_table,
  status = ifelse(status == "D", "dead", ifelse(status == "A", "alive", status))
)
unique(tree_not_dead(tree_table, .status = "dead")$status)

# Wrong!!!
stem_table <- bciex::bci12s7mini
stem_table <- dplyr::mutate(
  stem_table,
  status = ifelse(status == "D", "dead", ifelse(status == "A", "alive", status))
)
unique(tree_not_dead(stem_table, .status = "dead")$status)
