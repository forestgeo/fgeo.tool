# This file generates internal data: sysdata.rda. Only one sysdata.rda can exist
# in data/. Yet, many data objects can be saved into a single sysdata.rda. This
# file creates all internal data objects

library(readr)



# Functions' priority -----------------------------------------------------

# https://github.com/forestgeo/forestr/issues/32

# This data is on a google sheet owned by Mauro Lepore <maurolepore@gmail.com>
# at https://goo.gl/oiZVho.
# The data were edited by "McMahon, Sean" <mcmahons@si.edu> and Gabriel 
# Arellano <gabriel.arellano.torres@gmail.com>.
# The data were discussed among Sean, Gabriel "Davies, Stuart J." 
# <daviess@si.edu> and Mauro.
# Then, Mauro changed the privilege of Sean and Gabriel over the data so they
# can view it. Should priorities change, Mauro to be notified so he can adapt
# his work accordingly.
# The data were downloaded on the same day, about 30 minutes after the
# discussion finished.

library(tidyverse)



pull_decision <- function(value) {
  priority <- readr::read_delim("./data-raw/iss32_grouped_priority.csv", delim = "\t")
  dplyr::filter(priority, .data$priority == value) %>% pull(decision)
}

functions_priority <- readr::read_csv("./data-raw/iss32_fun_table_edited.csv")

functions_priority <- functions_priority %>% 
  mutate(priority = 
      forcats::fct_collapse(
        decision,    
        a = pull_decision(1),
        b = pull_decision(2),
        c = pull_decision(3),
        d = pull_decision(4)
      )
  )



# Use objects as internal data --------------------------------------------

usethis::use_data(
  functions_priority, 
  internal = TRUE, overwrite = TRUE
)
