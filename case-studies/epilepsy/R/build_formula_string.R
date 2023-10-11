# create formula string for each model ####
build_formula_string <- function(row, ...){
  outcome = row[["outcome"]]
  # prior names
  priornames = row[["priors"]]
  # which cells in the row are not "family" and non-empty?
  in_id <- c(which(!(names(row) %in% c("outcome", "family", "prior", "priors")) & row != ""))
  # cells that are included in the formula
  covars <- row[in_id]
  # extract levels for formula
  covars <- as.character(unlist(covars))
  # paste formula
  formula1 = paste(outcome, "~", paste(covars, collapse = "+")) 
  # build name
  out <- formula1
}