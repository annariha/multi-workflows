# create brms formulas ####
build_brms_formula <- function(row, ...){
  outcome = row[["outcome"]]
  fam = as.character(unlist(row["family"]))
  # which cells in the row are not "family", "model_name" and non-empty?
  in_id <- c(which(!(names(row) %in% c("outcome", "family", "prior", "priors", "model_name")) & row != ""))
  # cells that are included in the formula
  covars <- row[in_id]
  # extract levels for formula
  covars <- as.character(unlist(covars))
  # paste formula
  formula_str = paste(outcome, "~", paste(covars, collapse = "+")) 
  # turn string into formula 
  formula = brms::brmsformula(as.formula(formula_str), family=fam)
  out <- formula 
} 