ci_func <- function(estimate, lci, uci, digits = 2) {
  est <- round(estimate, digits)
  lci <- round(lci, digits)
  uci <- round(uci, digits)
  to_print <- str_glue("{est} (95% CI {lci}, {uci})")
  return(to_print)
}
