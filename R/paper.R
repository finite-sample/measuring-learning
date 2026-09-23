read_tab <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

num <- \(x, digits = 2) formatC(x, format = "f", digits = digits)

coef_text <- function(x, digits = 3) {
  out <- sub("^(-?)0\\.", "\\1.", num(x, digits))
  sub("^-(\\.0+)$", "\\1", out)
}

interval <- \(estimate, lower, upper, digits = 3) {
  paste0(coef_text(estimate, digits), " [", coef_text(lower, digits), ", ", coef_text(upper, digits), "]")
}

percent <- \(x) paste0(round(100 * x), "\\%")

count <- \(x) prettyNum(x, big.mark = ",")
