project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

source_manifest <- readr::read_csv(project_file("data", "sources.csv"), show_col_types = FALSE)

dp_data_root <- function() Sys.getenv("DP_DATA_ROOT", unset = project_file("..", "dp_data"))

source_path <- function(name, root = dp_data_root()) {
  index <- match(name, source_manifest$source)
  if (is.na(index)) stop("Unknown source: ", name)
  file.path(root, source_manifest$upstream_path[index])
}

verify_sources <- function(manifest = source_manifest, root = dp_data_root()) {
  paths <- file.path(root, manifest$upstream_path)
  if (any(!file.exists(paths))) {
    stop("Missing source: ", paste(manifest$upstream_path[!file.exists(paths)], collapse = ", "))
  }
  observed <- vapply(paths, digest::digest, "", algo = "sha256", file = TRUE)
  mismatch <- is.na(manifest$sha256) | unname(observed) != manifest$sha256
  if (any(mismatch)) {
    stop("Checksum mismatch: ", paste(manifest$upstream_path[mismatch], collapse = ", "))
  }
  invisible(TRUE)
}

# The maintained dp_data export has one row per participant.
read_polardata <- function(path = source_path("participants")) {
  data <- readr::read_tsv(path, col_types = readr::cols(
    .default = readr::col_double(), pollname = readr::col_character(), bettered = readr::col_logical()
  ))
  readr::stop_for_problems(data)
  identified <- data[!is.na(data$caseid), c("pollid", "caseid")]
  if (anyNA(data$pollid) || anyDuplicated(identified) || anyNA(data$X) || anyDuplicated(data$X)) {
    stop("Invalid participant IDs (pollid, caseid) or export row IDs (X).")
  }
  data |>
    dplyr::mutate(education_above_median = bettered) |>
    assertr::assert(assertr::within_bounds(0, 1 + 1e-9), t1know, t2know) |>
    assertr::verify(dplyr::n_distinct(dpnum) == 21)
}

read_indices <- function(path = source_path("indices")) {
  indices <- readr::read_tsv(path, col_types = "dcdcccd")
  readr::stop_for_problems(indices)
  stopifnot(nrow(indices) == 129, !anyNA(indices), !anyDuplicated(indices[c("dpnum", "att_index")]))
  indices
}

read_tab <- \(name) readr::read_csv(project_file("tabs", name), show_col_types = FALSE)

num <- \(x, digits = 2) formatC(ifelse(round(x, digits) == 0, 0, x), format = "f", digits = digits)

coef_text <- function(x, digits = 3) {
  out <- sub("^(-?)0\\.", "\\1.", num(x, digits))
  sub("^-(\\.0+)$", "\\1", out)
}

interval <- \(estimate, lower, upper, digits = 3) {
  paste0(coef_text(estimate, digits), " [", coef_text(lower, digits), ", ", coef_text(upper, digits), "]")
}

percent <- \(x) paste0(round(100 * x), "\\%")

count <- \(x) prettyNum(x, big.mark = ",")

poll_label <- function(id) {
  label <- tools::toTitleCase(gsub("-", " ", sub("-([0-9]{4})$", " (\\1)", id)))
  for (acronym in c("UK", "EU", "BTP", "CPL", "NIC", "NIC2", "WTU", "SWEPCO")) {
    label <- gsub(paste0("\\b", tools::toTitleCase(tolower(acronym)), "\\b"), acronym, label)
  }
  sub("Tomorrows Europe", "Tomorrow's Europe", label)
}

theme_evidence <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot"
    )
}

save_evidence <- function(plot, path, width, height) {
  ggplot2::ggsave(paste0(path, ".pdf"), plot, width = width, height = height)
  ggplot2::ggsave(paste0(path, ".png"), plot, width = width, height = height, dpi = 180)
}

# Point estimates and 95% intervals look the same in every figure.
geom_estimate <- function(...) {
  ggplot2::geom_pointrange(
    ggplot2::aes(xmin = lower, xmax = upper),
    shape = 16, size = 0.3, linewidth = 0.45, ...
  )
}

geom_zero <- function() ggplot2::geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3)

cor_if_variable <- function(x, y) {
  if (length(x) < 2 || any(!is.finite(x)) || any(!is.finite(y)) || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(NA_real_)
  }
  stats::cor(x, y)
}

load_analysis <- function(envir = parent.frame()) {
  for (file in c("01_simulations.R", "02_empirical.R", "03_figures.R")) {
    sys.source(project_file("R", file), envir = envir)
  }
}

write_tab <- function(x, name) {
  dir.create(project_file("tabs"), showWarnings = FALSE)
  readr::write_csv(x, project_file("tabs", name), na = "")
}

figure <- function(name) {
  knitr::include_graphics(normalizePath(project_file("figs", paste0(name, ".pdf"))))
}

table_evidence <- function(x, ..., longtable = FALSE) {
  settings <- list(format = "latex", booktabs = TRUE, linesep = "")
  if (longtable) settings$longtable <- TRUE else settings$position <- "H"
  do.call(knitr::kable, c(list(x = x), settings, list(...)))
}

render_manuscript <- function() {
  options(tinytex.install_packages = FALSE)
  rmarkdown::render(project_file("ms/main.Rmd"), knit_root_dir = project_file(), quiet = TRUE)
}

score_colours <- c(
  "Initial score" = "#333333", "Follow-up score" = "#0072B2",
  "Observed gain" = "#D55E00", "True gain" = "#008060"
)
score_shapes <- c("Initial score" = 16, "Follow-up score" = 17, "Observed gain" = 15, "True gain" = 18)
score_linetypes <- c(
  "Initial score" = "solid", "Follow-up score" = "longdash",
  "Observed gain" = "solid", "True gain" = "dotted"
)
