project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

source_manifest <- readr::read_csv(project_file("data", "sources.csv"), show_col_types = FALSE)

verify_sources <- function(manifest = source_manifest, root = project_file()) {
  paths <- file.path(root, manifest$path)
  if (any(!file.exists(paths))) {
    stop("Missing source: ", paste(manifest$path[!file.exists(paths)], collapse = ", "))
  }
  observed <- vapply(paths, digest::digest, "", algo = "sha256", file = TRUE)
  mismatch <- is.na(manifest$sha256) | unname(observed) != manifest$sha256
  if (any(mismatch)) {
    stop("Checksum mismatch: ", paste(manifest$path[mismatch], collapse = ", "))
  }
  invisible(TRUE)
}

# The maintained dp-data export has one row per participant.
read_polardata <- function(path = project_file("data", "raw", "polardata.tab")) {
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

read_indices <- function(path = project_file("data", "raw", "poll_indices.tab")) {
  indices <- readr::read_tsv(path, col_types = "dcdcccd")
  readr::stop_for_problems(indices)
  stopifnot(nrow(indices) == 129, !anyNA(indices), !anyDuplicated(indices[c("dpnum", "att_index")]))
  indices
}
