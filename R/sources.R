project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

source_manifest <- tibble::tribble(
  ~source, ~path, ~doi, ~md5, ~license,
  "distortions_responses", "data/raw/polardata.tab", "10.7910/DVN/D7G1LO",
  "8e2b8aa45f9ebb71d73d88e2ecf3dbdb", "CC0 1.0",
  "distortions_indices", "data/raw/poll_indices.tab", "10.7910/DVN/D7G1LO",
  "cb262ca053f88d0119757a5c1039ba18", "CC0 1.0"
)

verify_sources <- function(manifest = source_manifest) {
  observed <- unname(tools::md5sum(project_file(manifest$path)))
  if (!identical(observed, manifest$md5)) {
    stop("Checksum mismatch: ", paste(manifest$path[observed != manifest$md5], collapse = ", "))
  }
  invisible(TRUE)
}

# One row per participant. The public release duplicates 217 rows of one poll.
read_polardata <- function(path = project_file("data", "raw", "polardata.tab")) {
  readr::read_tsv(path, show_col_types = FALSE) |>
    dplyr::distinct(dplyr::across(-X), .keep_all = TRUE) |>
    assertr::assert(assertr::within_bounds(0, 1 + 1e-9), t1know, t2know) |>
    assertr::verify(dplyr::n_distinct(dpnum) == 21)
}

read_indices <- function(path = project_file("data", "raw", "poll_indices.tab")) {
  indices <- readr::read_tsv(path, show_col_types = FALSE)
  stopifnot(nrow(indices) == 129)
  indices
}
