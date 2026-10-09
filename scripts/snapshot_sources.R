source("R/sources.R")

root <- Sys.getenv("DP_DATA_ROOT", unset = "../dp-data")
staging <- tempfile("dp-data-snapshot-")
dir.create(staging)
withr::defer(unlink(staging, recursive = TRUE), envir = globalenv())

for (i in seq_len(nrow(source_manifest))) {
  entry <- source_manifest[i, ]
  target <- file.path(staging, entry$path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  status <- system2("git", c(
    "-C", shQuote(root), "show", shQuote(paste0(entry$revision, ":", entry$upstream_path))
  ), stdout = target)
  if (status != 0L) stop("Cannot read pinned dp-data revision from ", root)
}

verify_sources(root = staging)
for (path in source_manifest$path) {
  if (!file.copy(file.path(staging, path), project_file(path), overwrite = TRUE)) {
    stop("Cannot write snapshot: ", path)
  }
}
verify_sources()
message("Verified dp-data snapshot: ", unique(source_manifest$revision))
