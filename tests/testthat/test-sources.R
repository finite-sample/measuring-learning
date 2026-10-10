test_that("sparse attitude columns retain numeric values beyond the guessing sample", {
  path <- tempfile(fileext = ".tab")
  on.exit(unlink(path))
  d <- tibble::tibble(
    X = seq_len(6006), pollid = rep(seq_len(21), each = 286), caseid = seq_len(6006),
    dpnum = pollid, pollname = as.character(pollid), t1know = 0.2, t2know = 0.4,
    bettered = FALSE, attitude = replace(rep(NA_real_, 6006), 2, 0.5)
  )
  readr::write_tsv(d, path)
  expect_warning(actual <- read_polardata(path), NA)
  expect_type(actual$attitude, "double")
  expect_equal(actual$attitude[2], 0.5)
})

test_that("source hashes reject modified and missing upstream exports", {
  expect_true(verify_sources())
  root <- withr::local_tempdir()
  path <- file.path(root, "fixture.tab")
  writeLines("original", path)
  hash <- digest::digest(path, algo = "sha256", file = TRUE)
  manifest <- tibble::tibble(upstream_path = "fixture.tab", sha256 = hash)
  expect_true(verify_sources(manifest, root))
  writeLines("modified", path)
  expect_error(verify_sources(manifest, root), "Checksum mismatch: fixture.tab")
  unlink(path)
  expect_error(verify_sources(manifest, root), "Missing source: fixture.tab")
})

test_that("conflicting participant IDs cannot silently enter the analysis", {
  path <- tempfile(fileext = ".tab")
  on.exit(unlink(path))
  d <- tibble::tibble(
    X = seq_len(22), pollid = c(seq_len(21), 21), caseid = 1,
    dpnum = pollid, pollname = as.character(pollid),
    t1know = 0.2, t2know = c(rep(0.4, 21), 0.5), bettered = FALSE
  )
  readr::write_tsv(d, path)
  expect_error(read_polardata(path), "participant")
})


test_that("sources are read directly from the configured dp_data checkout", {
  root <- withr::local_tempdir()
  withr::local_envvar(DP_DATA_ROOT = root)
  expect_identical(source_path("items"), file.path(root, "output/analysis/analysis_items.parquet"))
  expect_error(source_path("unknown"), "Unknown source")
})
