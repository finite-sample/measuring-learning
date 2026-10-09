purrr::walk(list.files("R", full.names = TRUE), source)
scenarios <- estimator_scenarios()
jobs <- expand.grid(scenario = seq_len(nrow(scenarios)), replicate = seq_len(10))
cores <- as.integer(Sys.getenv("IRT_CORES", "1"))
stopifnot(is.finite(cores), cores >= 1)
results <- parallel::mclapply(seq_len(nrow(jobs)), function(i) {
  job <- jobs[i, ]
  message("Estimator comparison ", i, "/", nrow(jobs), ": ", scenarios$scenario[job$scenario])
  compare_learning_estimators(scenarios[job$scenario, ], job$replicate, 20261009L + i)
}, mc.cores = cores, mc.set.seed = FALSE) |>
  purrr::list_rbind()
readr::write_csv(results, "tabs/estimator_comparison.csv", na = "")
