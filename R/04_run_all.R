source("R/00_common.R")
load_analysis()

steps <- list(
  simulations = run_simulations, estimation = run_estimation,
  empirical = run_empirical, figures = run_figures, manuscript = render_manuscript
)
requested <- commandArgs(trailingOnly = TRUE)
if (!length(requested)) requested <- names(steps)
if (any(!requested %in% names(steps))) stop("Unknown stage: ", paste(setdiff(requested, names(steps)), collapse = ", "))
for (stage in requested) {
  message("Running ", stage)
  steps[[stage]]()
}
