purrr::walk(list.files("R", pattern = "^[a-z].*\\.R$", full.names = TRUE), source)

verify_sources()
polardata <- read_polardata()
indices <- read_indices()
dir.create("tabs", showWarnings = FALSE)
write_tab <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

features <- poll_features(polardata)
write_tab(features, "poll_features.csv")

simulations <- c("proportional", "additive", "catch_up") |>
  purrr::map(\(form) simulate_grid(draws = 4000, n = 300, learning_form = form)) |>
  purrr::list_rbind() |>
  dplyr::mutate(plausible = looks_like_a_poll(dplyr::pick(dplyr::everything()), features))
write_tab(simulations, "simulations.csv")

write_tab(who_learns(polardata), "who_learns.csv")

attitudes <- index_regressions(polardata, indices)
write_tab(attitudes, "attitudes.csv")
write_tab(summarise_attitudes(attitudes), "attitudes_summary.csv")

questionnaires <- compare_questionnaires()
write_tab(questionnaires, "questionnaires.csv")
write_tab(
  paired_score_differences(questionnaires, c("replicate", "items", "difficulty", "learning_form")),
  "questionnaire_differences.csv"
)
domains <- compare_domains()
write_tab(domains, "domains.csv")
write_tab(paired_score_differences(domains, c("replicate", "coverage", "exposure")), "domain_differences.csv")
write_tab(piecewise_scores(seq(0, 1, length.out = 301)), "piecewise.csv")
write_tab(piecewise_moments(), "piecewise_moments.csv")
validation <- validate_item_subsets(read_item_validation())
write_tab(validation$results, "item_validation.csv")
write_tab(validation$partitions, "item_partitions.csv")
write_tab(validation$samples, "item_validation_samples.csv")
write_tab(summarise_item_validation(validation$results), "item_validation_summary.csv")
