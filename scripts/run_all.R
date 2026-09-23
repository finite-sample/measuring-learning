purrr::walk(list.files("R", full.names = TRUE), source)

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
