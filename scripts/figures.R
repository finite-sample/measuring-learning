purrr::walk(list.files("R", full.names = TRUE), source)
dir.create("figs", showWarnings = FALSE)
read_tab <- \(name) readr::read_csv(file.path("tabs", name), show_col_types = FALSE)

proxies <- c(
  observed_gain = "Observed gain", x2 = "Post-process knowledge",
  x2_residualized = "Baseline-residualized post score"
)
forms <- c(
  proportional = "The knowledgeable learn more", additive = "Learning unrelated to initial knowledge",
  catch_up = "The less knowledgeable learn more"
)

# Figure 1. Correlation of each proxy with true gain across simulated processes
# whose observable features fall within the marginal ranges across polls.
worlds <- read_tab("simulations.csv") |>
  dplyr::filter(plausible) |>
  tidyr::pivot_longer(dplyr::all_of(names(proxies)), names_to = "proxy", values_to = "correlation") |>
  dplyr::summarise(
    estimate = stats::median(correlation),
    lower = stats::quantile(correlation, 0.05),
    upper = stats::quantile(correlation, 0.95),
    worlds = dplyr::n(),
    .by = c(learning_form, proxy)
  ) |>
  dplyr::mutate(
    proxy = factor(proxies[proxy], rev(proxies)),
    panel = paste0(forms[learning_form], "\n(", worlds, " worlds)"),
    panel = factor(panel, unique(panel[order(match(learning_form, names(forms)))]))
  )
p <- ggplot2::ggplot(worlds, ggplot2::aes(estimate, proxy)) +
  geom_zero() +
  geom_estimate() +
  ggplot2::facet_grid(panel ~ .) +
  ggplot2::labs(x = "Correlation with true knowledge gain (median, 5th-95th percentile)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0, hjust = 0))
save_evidence(p, "figs/simulations", width = 6.5, height = 4.2)

# Figure 2. Above-median education and learning proxies, by poll.
learners <- read_tab("who_learns.csv") |>
  dplyr::filter(proxy != "Post-process knowledge") |>
  dplyr::mutate(
    pooled = pollname == "Pooled estimate",
    row = forcats::fct_reorder(pollname, dplyr::if_else(pooled, -Inf, estimate), .fun = max),
    proxy = factor(
      dplyr::recode(proxy, "Post-process knowledge given initial" = "Post-process knowledge\ngiven initial"),
      c("Observed gain", "Post-process knowledge\ngiven initial")
    )
  )
p <- ggplot2::ggplot(learners, ggplot2::aes(estimate, row)) +
  geom_zero() +
  ggplot2::geom_hline(yintercept = 1.5, colour = "grey75", linewidth = 0.3) +
  geom_estimate() +
  ggplot2::facet_grid(. ~ proxy) +
  ggplot2::scale_x_continuous(breaks = c(-0.2, 0, 0.2, 0.4)) +
  ggplot2::labs(x = "Above-median education difference (95% intervals)", y = NULL) +
  theme_evidence()
save_evidence(p, "figs/who_learns", width = 6.5, height = 5.5)
