run_figures <- function() {
  dir.create(project_file("figs"), showWarnings = FALSE)

  proxies <- c(
    baseline = "Initial knowledge", observed_gain = "Observed gain", x2 = "Post-process knowledge",
    x2_residualized = "Baseline-residualized post score"
  )
  forms <- c(
    proportional = "The knowledgeable learn more", additive = "Learning unrelated to initial knowledge",
    catch_up = "The less knowledgeable learn more"
  )

  # Correlation of each proxy with true gain across simulated processes
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
  save_evidence(p, "figs/simulations", width = 6.5, height = 5)

  # Above-median education and learning proxies, by poll.
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

  curves <- read_tab("piecewise.csv") |>
    tidyr::pivot_longer(c(pre, post, gain, true_gain), names_to = "measure", values_to = "score") |>
    dplyr::mutate(measure = factor(
      measure, c("pre", "post", "gain", "true_gain"),
      c("Initial score", "Follow-up score", "Observed gain", "True gain")
    ))
  curve_symbols <- curves |>
    dplyr::group_by(measure) |>
    dplyr::filter(
      (dplyr::row_number() - 1L) %% 60L ==
        dplyr::if_else(measure %in% c("Initial score", "Observed gain"), 30L, 0L),
      initial > 0
    ) |>
    dplyr::ungroup()
  p <- ggplot2::ggplot(curves, ggplot2::aes(
    initial, score,
    colour = measure, shape = measure, linetype = measure
  )) +
    ggplot2::geom_vline(xintercept = c(1 / 3, 1 / 2), colour = "grey75", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(data = curve_symbols, size = 2.2) +
    ggplot2::scale_colour_manual(values = score_colours) +
    ggplot2::scale_shape_manual(values = score_shapes) +
    ggplot2::scale_linetype_manual(values = score_linetypes) +
    ggplot2::labs(
      x = "True initial knowledge (illustrative units)", y = "Score or gain",
      colour = NULL, shape = NULL, linetype = NULL
    ) +
    theme_evidence() +
    ggplot2::theme(legend.position = "bottom")
  save_evidence(p, "figs/piecewise", 6.5, 3.5)

  controlled <- read_tab("questionnaires.csv") |>
    dplyr::summarise(correlation = median(correlation), .by = c(items, difficulty, learning_form, estimator)) |>
    dplyr::mutate(
      learning_form = factor(learning_form, c("proportional", "additive", "catch_up")),
      difficulty = factor(difficulty, c("easy", "broad")),
      estimator = factor(
        estimator, c("baseline", "observed_gain", "post_score"),
        c("Initial knowledge", "Observed gain", "Follow-up knowledge")
      )
    )
  p <- ggplot2::ggplot(controlled, ggplot2::aes(items, correlation, linetype = estimator)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey75", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(size = 1) +
    ggplot2::facet_grid(learning_form ~ difficulty, labeller = ggplot2::labeller(
      learning_form = c(proportional = "Proportional", additive = "Additive", catch_up = "Catch-up"),
      difficulty = c(easy = "Easy items", broad = "Broader difficulties")
    )) +
    ggplot2::scale_x_continuous(breaks = c(5, 10, 15, 30)) +
    ggplot2::labs(x = "Number of questions", y = "Median correlation with true gain", linetype = NULL) +
    theme_evidence() +
    ggplot2::theme(legend.position = "bottom")
  save_evidence(p, "figs/questionnaires", 6.5, 5)

  validation <- read_tab("item_validation_summary.csv") |>
    dplyr::filter(metric == "post_minus_gain") |>
    dplyr::mutate(poll = poll_label(poll_id), mode = factor(mode, c("random", "easy")))
  p <- ggplot2::ggplot(validation, ggplot2::aes(median, reorder(poll, median))) +
    geom_zero() +
    ggplot2::geom_pointrange(ggplot2::aes(xmin = lower, xmax = upper), linewidth = 0.4) +
    ggplot2::facet_grid(. ~ mode, labeller = ggplot2::labeller(
      mode = c(easy = "Easier predictor items", random = "Random partitions")
    )) +
    ggplot2::labs(x = "Follow-up minus gain: correlation with held-out item gain", y = NULL) +
    theme_evidence()
  save_evidence(p, "figs/item_validation", 6.5, 5.5)
}
