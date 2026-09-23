# Does a bachelor's degree predict learning? The answer depends on the proxy:
# observed gain alone, or post-process knowledge controlling for initial
# knowledge (equivalently, gain controlling for initial knowledge).
who_learns <- function(polardata) {
  data <- polardata |>
    dplyr::filter(!is.na(educ3), !is.na(t1know), !is.na(t2know)) |>
    dplyr::mutate(ba = as.numeric(educ3 == 1), gain = t2know - t1know)
  specs <- list(
    "Observed gain" = gain ~ ba,
    "Post-process knowledge" = t2know ~ ba,
    "Post-process knowledge given initial" = t2know ~ ba + t1know
  )
  by_poll <- purrr::imap(specs, \(formula, label) {
    data |>
      dplyr::group_split(pollname) |>
      purrr::map(\(poll) {
        fit <- stats::lm(formula, data = poll)
        vc <- sandwich::vcovCL(fit, cluster = poll$pollgroup, type = "HC1")
        tibble::tibble(
          pollname = poll$pollname[[1]], proxy = label,
          estimate = stats::coef(fit)[["ba"]], std_error = sqrt(vc["ba", "ba"])
        )
      }) |>
      purrr::list_rbind()
  }) |>
    purrr::list_rbind()
  pooled <- by_poll |>
    dplyr::group_split(proxy) |>
    purrr::map(\(x) {
      fit <- bayesmeta::bayesmeta(y = x$estimate, sigma = x$std_error, labels = x$pollname, tau.prior = tau_prior)
      tibble::tibble(
        proxy = x$proxy[[1]], pollname = "Pooled estimate",
        estimate = fit$summary["median", "mu"],
        lower = fit$summary["95% lower", "mu"], upper = fit$summary["95% upper", "mu"]
      )
    }) |>
    purrr::list_rbind()
  dplyr::bind_rows(
    dplyr::mutate(by_poll, lower = estimate - 1.96 * std_error, upper = estimate + 1.96 * std_error),
    pooled
  )
}

tau_prior <- \(t) bayesmeta::dhalfnormal(t, scale = 0.1)

# Share of attitude indices on which each proxy predicts moving with the net
# change, and how often it does so at conventional significance.
summarise_attitudes <- function(regressions) {
  regressions |>
    dplyr::summarise(
      indices = dplyr::n(),
      mean_estimate = mean(estimate),
      share_positive = mean(estimate > 0),
      share_significant_positive = mean(estimate / std_error > 1.96),
      share_significant_negative = mean(estimate / std_error < -1.96),
      .by = proxy
    )
}
