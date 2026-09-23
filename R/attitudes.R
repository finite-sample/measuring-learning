# Attitude change on a learning proxy and the distance from one's small group,
# one regression per poll and attitude index (the model of the paper's opening
# illustration). The learning coefficient is multiplied by the sign of the
# poll's net change on that index, so a positive value means those who learned
# more moved further in the direction the sample moved, and is expressed per
# standard deviation of the proxy.
index_regressions <- function(polardata, indices) {
  purrr::pmap(indices, function(dpnum, att_index, t1var, t2_t3var, ...) {
    poll <- polardata |>
      dplyr::filter(.data$dpnum == .env$dpnum) |>
      dplyr::transmute(
        group = pollgroup,
        a1 = .data[[t1var]], a2 = .data[[t2_t3var]],
        x1 = t1know, x2 = t2know
      ) |>
      dplyr::filter(!is.na(a1), !is.na(a2), !is.na(x1), !is.na(x2)) |>
      dplyr::mutate(
        group_distance = a1 - (sum(a1) - a1) / (dplyr::n() - 1),
        .by = group
      ) |>
      dplyr::filter(is.finite(group_distance)) |>
      dplyr::mutate(change = a2 - a1, gain = x2 - x1)
    if (nrow(poll) < 50 || stats::sd(poll$change) == 0) {
      return(NULL)
    }
    direction <- sign(mean(poll$change))
    fit_proxy <- function(formula, proxy) {
      fit <- stats::lm(formula, data = poll)
      vc <- sandwich::vcovCL(fit, cluster = ~group, type = "HC1")
      scale <- direction * stats::sd(poll[[proxy]])
      tibble::tibble(estimate = stats::coef(fit)[[proxy]] * scale, std_error = sqrt(vc[proxy, proxy]) * abs(scale))
    }
    dplyr::bind_rows(
      gain = fit_proxy(change ~ gain + group_distance, "gain"),
      x2 = fit_proxy(change ~ x2 + group_distance, "x2"),
      x2_given_x1 = fit_proxy(change ~ x2 + x1 + group_distance, "x2"),
      x1 = fit_proxy(change ~ x1 + group_distance, "x1"),
      .id = "proxy"
    ) |>
      dplyr::mutate(dpnum = dpnum, index = att_index, n = nrow(poll), net_change = mean(poll$change), .before = 1)
  }) |>
    purrr::list_rbind()
}
