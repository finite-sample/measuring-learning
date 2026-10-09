validate_item_pair <- function(pre, post) {
  stopifnot(
    is.matrix(pre), is.matrix(post), identical(dim(pre), dim(post)),
    ncol(pre) >= 3L, nrow(pre) > 0L,
    all(is.na(pre) | pre %in% c(0, 1)), all(is.na(post) | post %in% c(0, 1))
  )
}

# PRE is log knowledge; CHANGE is its change. Baseline mean zero and variance
# one fix the scale. Matched items share slopes and intercepts across waves.
fit_joint_irt <- function(pre, post, guess = 0, max_cycles = 1500) {
  validate_item_pair(pre, post)
  items <- ncol(pre)
  stopifnot(length(guess) %in% c(1L, items), all(is.finite(guess)), all(guess >= 0 & guess < 1))
  data <- cbind(pre, post)
  colnames(data) <- paste0("item", seq_len(2 * items))
  model <- paste0(
    "PRE = 1-", 2 * items, "\nCHANGE = ", items + 1, "-", 2 * items,
    "\nCOV = PRE*CHANGE, CHANGE*CHANGE\nMEAN = CHANGE"
  )
  guesses <- rep(rep(guess, length.out = items), 2)
  parameters <- mirt::mirt(data, model, itemtype = "2PL", guess = guesses, pars = "values", verbose = FALSE)
  parameters$value[parameters$name == "COV_22"] <- 0.15
  parameters$value[parameters$name == "MEAN_2"] <- 0.4
  parameters$lbound[parameters$name == "COV_22"] <- 0.0025
  active <- parameters$name %in% c("a1", "a2") & parameters$est
  parameters$lbound[active] <- 0.2
  parameters$ubound[active] <- 5
  constraints <- unlist(lapply(seq_len(items), function(j) {
    first <- parameters$item == paste0("item", j)
    second <- parameters$item == paste0("item", j + items)
    list(
      parameters$parnum[(first & parameters$name == "a1") | (second & parameters$name %in% c("a1", "a2"))],
      parameters$parnum[(first | second) & parameters$name == "d"]
    )
  }), recursive = FALSE)
  warnings <- character()
  change_grid <- seq(-2, 3, length.out = 61)
  for (attempt in seq_len(3)) {
    fit <- withCallingHandlers(
      mirt::mirt(
        data, model,
        itemtype = "2PL", guess = guesses, pars = parameters, constrain = constraints,
        technical = list(
          NCYCLES = max_cycles,
          customTheta = as.matrix(expand.grid(seq(-6, 6, length.out = 41), change_grid))
        ),
        accelerate = "none", verbose = FALSE
      ),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    coefficients <- mirt::coef(fit, simplify = TRUE)
    change_mean <- coefficients$means[2]
    change_sd <- sqrt(coefficients$cov[2, 2])
    outside <- stats::pnorm(min(change_grid), change_mean, change_sd) +
      stats::pnorm(max(change_grid), change_mean, change_sd, lower.tail = FALSE)
    if (outside < 1e-6) break
    parameters <- mirt::mod2values(fit)
    change_grid <- seq(
      min(min(change_grid), change_mean - 6 * change_sd),
      max(max(change_grid), change_mean + 6 * change_sd),
      by = 1 / 12
    )
  }
  if (outside >= 1e-6) stop("Change distribution extends beyond the integration grid")
  list(
    items = coefficients$items, mean = coefficients$means, covariance = coefficients$cov,
    converged = mirt::extract.mirt(fit, "converged"), warnings = unique(warnings),
    iterations = mirt::extract.mirt(fit, "iterations"), quadrature_tail = outside
  )
}

# Integrate gain itself, not the gain between two posterior mean abilities.
# Intervals condition on the fitted population and item parameters.
posterior_irt_learning <- function(model, pre, post, nodes = 41) {
  validate_item_pair(pre, post)
  stopifnot(nrow(model$items) == 2 * ncol(pre), nodes >= 5L)
  quadrature <- statmod::gauss.quad.prob(nodes, dist = "normal")
  pairs <- expand.grid(seq_len(nodes), seq_len(nodes))
  standard <- cbind(quadrature$nodes[pairs[[1]]], quadrature$nodes[pairs[[2]]])
  theta <- sweep(standard %*% chol(model$covariance), 2, model$mean, `+`)
  prior <- quadrature$weights[pairs[[1]]] * quadrature$weights[pairs[[2]]]
  linear <- sweep(theta %*% t(model$items[, c("a1", "a2")]), 2, model$items[, "d"], `+`)
  probability <- sweep(sweep(stats::plogis(linear), 2, 1 - model$items[, "g"], `*`), 2, model$items[, "g"], `+`)
  probability <- pmin(pmax(probability, 1e-15), 1 - 1e-15)
  answers <- cbind(pre, post)
  correct <- (!is.na(answers) & answers == 1) * 1
  incorrect <- (!is.na(answers) & answers == 0) * 1
  log_weight <- correct %*% t(log(probability)) + incorrect %*% t(log1p(-probability))
  log_weight <- sweep(log_weight, 2, log(prior), `+`)
  log_weight <- log_weight - apply(log_weight, 1, max)
  weight <- exp(log_weight)
  weight <- weight / rowSums(weight)
  initial <- exp(theta[, 1])
  later <- exp(theta[, 1] + theta[, 2])
  gain <- later - initial
  order_gain <- order(gain)
  limits <- t(apply(weight[, order_gain, drop = FALSE], 1, function(w) {
    cumulative <- cumsum(w)
    vapply(c(0.025, 0.975), function(p) gain[order_gain[which(cumulative >= p)[1]]], numeric(1))
  }))
  tibble::tibble(
    estimate = as.vector(weight %*% gain), lower = limits[, 1], upper = limits[, 2],
    initial = as.vector(weight %*% initial), later = as.vector(weight %*% later),
    probability_positive = as.vector(weight %*% (gain > 0)),
    items_pre = rowSums(!is.na(pre)), items_post = rowSums(!is.na(post))
  )
}
