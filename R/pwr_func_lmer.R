pwr_func_lmer <- function(betas = list("int" = 1, "x1" = -5, "x2" = 0),
                          dists = list("x1" = stats::rnorm, "x2" = stats::rbinom, "x3" = stats::rnorm),
                          distpar = list("x1" = list(mean = 0, sd = 1), "x2" = list(size = 1, prob = 0.4), "x3" = list(mean = 1, sd = 2)),
                          N = 25,
                          reps = 1000,
                          alpha = 0.05,
                          var_intr = "x1",
                          grp = "ID",
                          mod = paste0("out ~ x1 + x2 + x3 + (1|", grp, ")"),
                          r_slope = "x1",
                          r_int = "int",
                          n_time = 5,
                          mean_i = 0,
                          var_i = 1,
                          mean_s = 0,
                          var_s = 1,
                          cov_is = 0,
                          mean_r = 0,
                          var_r = 1,
                          cor_mat = diag(2),
                          corvars = list(c("x1", "x3"))) {

  simulate <- function() {
    n <- N * n_time
    data <- data.frame(grp = rep(1:N, each = n_time))
    names(data) <- grp

    # Generate data for correlated variables
    for (cor_group in corvars) {
      if (all(sapply(cor_group, function(x) x %in% names(dists) && identical(dists[[x]], stats::rnorm)))) {
        # Generate correlated normal data for this group
        group_means <- sapply(cor_group, function(var) distpar[[var]]$mean)
        mv_data <- MASS::mvrnorm(N*n_time, mu = group_means, Sigma = cor_mat)
        colnames(mv_data) <- cor_group
        data <- cbind(data, mv_data)
      }
    }

    # Generate data for non-correlated variables
    non_correlated_vars <- setdiff(names(dists), unlist(corvars))
    for (var in non_correlated_vars) {
      params <- distpar[[var]]
      data[[var]] <- do.call(dists[[var]], c(list(N*n_time), params))
    }

    # Random effects
    REff <- MASS::mvrnorm(N, mu = c(mean_i, mean_s), Sigma = rbind(c(var_i, cov_is), c(cov_is, var_s)))  # Using N instead of n_pers
    colnames(REff) <- c(r_int, r_slope)

    data[["rand_int"]] <- rep(REff[, r_int], each = n_time)
    data[["rand_slope"]] <- rep(REff[, r_slope], each = n_time)

    # Model outcome with Gaussian error term
    linear_combination <- betas[[r_int]] + data[["rand_int"]] + (betas[[r_slope]] + data[["rand_slope"]]) * data[[r_slope]]

    # Use Map to add contributions from each beta and corresponding variable
    fixed_effects <- Map(`*`, betas[names(betas) != r_int & names(betas) != r_slope], data[names(betas) != r_int & names(betas) != r_slope])
    linear_combination <- linear_combination + Reduce(`+`, fixed_effects)

    # Model outcome with Gaussian error term
    data$out <- linear_combination + stats::rnorm(n, mean = mean_r, sd = sqrt(var_r))


    fit <- lmerTest::lmer(stats::as.formula(mod), data = data)
    tidy_fit <- broom.mixed::tidy(fit)

    coef_value <- tidy_fit[tidy_fit$term == var_intr, "estimate"]
    p_value <- tidy_fit[tidy_fit$term == var_intr, "p.value"] < alpha

    return(c(coef_value[[1]], p_value[[1]]))
  }

  results <- t(sapply(1:reps, function(x) simulate()))
  sim_results <- data.frame(
    mean_coef = mean(results[, 1]),
    power = mean(results[, 2]) * 100
  )

  return(sim_results)
}


# pwr_func_lmer <- function(betas = list("int" = 1, "x1" = -5, "x2" = 0),
#                           dists = list("x1" = rnorm(N*n_time, 15, 10), "x2" = rbinom(N*n_time, 1, 0.4)),
#                           N = 25,
#                           reps = 1000,
#                           alpha = 0.05,
#                           var_intr = "x1",
#                           grp = "ID",  # New parameter for grouping
#                           mod = paste0("out ~ x1 + x2 + (1|", grp, ")"),  # Modified to use grp
#                           r_slope = "x1",
#                           r_int = "int",
#                           n_time = 5,
#                           mean_i = 0,
#                           var_i = 1,
#                           mean_s = 0,
#                           var_s = 1,
#                           cov_is = 0,
#                           mean_r = 0,
#                           var_r = 1) {
#
#   simulate <- function() {
#     n <- N * n_time
#     data <- data.frame(grp = rep(1:N, each = n_time))
#     names(data) <- grp
#
#     for (var in names(dists)) {
#       data[[var]] <- eval(dists[[var]])
#     }
#
#     # Random effects
#     REff <- MASS::mvrnorm(N, mu = c(mean_i, mean_s), Sigma = rbind(c(var_i, cov_is), c(cov_is, var_s)))  # Using N instead of n_pers
#     colnames(REff) <- c(r_int, r_slope)
#
#     data[["rand_int"]] <- rep(REff[, r_int], each = n_time)
#     data[["rand_slope"]] <- rep(REff[, r_slope], each = n_time)
#
#     # Model outcome with Gaussian error term
#     linear_combination <- betas[[r_int]] + data[["rand_int"]] + (betas[[r_slope]] + data[["rand_slope"]]) * data[[r_slope]]
#
#     # Use Map to add contributions from each beta and corresponding variable
#     fixed_effects <- Map(`*`, betas[names(betas) != r_int & names(betas) != r_slope], data[names(betas) != r_int & names(betas) != r_slope])
#     linear_combination <- linear_combination + Reduce(`+`, fixed_effects)
#
#     # Model outcome with Gaussian error term
#     data$out <- linear_combination + rnorm(n, mean = mean_r, sd = sqrt(var_r))
#
#
#     fit <- lmerTest::lmer(as.formula(mod), data = data)
#     tidy_fit <- broom.mixed::tidy(fit)
#
#     coef_value <- tidy_fit[tidy_fit$term == var_intr, "estimate"]
#     p_value <- tidy_fit[tidy_fit$term == var_intr, "p.value"] < alpha
#
#     return(c(coef_value[[1]], p_value[[1]]))
#   }
#
#
# results <- t(sapply(1:reps, function(x) simulate()))
#   sim_results <- data.frame(
#     mean_coef = mean(results[, 1]),
#     power = mean(results[, 2]) * 100
#   )
#
#   return(sim_results)
# }

