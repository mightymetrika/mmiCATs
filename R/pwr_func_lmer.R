pwr_func_lmer <- function(betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10),
                          dists = list("x1" = stats::rnorm, "x2" = stats::rbinom, "x3" = stats::rnorm),
                          distpar = list("x1" = list(mean = 0, sd = 1), "x2" = list(size = 1, prob = 0.4), "x3" = list(mean = 1, sd = 2)),
                          N = 25,
                          reps = 1000,
                          alpha = 0.05,
                          var_intr = "x1",
                          grp = "ID",
                          mod = paste0("out ~ x1 + x2 + x3 + (1|", grp, ")"),
                          catsmod = "out ~ x1 + x2 + x3",
                          r_slope = "x1",
                          r_int = "int",
                          n_time = 20,
                          mean_i = 0,
                          var_i = 1,
                          mean_s = 0,
                          var_s = 1,
                          cov_is = 0,
                          mean_r = 0,
                          var_r = 1,
                          cor_mat = NULL,
                          corvars = NULL) {

  #Create ri formula
  ri_formula <- paste0(catsmod, "+ (1|", grp, ")")

  simulate <- function() {
    n <- N * n_time
    sdata <- data.frame(grp = rep(1:N, each = n_time))
    names(sdata) <- grp

    # Check if cor_mat and corvars are not NULL
    if (!is.null(cor_mat) && !is.null(corvars)) {
      # Process the correlated variables
      correlated_data <- lapply(corvars, function(cor_group) {
        if (all(sapply(cor_group, function(x) x %in% names(dists) && identical(dists[[x]], stats::rnorm)))) {
          group_means <- sapply(cor_group, function(var) distpar[[var]]$mean)
          mv_data <- MASS::mvrnorm(N*n_time, mu = group_means, Sigma = cor_mat)
          colnames(mv_data) <- cor_group
          return(mv_data)
        } else {
          return(NULL)
        }
      })
      correlated_data <- do.call(cbind, correlated_data)
      sdata <- cbind(sdata, correlated_data)
    }

    # Generate data for non-correlated variables
    non_correlated_vars <- setdiff(names(dists), unlist(corvars))
    non_correlated_data <- Map(function(var, params) {
      do.call(dists[[var]], c(list(N*n_time), params))
    }, var = non_correlated_vars, params = distpar[non_correlated_vars])
    sdata <- cbind(sdata, non_correlated_data)


    # Random effects
    REff <- MASS::mvrnorm(N, mu = c(mean_i, mean_s), Sigma = rbind(c(var_i, cov_is), c(cov_is, var_s)))  # Using N instead of n_pers
    colnames(REff) <- c(r_int, r_slope)

    sdata[["rand_int"]] <- rep(REff[, r_int], each = n_time)
    sdata[["rand_slope"]] <- rep(REff[, r_slope], each = n_time)

    # Model outcome with Gaussian error term
    linear_combination <- betas[[r_int]] + sdata[["rand_int"]] + (betas[[r_slope]] + sdata[["rand_slope"]]) * sdata[[r_slope]]

    # Use Map to add contributions from each beta and corresponding variable
    fixed_effects <- Map(`*`, betas[names(betas) != r_int & names(betas) != r_slope], sdata[names(betas) != r_int & names(betas) != r_slope])
    linear_combination <- linear_combination + Reduce(`+`, fixed_effects)

    # Model outcome with Gaussian error term
    sdata$out <- linear_combination + stats::rnorm(n, mean = mean_r, sd = sqrt(var_r))

    #Safe model fitting function
    safe_fit_model <- function(fit_expression) {
      tryCatch(
        eval(fit_expression),
        error = function(e) {
          warning(sprintf("Model fitting failed: %s", e$message))
          return(NULL)
        }
      )
    }

    # Fit models for each method
    lmer_fit <- safe_fit_model(quote(lmerTest::lmer(stats::as.formula(mod), data = sdata)))
    ri_fit <- safe_fit_model(quote(lmerTest::lmer(stats::as.formula(ri_formula), data = sdata)))
    lm_fit <- safe_fit_model(quote(stats::glm(stats::as.formula(catsmod), data = sdata)))
    cats_fit <- safe_fit_model(quote(clusterSEs::cluster.im.glm(lm_fit, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), truncate = FALSE, drop = TRUE, report = FALSE, ci.level = 1 - alpha, return.vcv = TRUE)))
    cats_fit_trunc <- safe_fit_model(quote(clusterSEs::cluster.im.glm(lm_fit, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), truncate = TRUE, drop = TRUE, report = FALSE, ci.level = 1 - alpha, return.vcv = TRUE)))
    lmRob_fit <- safe_fit_model(quote(robust::lmRob(stats::as.formula(catsmod), data = sdata)))
    cats_fit_robust_cluster <- safe_fit_model(quote(cluster_im_lmRob(lmRob_fit, formula = catsmod, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), drop = TRUE, ci.level = 1 - alpha, return.vcv = TRUE)))
    lmrobBase_fit <- safe_fit_model(quote(robustbase::lmrob(stats::as.formula(catsmod), data = sdata)))
    cats_fit_robustbase_cluster <- safe_fit_model(quote(cluster_im_lmRob(lmrobBase_fit, formula = catsmod, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), drop = TRUE, ci.level = 1 - alpha, return.vcv = TRUE, engine = "robustbase")))

    # Safe Result Extraction Function
    safe_extract_results <- function(extract_function, ...) {
      tryCatch(
        extract_function(...),
        error = function(e) {
          warning(sprintf("Result extraction failed: %s", e$message))
          return(list(estimate = NA, significant = NA, conf_low = NA, conf_high = NA))
        }
      )
    }

    extract_lme_results <- function(fit, var_intr, alpha) {
      tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE, conf.level = 1 - alpha)
      estimate <- tidy_fit[tidy_fit$term == var_intr, "estimate"]
      p_value <- tidy_fit[tidy_fit$term == var_intr, "p.value"] < alpha
      conf_low <- tidy_fit[tidy_fit$term == var_intr, "conf.low"]
      conf_high <- tidy_fit[tidy_fit$term == var_intr, "conf.high"]
      list(estimate = estimate, significant = p_value, conf_low = conf_low, conf_high = conf_high)
    }

    extract_cats_results <- function(clust_fit, var_intr, alpha) {
      estimate <- clust_fit$beta.bar[var_intr]
      p_value <- clust_fit$p.values[var_intr, ] < alpha
      conf_low <- clust_fit$ci[var_intr,1]
      conf_high <- clust_fit$ci[var_intr,2]

      list(estimate = estimate, significant = p_value, conf_low = conf_low, conf_high = conf_high)
    }

    # Extract results using helper functions
    lme_results <- safe_extract_results(extract_lme_results, lmer_fit, var_intr, alpha)
    ri_results <- safe_extract_results(extract_lme_results, ri_fit, var_intr, alpha)
    cats_results <- safe_extract_results(extract_cats_results, cats_fit, var_intr, alpha)
    cats_trunc_results <- safe_extract_results(extract_cats_results, cats_fit_trunc, var_intr, alpha)
    cats_rob_results <- safe_extract_results(extract_cats_results, cats_fit_robust_cluster, var_intr, alpha)
    cats_robBase_results <- safe_extract_results(extract_cats_results, cats_fit_robustbase_cluster, var_intr, alpha)

    # Combine results
    combined_results <- list(lme = lme_results,
                             ri = ri_results,
                             cats = cats_results,
                             cats_trunc = cats_trunc_results,
                             cats_robust = cats_rob_results,
                             cats_robustbase = cats_robBase_results)
    return(combined_results)
  }

  all_results <- replicate(reps, simulate(), simplify = FALSE)

  # compute_method_results <- function(results, method, true_coefficient) {
  #   method_results <- lapply(results, function(sim_result) sim_result[[method]])
  #   estimates <- sapply(method_results, function(x) unlist(x$estimate))
  #   significant <- sapply(method_results, function(x) unlist(x$significant))
  #   conf_low <- sapply(method_results, function(x) unlist(x$conf_low))
  #   conf_high <- sapply(method_results, function(x) unlist(x$conf_high))
  #
  #   mean_coef <- mean(estimates, na.rm = TRUE)
  #   rejection_rate <- mean(significant, na.rm = TRUE) * 100
  #   rejection_rate_se <- stats::sd(significant, na.rm = TRUE) / sqrt(length(significant))
  #
  #   rmse <- sqrt(mean((estimates - true_coefficient)^2, na.rm = TRUE))
  #   rrmse <- rmse / abs(true_coefficient)
  #
  #   coverage <- mean((conf_low <= true_coefficient) & (conf_high >= true_coefficient), na.rm = TRUE) * 100
  #
  #   avg_ci_width <- mean(conf_high - conf_low, na.rm = TRUE)
  #
  #
  #   return(list(mean_coef = mean_coef, rejection_rate = rejection_rate,
  #               rejection_rate_se = rejection_rate_se, rmse = rmse, rrmse = rrmse,
  #               coverage = coverage, avg_ci_width = avg_ci_width))
  # }
  #
  # sim_results <- lapply(c("lme", "ri", "cats", "cats_trunc", "cats_robust", "cats_robustbase"), function(method) compute_method_results(all_results, method, true_coefficient = betas[[var_intr]]))
  #
  # names(sim_results) <- c("lme", "ri", "cats", "cats_trunc", "cats_robust", "cats_robustbase")
  #
  # return(sim_results)

  compute_method_results <- function(results, method, true_coefficient) {
    method_results <- lapply(results, function(sim_result) sim_result[[method]])
    estimates <- sapply(method_results, function(x) unlist(x$estimate))
    significant <- sapply(method_results, function(x) unlist(x$significant))
    conf_low <- sapply(method_results, function(x) unlist(x$conf_low))
    conf_high <- sapply(method_results, function(x) unlist(x$conf_high))

    mean_coef <- mean(estimates, na.rm = TRUE)
    rejection_rate <- mean(significant, na.rm = TRUE) * 100
    rejection_rate_se <- stats::sd(significant, na.rm = TRUE) / sqrt(length(significant))
    rmse <- sqrt(mean((estimates - true_coefficient)^2, na.rm = TRUE))
    rrmse <- rmse / abs(true_coefficient)
    coverage <- mean((conf_low <= true_coefficient) & (conf_high >= true_coefficient), na.rm = TRUE) * 100
    avg_ci_width <- mean(conf_high - conf_low, na.rm = TRUE)

    data.frame(model = method, mean_coef = mean_coef, rejection_rate = rejection_rate,
               rejection_rate_se = rejection_rate_se, rmse = rmse, rrmse = rrmse,
               coverage = coverage, avg_ci_width = avg_ci_width)
  }

  sim_results <- lapply(c("lme", "ri", "cats", "cats_trunc", "cats_robust", "cats_robustbase"),
                        function(method) compute_method_results(all_results, method, true_coefficient = betas[[var_intr]]))

  # Combine results into a single dataframe
  final_results <- do.call(rbind, sim_results)

  return(final_results)
}


