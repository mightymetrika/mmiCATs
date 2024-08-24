#' Power Analysis for Clustered Data
#'
#' Conducts a power analysis for clustered data using simulation. This function
#' allows for comparing the performance of different estimation methods in terms
#' of power, rejection rate, root mean square error (RMSE), relative RMSE, coverage
#' probability, and average confidence interval width.
#'
#' @param betas Named list of true coefficient values for the fixed effects.
#' @param dists Named list of functions to generate random distributions for each
#'              predictor.
#' @param distpar Named list of parameter lists for each distribution function in
#'                `dists`.
#' @param N Integer specifying the number of groups.
#' @param reps Integer specifying the number of replications for the simulation.
#' @param alpha Numeric value specifying the significance level for hypothesis
#'              testing.
#' @param var_intr Name of the variable of interest (for power calculations) as
#'                 a string.
#' @param grp Name of the grouping variable as a string.
#' @param mod Formula for the mixed-effects model.
#' @param catsmod Formula for the CATs model.
#' @param r_slope Name of the random slope variable as a string.
#' @param r_int Name of the random intercept as a string.
#' @param n_time Integer specifying the number of time points per group.
#' @param mean_i Mean for the random intercept.
#' @param var_i Variance for the random intercept.
#' @param mean_s Mean for the random slope.
#' @param var_s Variance for the random slope.
#' @param cov_is Covariance between the random intercept and slope.
#' @param mean_r Mean for the residual error.
#' @param var_r Variance for the residual error.
#' @param cor_mat Correlation matrix for correlated predictors, if any.
#' @param corvars List of vectors, each vector containing names of correlated
#'                variables.
#'
#' @return A dataframe summarizing the results of the power analysis, including
#'         average coefficient estimate, rejection rate, root mean square error,
#'         relative root mean square error, coverage probability, and average
#'         confidence interval width for each method.
#'
#' @examples
#' pwr_func_lmer(reps = 2)
#'
#' @export
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
    # Handle n_time as either an integer or vector
    if (length(n_time) == 1) {
      times <- rep(n_time, N)
    } else if (length(n_time) == N) {
      times <- n_time
    } else {
      stop("n_time must be either a single integer or a vector of length N")
    }

    # n <- N * n_time # Total number of observations
    n <- sum(times) # Total number of observations
    # sdata <- data.frame(grp = rep(1:N, each = n_time))
    sdata <- data.frame(grp = rep(1:N, times = times))
    names(sdata) <- grp

    # Check if cor_mat and corvars are not NULL
    if (!is.null(cor_mat) && !is.null(corvars)) {
      # Process the correlated variables
      correlated_data <- lapply(corvars, function(cor_group) {
        if (all(sapply(cor_group, function(x) x %in% names(dists) && identical(dists[[x]], stats::rnorm)))) {
          group_means <- sapply(cor_group, function(var) distpar[[var]]$mean)
          # mv_data <- MASS::mvrnorm(N*n_time, mu = group_means, Sigma = cor_mat)
          mv_data <- MASS::mvrnorm(n, mu = group_means, Sigma = cor_mat)
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
    if (!identical(non_correlated_vars, character(0))){
      non_correlated_data <- Map(function(var, params) {
        # do.call(dists[[var]], c(list(N*n_time), params))
        do.call(dists[[var]], c(list(n), params))
      }, var = non_correlated_vars, params = distpar[non_correlated_vars])
      sdata <- cbind(sdata, non_correlated_data)
    }



    # Random effects
    REff <- MASS::mvrnorm(N, mu = c(mean_i, mean_s), Sigma = rbind(c(var_i, cov_is), c(cov_is, var_s)))  # Using N instead of n_pers
    colnames(REff) <- c(r_int, r_slope)

    # sdata[["rand_int"]] <- rep(REff[, r_int], each = n_time)
    # sdata[["rand_slope"]] <- rep(REff[, r_slope], each = n_time)
    sdata[["rand_int"]] <- rep(REff[, r_int], times = times)
    sdata[["rand_slope"]] <- rep(REff[, r_slope], times = times)

    # Model outcome with Gaussian error term
    linear_combination <- betas[[r_int]] + sdata[["rand_int"]] + (betas[[r_slope]] + sdata[["rand_slope"]]) * sdata[[r_slope]]

    # Use Map to add contributions from each beta and corresponding variable
    fixed_effects <- Map(`*`,
                         betas[names(betas) != r_int & names(betas) != r_slope],
                         sdata[names(sdata) %in% names(betas) & names(sdata) != r_int & names(sdata) != r_slope])
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
          return(list(estimate = NA, significant = NA, conf_low = NA, conf_high = NA, mf_success = NA))
        }
      )
    }

    extract_lme_results <- function(fit, var_intr, alpha, ddf.method = "Satterthwaite") {
      mf_success <- ifelse(is.null(fit), 0, 1)

      # Extract results only if fit is not NULL
      if (mf_success) {
        tidy_fit <- broom.mixed::tidy(fit, conf.int = TRUE,
                                      conf.level = 1 - alpha, ddf.method = ddf.method)
        estimate <- tidy_fit[tidy_fit$term == var_intr, "estimate"]
        p_value <- tidy_fit[tidy_fit$term == var_intr, "p.value"]
        conf_low <- tidy_fit[tidy_fit$term == var_intr, "conf.low"]
        conf_high <- tidy_fit[tidy_fit$term == var_intr, "conf.high"]
      }

      cat_res <- list(
        estimate = ifelse(mf_success, estimate, NA),
        significant = ifelse(mf_success, p_value < alpha, NA),
        conf_low = ifelse(mf_success, conf_low, NA),
        conf_high = ifelse(mf_success, conf_high, NA),
        mf_success = mf_success
      )

      return(cat_res)
    }

    extract_cats_results <- function(clust_fit, var_intr, alpha) {

      mf_success <- ifelse(is.null(clust_fit), 0, 1)

      cat_res <- list(
        estimate = ifelse(mf_success, clust_fit$beta.bar[var_intr], NA),
        significant = ifelse(mf_success, clust_fit$p.values[var_intr, ] < alpha, NA),
        conf_low = ifelse(mf_success, clust_fit$ci[var_intr, 1], NA),
        conf_high = ifelse(mf_success, clust_fit$ci[var_intr, 2], NA),
        mf_success = mf_success
      )

      return(cat_res)

    }

    # Extract results using helper functions
    lme_results <- safe_extract_results(extract_lme_results, lmer_fit, var_intr, alpha)
    ri_results <- safe_extract_results(extract_lme_results, ri_fit, var_intr, alpha)
    lme_kr_results <- safe_extract_results(extract_lme_results, lmer_fit, var_intr, alpha, ddf.method = "Kenward-Roger")
    ri__kr_results <- safe_extract_results(extract_lme_results, ri_fit, var_intr, alpha, ddf.method = "Kenward-Roger")
    cats_results <- safe_extract_results(extract_cats_results, cats_fit, var_intr, alpha)
    cats_trunc_results <- safe_extract_results(extract_cats_results, cats_fit_trunc, var_intr, alpha)
    cats_rob_results <- safe_extract_results(extract_cats_results, cats_fit_robust_cluster, var_intr, alpha)
    cats_robBase_results <- safe_extract_results(extract_cats_results, cats_fit_robustbase_cluster, var_intr, alpha)

    # Combine results
    combined_results <- list(lme = lme_results,
                             ri = ri_results,
                             lme_kr = lme_kr_results,
                             ri_kr = ri__kr_results,
                             cats = cats_results,
                             cats_trunc = cats_trunc_results,
                             cats_robust = cats_rob_results,
                             cats_robustbase = cats_robBase_results)
    return(combined_results)
  }

  all_results <- replicate(reps, simulate(), simplify = FALSE)

  compute_method_results <- function(results, method, true_coefficient) {
    method_results <- lapply(results, function(sim_result) sim_result[[method]])
    estimates <- sapply(method_results, function(x) unlist(x$estimate))
    significant <- sapply(method_results, function(x) unlist(x$significant))
    conf_low <- sapply(method_results, function(x) unlist(x$conf_low))
    conf_high <- sapply(method_results, function(x) unlist(x$conf_high))
    success_bin <- sapply(method_results, function(x) unlist(x$mf_success))

    mean_coef <- mean(estimates, na.rm = TRUE)
    rejection_rate <- mean(significant, na.rm = TRUE) * 100
    rejection_rate_se <- stats::sd(significant, na.rm = TRUE) / sqrt(sum(!is.na(significant)))
    rmse <- sqrt(mean((estimates - true_coefficient)^2, na.rm = TRUE))
    rrmse <- rmse / abs(true_coefficient)
    coverage <- mean((conf_low <= true_coefficient) & (conf_high >= true_coefficient), na.rm = TRUE) * 100
    avg_ci_width <- mean(conf_high - conf_low, na.rm = TRUE)

    if(is.list(success_bin)){
      success_bin <- unlist(success_bin)
    }
    success <- sum(success_bin, na.rm = TRUE)

    data.frame(model = method, mean_coef = mean_coef, rejection_rate = rejection_rate,
               rejection_rate_se = rejection_rate_se, rmse = rmse, rrmse = rrmse,
               coverage = coverage, avg_ci_width = avg_ci_width, success = success)
  }

  sim_results <- lapply(c("lme", "ri", "lme_kr", "ri_kr", "cats", "cats_trunc", "cats_robust", "cats_robustbase"),
                        function(method) compute_method_results(all_results, method, true_coefficient = betas[[var_intr]]))

  # Combine results into a single dataframe
  final_results <- do.call(rbind, sim_results)

  return(final_results)
}
