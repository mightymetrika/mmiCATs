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
                          cor_mat = diag(2),
                          corvars = list(c("x1", "x3"))) {

  #Create ri formula
  ri_formula <- paste0(catsmod, "+ (1|", grp, ")")

  simulate <- function() {
    n <- N * n_time
    sdata <- data.frame(grp = rep(1:N, each = n_time))
    names(sdata) <- grp

    # Generate data for correlated variables
    for (cor_group in corvars) {
      if (all(sapply(cor_group, function(x) x %in% names(dists) && identical(dists[[x]], stats::rnorm)))) {
        # Generate correlated normal data for this group
        group_means <- sapply(cor_group, function(var) distpar[[var]]$mean)
        mv_data <- MASS::mvrnorm(N*n_time, mu = group_means, Sigma = cor_mat)
        colnames(mv_data) <- cor_group
        sdata <- cbind(sdata, mv_data)
      }
    }

    # Generate data for non-correlated variables
    non_correlated_vars <- setdiff(names(dists), unlist(corvars))
    for (var in non_correlated_vars) {
      params <- distpar[[var]]
      sdata[[var]] <- do.call(dists[[var]], c(list(N*n_time), params))
    }

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

    # Safe Model Fitting Function
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
    cats_fit <- safe_fit_model(quote(clusterSEs::cluster.im.glm(lm_fit, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), truncate = FALSE, drop = TRUE, report = FALSE)))
    cats_fit_trunc <- safe_fit_model(quote(clusterSEs::cluster.im.glm(lm_fit, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), truncate = TRUE, drop = TRUE, report = FALSE)))
    lmRob_fit <- safe_fit_model(quote(robust::lmRob(stats::as.formula(catsmod), data = sdata)))
    cats_fit_robust_cluster <- safe_fit_model(quote(cluster_im_lmRob(lmRob_fit, formula = catsmod, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), drop = TRUE, report = FALSE)))
    lmrobBase_fit <- safe_fit_model(quote(robustbase::lmrob(stats::as.formula(catsmod), data = sdata)))
    cats_fit_robustbase_cluster <- safe_fit_model(quote(cluster_im_lmRob(lmrobBase_fit, formula = catsmod, dat = sdata, cluster = stats::as.formula(paste0("~ ",grp)), drop = TRUE, report = FALSE, engine = "robustbase")))

    # Safe Result Extraction Function
    safe_extract_results <- function(extract_function, ...) {
      tryCatch(
        extract_function(...),
        error = function(e) {
          warning(sprintf("Result extraction failed: %s", e$message))
          return(list(estimate = NA, significant = NA))
        }
      )
    }

    extract_lme_results <- function(fit, var_intr, alpha) {
      tidy_fit <- broom.mixed::tidy(fit)
      estimate <- tidy_fit[tidy_fit$term == var_intr, "estimate"]
      p_value <- tidy_fit[tidy_fit$term == var_intr, "p.value"] < alpha
      list(estimate = estimate, significant = p_value)
    }

    extract_cats_results <- function(fit, clust_fit, var_intr, alpha) {
      tidy_fit <- broom::tidy(fit)
      estimate <- tidy_fit[tidy_fit$term == var_intr, "estimate"]
      p_value <- clust_fit$p.values[var_intr, ] < alpha
      if(is.null(p_value)){
        p_value <- NA
      }
      list(estimate = estimate, significant = p_value)
    }

    # Extract results using helper functions
    lme_results <- safe_extract_results(extract_lme_results, lmer_fit, var_intr, alpha)
    ri_results <- safe_extract_results(extract_lme_results, ri_fit, var_intr, alpha)
    cats_results <- safe_extract_results(extract_cats_results, lm_fit, cats_fit, var_intr, alpha)
    cats_trunc_results <- safe_extract_results(extract_cats_results,lm_fit, cats_fit_trunc, var_intr, alpha)
    cats_rob_results <- safe_extract_results(extract_cats_results,lmRob_fit, cats_fit_robust_cluster, var_intr, alpha)
    cats_robBase_results <- safe_extract_results(extract_cats_results,lmrobBase_fit, cats_fit_robustbase_cluster, var_intr, alpha)

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


  compute_method_results <- function(results, method) {
    method_results <- lapply(results, function(sim_result) sim_result[[method]])
    mean_coef <- mean(sapply(method_results, function(x) unlist(x$estimate)), na.rm = TRUE)
    power <- mean(sapply(method_results, function(x) unlist(x$significant)), na.rm = TRUE) * 100
    return(list(mean_coef = mean_coef, power = power))
  }

  sim_results <- lapply(c("lme", "ri", "cats", "cats_trunc", "cats_robust", "cats_robustbase"), function(method) compute_method_results(all_results, method))

  names(sim_results) <- c("lme", "ri", "cats", "cats_trunc", "cats_robust", "cats_robustbase")

  return(sim_results)
}


