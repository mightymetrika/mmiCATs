
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmiCATs

<!-- badges: start -->
<!-- badges: end -->

The goal of the Mighty Metrika Interface to Cluster Adjusted
t-Statistics (‘mmiCATs’) R package is to provide ‘shiny’ web
applications for CATs and to provide research tools for investigating
when CATs models should be preferred over other statistical models used
for cluster adjustment.

The implementations of CATs in ‘mmiCATs’ is based on the
cluster.im.glm() function from the R package ‘clusterSEs’. For more
information on CATs see [Esarey and Menger
(2018)](https://doi.org/10.1017/psrm.2017.42).

## Installation

You can install the development version of ‘mmiCATs’ like so:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/mmiCATs")
```

## Shiny Application

Load the ‘mmiCATs’ package and call the mmiCATs() function to launch a
‘shiny’ web application which allows you to run the
clusterSEs::cluster.im.glm() function on a csv dataset:

``` r
library(mmiCATs)
```

``` r
mmiCATs()
```

## CATs with Robust Models (for simulation research purposes only)

R packages such as ‘robust’ and ‘robustbase’ have functions which allow
users to run robust alternatives to stats::lm() and stats::glm(). The
‘mmiCATs’ package has the functions cluster_im_lmRob() and
cluster_im_glmRob() which take the basic algorithm used in
clusterSEs::cluster.im.glm() but swaps out stats::glm() for a robust
alternative. In the case of cluster_im_lmRob() either robust::lmRob() or
robustbase::lmrob() get swapped in while in the case of
cluster_im_glmRob() either robust::glmRob() or robustbase::glmrob() get
swapped in. The example below shows some simple results comparing the
methods for linear models.

``` r
# Get common parameters
.form <- Sepal.Length ~ Petal.Length + Petal.Width
.clust <- ~ Species

# clusterSEs::cluster.im.glm()
glmout <- stats::glm(.form,
                     family = "gaussian",
                     data = iris)

clusterSEs::cluster.im.glm(glmout, dat = iris, cluster = .clust,
                           return.vcv = TRUE)
#> 
#>  Cluster-Adjusted p-values:  
#>  
#>            variable name   cluster-adjusted p-value
#>              (Intercept)                       0.11
#>             Petal.Length                      0.055
#>              Petal.Width                      0.705
#> 
#>  Confidence Intervals (centered on cluster-averaged results): 
#>  
#>       variable name              CI lower             CI higher
#>         (Intercept)     -1.42830072928431      6.54809830391216
#>        Petal.Length   -0.0384794641752815      1.59034415889221
#>         Petal.Width     -1.17723433639784      1.44337893950581

# robust::lmRob()
robustout <- robust::lmRob(.form, data = iris)

cluster_im_lmRob(robustout, .form, dat = iris, cluster = .clust,
                 engine = "robust", return.vcv = TRUE)
#> 00:00:00 left
#> 00:00:00 left
#> 00:00:00 left
#> $p.values
#>                    [,1]
#> (Intercept)  0.10328518
#> Petal.Length 0.02382271
#> Petal.Width  0.71362962
#> 
#> $ci
#>                CI lower CI higher
#> (Intercept)  -1.2299791  6.133096
#> Petal.Length  0.2716887  1.406647
#> Petal.Width  -1.1638474  1.417432
#> 
#> $vcv.hat
#>              (Intercept) Petal.Length Petal.Width
#> (Intercept)    2.1963785  -0.32092673   0.5416086
#> Petal.Length  -0.3209267   0.05218536  -0.1060045
#> Petal.Width    0.5416086  -0.10600446   0.2699346
#> 
#> $beta.bar
#>  (Intercept) Petal.Length  Petal.Width 
#>    2.4515587    0.8391680    0.1267921

# robustbase::lmrob()
robustbaseout <- robustbase::lmrob(.form, data = iris)

cluster_im_lmRob(robustbaseout, .form, dat = iris, cluster = .clust,
                 engine = "robustbase", return.vcv = TRUE)
#> $p.values
#>                    [,1]
#> (Intercept)  0.10305707
#> Petal.Length 0.03924708
#> Petal.Width  0.72343658
#> 
#> $ci
#>                 CI lower CI higher
#> (Intercept)  -1.26209472  6.312871
#> Petal.Length  0.09756128  1.507929
#> Petal.Width  -1.10924300  1.341017
#> 
#> $vcv.hat
#>              (Intercept) Petal.Length Petal.Width
#> (Intercept)    2.3246098  -0.41242678   0.5770706
#> Petal.Length  -0.4124268   0.08058483  -0.1296058
#> Petal.Width    0.5770706  -0.12960576   0.2432276
#> 
#> $beta.bar
#>  (Intercept) Petal.Length  Petal.Width 
#>    2.5253884    0.8027451    0.1158868
```

The simulation study in [Esarey and Menger
(2018)](https://doi.org/10.1017/psrm.2017.42) tested a few different
methods for handling clustering. They found that a correctly specified
mixed effects model tends to perform most efficiently; however, they
found that CATs can outperform a mispecified mixed effects model. The
pwr_func_lmer() function can be used to run a simulation where data is
generated from a mixed effect model and results are compared between:

- The data generating mixed effects model
- A random intercept model
- A clusterSEs::cluster.im.glm(drop = TRUE, truncate = FALSE) model
- A clusterSEs::cluster.im.glm(drop = TRUE, truncate = TRUE) model
- A cluster_im_lmRob(drop = TRUE, engine = “robust”) model
- A cluster_im_lmRob(drop = TRUE, engine = “robustbase”) model

The models are compared on:

- Mean coefficient
- Rejection rate
- Rejection rate standard error
- Root mean square error
- Relative root mean square error
- Confidence interval coverage
- Average confidence interval width

The following example shows a simulation where both a random intercept
and random slope are specified and where two of the variables (x1 and
x3) are correlated. The simulation is limited to 5 reps to minimize
computation time. The main variable of interest is variable x1; as such,
the comparison metrics will be with respect to x1.

``` r
pwr_func_lmer(betas = list("int" = 0, "x1" = -5, "x2" = 2, "x3" = 10),
              dists = list("x1" = stats::rnorm,
                           "x2" = stats::rbinom,
                           "x3" = stats::rnorm),
              distpar = list("x1" = list(mean = 0, sd = 1),
                             "x2" = list(size = 1, prob = 0.4),
                             "x3" = list(mean = 1, sd = 2)),
              N = 50,
              reps = 5,
              alpha = 0.05,
              var_intr = "x1",
              grp = "ID",
              mod = "out ~ x1 + x2 + x3 + (x3|ID)",
              catsmod = "out ~ x1 + x2 + x3",
              r_slope = "x3",
              r_int = "int",
              n_time = 100,
              mean_i = 0,
              var_i = 1,
              mean_s = 0,
              var_s = 1,
              cov_is = 0,
              mean_r = 0,
              var_r = 1,
              cor_mat = diag(2),
              corvars = list(c("x1", "x3")))
#>             model mean_coef rejection_rate rejection_rate_se        rmse
#> 1             lme -4.999843            100                 0 0.007395759
#> 2              ri -4.998563            100                 0 0.006039764
#> 3            cats -5.000869            100                 0 0.006993846
#> 4      cats_trunc -5.000869            100                 0 0.006993846
#> 5     cats_robust -5.003207            100                 0 0.005779193
#> 6 cats_robustbase -5.003256            100                 0 0.005506254
#>         rrmse coverage avg_ci_width
#> 1 0.001479152      100   0.05643267
#> 2 0.001207953      100   0.07709939
#> 3 0.001398769      100   0.06896953
#> 4 0.001398769      100   0.06896953
#> 5 0.001155839      100   0.07201932
#> 6 0.001101251      100   0.07200100
```
