
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmiCATs

<!-- badges: start -->
<!-- badges: end -->

The goal of the Mighty Metrika Interface to Cluster Adjusted
t-Statistics (‘mmiCATs’) R package is to provide ‘shiny’ web
applications for CATs and to provide research tools which can be used to
further understand when CATs models might be preferred over other
statistical models used for cluster adjustment.

The implementations of CATs in mmiCATs is based on the cluster.im.glm()
function from the R package ‘clusterSEs’. For more information on CATs
see [Esarey and Menger (2018)](https://doi.org/10.1017/psrm.2017.42).

## Installation

You can install the development version of mmiCATs like so:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/mmiCATs")
```

## Shiny Application

Load the ‘mmiCATs’ package and call the mmiCATs() function to launch a
‘shiny’ web application which allows you to run the
clusterSEs::cluster.im.glm() function on your csv data:

``` r
library(mmiCATs)
```

``` r
mmiCATs()
```

## CATs with Robust Models (for research purposes only)

R packages such as ‘robust’ and ‘robustbase’ have functions which allow
users to run robust alternatives to stats::lm() and stats::glm(). The
‘mmiCATs’ has the functions cluster_im_lmRob() and cluster_im_glmRob()
which take the basic algorithm used in clusterSEs::cluster.im.glm() but
swap out stats::glm() for robust::lmRob() or robustbase::lmrob(), in the
case of cluster_im_lmRob(), or for robust::glmRob() or
robustbase::glmrob(), in the case of cluster_im_glmRob(). The example
below shows some simple results comparing the methods for linear models.

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
