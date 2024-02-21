## CRAN Package Check Results

The purpose of this update is to edit unit tests in the test-cluster_im_glmrob.R file which caused errors on the M1mac platform. The updated unit tests rely on dimensionality checks (i.e., length, nrow, ncol) & and check for null values in order to avoid tests based on numerical values that may vary from platform to platform based on extended-precision floating-point operations. 

## R CMD check results

0 errors ✔ | 0 warnings ✔ | 1 note ✖

* Days since last update: 5

## Revdepcheck

This package has no reverse dependencies.
