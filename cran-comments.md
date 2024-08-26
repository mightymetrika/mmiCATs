## R CMD check results

0 errors | 0 warnings | 0 notes

## Summary of updates

This update includes the following major changes:

* Added the Kenward-Roger approximation for small sample inference to the
pwr_func_lmer() simulation function. This enhancement allows users to compare
cluster-adjusted t-statistics to mixed-effects models with Kenward-Roger degrees
of freedom.

*Introduced the KenRCATs() function, which launches a Shiny web application.
This web-based interface for pwr_func_lmer() provides a user-friendly way to
conduct power analyses and simulations.

*Updated documentation and examples to reflect these new features.

*Minor bug fixes and performance improvements.

## Revdepcheck

This package has no reverse dependencies.
