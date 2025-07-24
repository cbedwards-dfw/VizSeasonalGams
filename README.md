
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VizSeasonalGams

<!-- badges: start -->
<!-- badges: end -->

The goal of VizSeasonalGams is to streamline plotting fitted gams that
incorporate a seasonal component (e.g., smooths across day of year).
While the tools here can work for models that include a smooth of
interest across any continuous variable, this documentation will default
to assuming the data and model include `"doy"` which contains day of
year. You can provide an alternate variable name with the optional
`plot_by` variable.

## Installation

You can install the development version of VizSeasonalGams from
[GitHub](https://github.com/) with:

``` r
pak::pak("cbedwards-dfw/VizSeasonalGams")
```
