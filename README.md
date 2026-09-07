# landmarked

<!-- badges: start -->

<!-- badges: end -->

The goal of landmarked is to plot survival curves for time-to-event data adjusted for a specified landmark time. You can use your existing `survminer::ggsurvplot` code and specify the landmark time and label.

## Installation

You can install the landmarked from [CRAN](https://cran.r-project.org/) with:

``` r
install.packages("landmarked")
```

You can install the development version of landmarked from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("CStats/landmarked")
```

## Example

This is a basic example which shows you how to plot an adjusted landmark curve.

``` r
library(landmarked)

fit = survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)
ggsurvplotlm(fit, landmark_time = 42, landmark_label = "Landmark Time")
```
