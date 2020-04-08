
<!-- README.md is generated from README.Rmd. Please edit that file -->
anomalystreamr
==============

<!-- badges: start -->
<!-- badges: end -->
The goal of anomalystreamr is to provide a novel algorithm to detect anomalies in streaming time series data via statistical learning. Using time series decomposition, a sliding window approach and recursive updates of the test statistics, the generalised extreme studentised deviate (ESD) test is made computationally feasible for streaming time series data.
The method is statistically principled and it outperforms the software package, recently released by Twitter Inc. (Twitter) and used by multiple teams at Twitter as their state of the art on a daily basis. The methodology is demonstrated via the following example using unlabelled data from the Twitter GitHub repository and a manufacturing example with labelled anomalies.

Installation
------------

<!-- You can install the released version of anomalystreamr from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("anomalystreamr") -->
<!-- ``` -->
You can install the development version of anomalystreamr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("trionaryan/anomalystreamr")
```

Example
-------

This is an example

``` r
library(anomalystreamr)
## basic example code
```

Can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

Embed plots like so:

<img src="man/figures/README-pressure-1.png" width="100%" />
