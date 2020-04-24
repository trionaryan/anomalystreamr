
<!-- README.md is generated from README.Rmd. Please edit that file -->
<img src="man/figures/hexanomalystreamerblack.png" align="right" height="150" />
================================================================================

anomalystreamr
==============

<p>
 
</p>
Real-time statistically principled anomaly detection in time series data implemented in R.

The R-ESD (Recursive Extreme Studentised Deviate) algorithm is an improved streaming-compatible version of a widely used approach; the SH-ESD (Seasonal Hybrid Extreme Studentised Deviate) algorithm from the AnomalyDetection software package, used by multiple teams at Twitter Inc. as their state of the art on a daily basis.

Installation
------------

To install the development version anomalytreamr 0.1.0 and also AnomalyDetection (for comparison) from github use:

``` r
# If required install devtools:
# install.packages("devtools")
devtools::install_github("trionaryan/anomalystreamr")
library(anomalystreamr)
devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
```

### Example

``` r
# Using unlabelled data from the Twitter AnomalyDetection GitHub repository.
data(raw_data) # Load Twitter data
xdata <- raw_data[ ,2]
wprime <- (1440*6) # Initial learning window size

# Find anomalies using R-ESD streaming algorithm
resd_anomalies <- resd_streaming_algorithm(xdata, wprime, w = 1440, k = 288, alpha = 0.05, iters = (length(xdata) - wprime - 10))

# Find anomalies using SH-ESD non-overlapping windows (not streaming)
twitter_anomalies <- AnomalyDetectionVec(xdata, max_anoms = 0.02, period = 720, longterm_period = 1441, direction = 'both', only_last = FALSE, plot = FALSE)
```

<img src="man/figures/README-example-1.png" width="100%" /> <img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" style="display: block; margin: auto;" />

### Companion article

For further details and examples see [here](https://arxiv.org/abs/1911.05376). Beyond SH-ESD, using the EGADS benchmark A3 dataset, we demonstrate the R-ESD algorithm outperforms the EGADS and DeepAdVote algorithms both in accuracy and computational time.
