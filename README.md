
<!-- README.md is generated from README.Rmd. Please edit that file -->
anomalystreamr <img src="man/figures/hexanomalystreamerblack.png" align="right" height="150" />
===============================================================================================

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

The anomalystreamr package provides code for the Recursive Extreme Dtudentised Deviate (R-ESD) algorithm; a real-time, low memory, statistically principled algorithm to detect anomalies in time series data. It is an improved, corrected and streaming-compatible version of a very-widely used approach; the Seasonal Hybrid Extreme Studentised Deviate (SH-ESD) algorithm from the AnomalyDetection software package, used by multiple teams at Twitter Inc. as their state of the art on a daily basis.

The methodology is demonstrated via the following example using unlabelled data from the Twitter AnomalyDetection GitHub repository and a manufacturing example with labelled anomalies.

Installation
------------

<!-- You can install the released version of anomalystreamr from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("anomalystreamr") -->
<!-- ``` -->
You can install the development version of anomalystreamr from [GitHub](https://github.com/) with:

Installation
------------

You can install anomalytreamr development version 0.1.0 from github with:

``` r
# install.packages("devtools")
devtools::install_github("trionaryan/anomalystreamr")
#> Skipping install of 'anomalystreamr' from a github remote, the SHA1 (3b9eb00a) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

Example
-------

This is an example

``` r
### Stremaing Twitter's Github Example
data(raw_data)
xdata=raw_data[,2]
anomalies_known=14390 #No known anomalies. this is just a decoy!
wprime=(1440*6)
w=1440
iters=length(xdata)-wprime-10
k=288
twitter_anoms<-AnomalyDetectionVec(xdata,max_anoms=0.02,period=720,longterm_period=1441,direction='both',only_last=FALSE, plot=TRUE)

resd_anomalies<-resd_streaming_algorithm(xdata,wprime,w,k,alpha=0.05,iters)
#> [1] "period is  720"  "period is  1440" "period is  480" 
#> [1] "NAs in the stlmodel forecast, model can't forecast that far so need to reduce iters to:"
#> [1] 5737
print(paste("precision,recall,f1score"))
#> [1] "precision,recall,f1score"
print(precision_recall(resd_anomalies,anomalies_known))
#> [1]   0   0 NaN
```

Embed plots like so:
