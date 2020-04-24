#' Model fitting functions used in to find a useful model based on the initialisation phase
#'
#' @param xprime time series initialisation dataset as a vector.
#' @param timeprime time vector (for now must be regular)
#'
#' @return streg fitted model
#'
#' @importFrom forecast stlm
#' @importFrom TSA periodogram
#' @importFrom forecast msts

#
model_fitting<-function(xprime,timeprime){
  stlmodel=TRUE
  ###################### Algorithm 1 Step 2: ######################
  # This function finds a useful period and fits an stl model to the initial wprime window of time series data
  # Compute the periodogram for the initial window i.e. Find the best period to fit the seasonal trend
  # The resulting 'per' will be used to fit the seasonal component of the time series.
  p=periodogram(xprime,plot=F)
  fs=data.frame(freq=p$freq,spec=p$spec)
  ord=fs[order(-fs$spec),]
  per=1/ord[1:3,1]
  per=per[per<length(xprime)/3] # to ensure at least 3 seasons worth in the learning window
  # print(paste("period is ",per))
  ts_data = msts(xprime,seasonal.periods = per)
  # AAN means additive errors, additive trend, no seasonality (because this is computed in previous step)
  stlmodel= stlm(ts_data,s.window="periodic",robust = TRUE,etsmodel="AAN")
  return(stlmodel)
}
