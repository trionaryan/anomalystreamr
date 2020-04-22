#' Functions used to find a useful model forecast based on the initialisation phase / wprime window
#'
#' @param xdata Univariate time series dataset
#' @param wprime Initialisation window size
#' @param w Integer sliding window size, w<wprime

#' @return  initial fitted model a list of the following
# initial$epsilon: stationary residuals
# initial$ss: initial sums of squares
# initial$e0bar: mean of the vector of time series residuals
#'
#' @importFrom forecast forecast
#' @importFrom stats var


resd_initialisation<-function(xdata,wprime,w){
  ###################### Algorithm 1. Step 1: ##################################
  # Define the intial window of data X_t-w+1:t in this case t=w
  xprime = xdata[1:wprime]
  timeprime=1:wprime
  ###################### Algorithm 1 Steps 2 and 3: ############################
  #streg=model_fitting(xprime=xprime,timeprime=1:wprime)
  # Create forecasts for example using the using the forecast function as far as is practicable
  # Forecast into the future
  stlmodel=model_fitting(xprime,timeprime)
  horizon=length(xdata)-wprime
  forecast_answer=forecast(stlmodel,h=horizon,find.frequency=FALSE)
  x_forecast=forecast_answer$mean

  ###################### Algorithm 1 Step 4  ######################
  tt=w # in a genuine streaming situation tt does not have to be equal to w! 
  # Define the current window of data to search for anomalies
  x=xdata[(tt-w+1):tt]
  
  # Define regular time points in the streaming window (for now)
  time_stream=(tt-w+1):tt

  ###################### Algorithm 1 Step 5  ######################
  # Using the forecasts from line 2, calculate
  # the forecasted stationary residuals $\bfepsilon = \bfx - \bfx^f = (\epsilon_{1},\epsilon_{2}\dots,\epsilon_{w})$;
  epsilon=x-x_forecast[1:w]
  
  ###################### Algorithm 1 Step 6  ######################
  # Compute the initial sum of squares of the residuals
  ss=(w-1)*var(epsilon)
  
  ###################### Algorithm 1 Step 7  ######################
  # Compute the initial mean of the residuals
  e0bar=mean(epsilon)
  initial=list(epsilon=epsilon,ss=ss,e0bar=e0bar,x_forecast=x_forecast)
  return(initial)
}


