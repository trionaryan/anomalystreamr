#' Model fitting functions used in to find an model based on the initialisation phase
#'
#' @param xdata Raw_data[1:1055,2]
#' @param wprime Initialisation window size, wprime>w e.g. wprime=100.
#' @param w Integer sliding window size  e.g.w=50.
#' @param tt time to start streaming tt=wprime here or w but could be any time of interest.

#' @return  initial fitted model a list of the following
# initial$epsilon: stationary residuals
# initial$ss: initial sums of squares
# initial$e0bar: mean of the vector of time series residuals
#'
#' @importFrom forecast forecast
#' @importFrom stats var


resd_initialisation<-function(xdata,wprime,w,tt){
  # Sequential Grubbs test for up to k anomalies in a vector of residuals

  # FUNCTION INPUTSdevtools::use_readme_rmd()
  # xdata:
  # wprime: Initialisation window size, wprime>w
  # w: sliding window size
  # tt: time to start streaming tt=wprime here but could be different later

  # FUNCTION OUTPUTS
  # initial: a list of the following
  # initial$epsilon: stationary residuals
  # initial$ss: initial sums of squares
  # initial$e0bar: mean of the vector of time series residuals


  #### Algorithm 1. Step 1: ####
  # Define the intial window of data X_t-w+1:t in this case t=w
  xprime = xdata[1:wprime]
  timeprime=1:wprime
  ###################### Algorithm 1 Steps 2 and 3: ######################
  #streg=model_fitting(xprime=xprime,timeprime=1:wprime)
  # Create forecasts for example using the using the forecast function as far as is practicable
  # Forecast into the future
  stlmodel=model_fitting(xprime,timeprime)
  horizon=length(xdata)-wprime
  forecast_answer=forecast(stlmodel,h=horizon,find.frequency=FALSE)
  x_forecast=forecast_answer$mean
  # summary(x_forecast)

  #x_forecast=stlf(ts(xprime,frequency=per), method='naive',h=horizon)$mean

  # plot(xdata[1:(wprime + horizon)], type = 'l')
  # lines(1:wprime, fitted.values(stlmodel), col = 'red')
  # lines((wprime+1):(wprime + horizon), x_forecast, col = 'blue')
  # summary(x_forecast)
  ###################### Algorithm 1 Step 4  ######################
  # Define the current window of data to search for anomalies
  x=xdata[(tt-w+1):tt]
  # Test #
  # if(length(x)!=w) {
  #   print("There is a problem with size of data window")
  # }
  # Define regular time points in the streaming window (for now)
  time_stream=(tt-w+1):tt

  ###################### Algorithm 1 Step 5  ######################
  # Using the forecasts from line 2, calculate
  # the forecasted stationary residuals $\bfepsilon = \bfx - \bfx^f = (\epsilon_{1},\epsilon_{2}\dots,\epsilon_{w})$;
  # old way for streg model epsilon=initial_decomposition(x,time_stream,streg)
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


