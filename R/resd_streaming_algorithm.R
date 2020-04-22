#' R-ESD streaming algorithm
#'
#' @param xdata Raw_data[1:1055,2]
#' @param w Integer sliding window size  e.g.w=50.
#' @param wprime Initialisation window size, wprime>w e.g. wprime=100.
#' @param k maximum number of anomalies in a given window, e.g.  k=10 by definition of anomaly k<=0.5(length(xdata)).
#' @param alpha Significance level in t distribution critical value.
#' @param iters numeric integer number of its.
#'
#' @return anomalies list of the following.
#' @return anomaly_results$x_A the ith row of this matrix is the anomalies found using the Sequential Grubbs algorithm in the ith window
#' @return anomaly_results$t_A  the time stamp of the detected anomaly LATER
#' @return anomaly_results$t_window the time stamp of the last observation in the relevant window
#' @return iters total number of windows for streaming. 
#'
#' @return stlmodel Model stl.

#' @importFrom forecast stlm
#' @importFrom TSA periodogram
#' @importFrom forecast msts
#' @importFrom stats predict
#' @importFrom stats na.omit
#'
#'
resd_streaming_algorithm <- function(xdata,wprime,w,k,alpha,iters){

  # Set up memory storage  
  anomaly_results=list(x_A=matrix(numeric(),iters,k),t_window=matrix(numeric(),iters),t_A=matrix(numeric(),iters,k))

  ####################################################################################
  ####################### INITIALISATION PHASE #######################################
  ####################################################################################

  initialised=resd_initialisation(xdata,wprime,w)
  epsilon=initialised$epsilon
  ss=initialised$ss
  e0bar=initialised$e0bar
  x_forecast=initialised$x_forecast
  
  if(sum(is.na(x_forecast)>0)){
    print("NAs in the stlmodel forecast, model can't forecast that far so need to reduce iters to:")
    iters=iters-sum(is.na(x_forecast))-1
    print(iters)
  }
  ####################################################################################
  ####################### STREAMING PHASE ############################################
  ####################################################################################
  tt=w # start streaming from w but in a genuine streaming situation tt could be a different starting point
  ###################### Algorithm 1 Step 9  ######################
  # This loop slides the window by one observation for each i increment
  # for(i in 1:(first_window_with_xmin-1)){
  for(i in 1:iters){
    # we could refit the model here or every nth time etc OR every time the epsilons exhibit trend. Here we are predicting from the original window model onto the new window = old window +i.

    ###################### Algorithm 1 Step 10 ######################
    # Pull out the first term in the window that is about to be deleted as the window rolls forward.
    e0=epsilon[1]
    # to test, print(c(epsilon[1],xdata[tt-w+i]-predict(streg,data.frame(time=tt-w+i))[[1]] ))
    ###################### Algorithm 1 Step 12 ######################
    # Predict the linear and seasonal trend from the fitted model in the streaming window
    # Calculate $\epsilon_w = x_s - x^f_s$ using the forecasts found in line 2;
    ew=xdata[i+tt]-x_forecast[i]

    ###################### Algorithm 1 Step 11 ######################
    # Drop the first data point in epsilon and add the new_term to form the new streaming window
    epsilon=c(epsilon[-1],ew)

    ###################### Algorithm 1 Step 10 ######################
    # Perform a recursive update of the sum of squares, replacing the first observation from the previous window,
    ss=ss+(ew-e0)*((ew+e0-(2*e0bar)-(ew-e0)/w))
    # test
    if(abs((w-1)*var(epsilon)-ss)>0.000000001){
     print("problem with recursive update of ss in algorithm 1 step 10")
     print(abs((w-1)*var(epsilon)-ss))
    }
    # Update ebar using a recursive formula rather than the full mean calculation
    e0bar=e0bar+(ew-e0)/w
    # test
    if(abs(e0bar-mean(epsilon))>0.00000001){
      print("problem with algorithm 1 step 10 recursive e0bar update")
    }
  
    ###################### Algorithm 1 Step 11  ######################
    # Perform the ESD test for up to k anomalies in the data window
    esd_grubbs_results=esd_grubbs_test(e0bar,epsilon,k,alpha=alpha,ss,w)
    # Store the results
    anomaly_results$t_window[i]=tt+i # This is an integer index but could report timepoint either.
    anomaly_results$t_A[i,]=esd_grubbs_results$t_A+wprime-w+i 
  }
   return(anomaly_results)
}



