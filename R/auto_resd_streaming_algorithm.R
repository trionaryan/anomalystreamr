#' Function to automatically tune window sizes if there's an anomaly in the learning window
#'
#' @param xdata Raw_data[1:1055,2]
#' @param w Integer sliding window size  e.g.w=50.
#' @param wprime Initialisation window size, wprime>w e.g. wprime=100.
#' @param k maximum number of anomalies in a given window, e.g.  k=10 by definition of anomaly k<=0.5(length(xdata)).
#' @param alpha Significance level in t distribution critical value.
#' @param iters numeric integer number of iterations.
#' @param anomalies_known known anomaly labels
#'
#' @return anomaly_results list of the following.
#' @return anomaly_results$x_A the ith row of this matrix is the anomalies found using the Sequential Grubbs algorithm in the ith window
#' @return anomaly_results$t_A  the time stamp of the detected anomaly LATER
#' @return anomaly_results$t_window the time stamp of the last observation in the relevant window
#'
#' @importFrom forecast stlm
#' @importFrom TSA periodogram
#' @importFrom forecast msts
#' @importFrom stats predict
#' @importFrom stats na.omit
#'
auto_resd_streaming_algorithm<-function(xdata,anomalies_known,alpha,wprime,w,iters,k){
  if(wprime>min(anomalies_known)){
    print("Warning! There is an anomaly in the learning window!")
    print(min(anomalies_known))
    print("Setting wprime to be 50 less than the first anomaly and w to be half of this")
    # wprime=min(anomalies_known)-100
    w=round(wprime/2)
    print("wprime and w are")
    print(c(wprime,w))
    print("k is")
    print(k)
  }
  anomaly_results=resd_streaming_algorithm(xdata=xdata,wprime=wprime,w=w,k=k,alpha=alpha,iters=iters)
  return(anomaly_results)

}
