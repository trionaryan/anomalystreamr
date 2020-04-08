#' Methods / functions used in anomalystreamr()
#'
#' @param xdata Raw_data[1:1055,2]
#' @param w Integer sliding window size  e.g.w=50.
#' @param wprime Initialisation window size, wprime>w e.g. wprime=100.
#' @param k maximum number of anomalies in a given window, e.g.  k=10 by definition of anomaly k<=0.5(length(xdata)).
#' @param buffer buffer for mvecdeque vector.
#' @param alpha Significance level in t distribution critical value.
#' @param tt time to start streaming tt=wprime here or w but could be any time of interest.
#' @param deque boolean implement deque or not.
#' @param iters numeric integer number of its.
#'
#' @return anomalies list of the following.
#' @return anomaly_results$x_A the ith row of this matrix is the anomalies found using the Sequential Grubbs algorithm in the ith window
#' @return anomaly_results$t_A  the time stamp of the detected anomaly LATER
#' @return anomaly_results$t_window the time stamp of the last observation in the relevant window
#' @return iters total number of windows for streaming. How do we do this on the fly but efficiently?!?!
#'
#' @return stlmodel Model stl.

#' @importFrom forecast stlm
#' @importFrom TSA periodogram
#' @importFrom forecast msts
#' @importFrom stats predict
#' @importFrom stats na.omit
#'
#'
resd_streaming_algorithm <- function(xdata,wprime,w,tt,k,buffer,alpha,iters,deque){

  # FUNCTION INPUTS
  # data: dataset as a vector OR xdata[1,]=(x,t)? e.g.
  # xdata=raw_data[1:1055,2]
  # w: sliding window size  e.g.
  # w=50
  # wprime: Initialisation window size, wprime>w e.g.
  # wprime=100
  # k: maximum number of anomalies in a given window, e.g.  k=10 by definition of anomaly k<=0.5(length(xdata))
  # k=10
  # buffer: buffer for mvecdeque vector
  # alpha: Significance level in t distribution critical value
  # alpha =.05
  # tt: time to start streaming tt=wprime here or w but could be any time of interest

  # FUNCTION OUTPUTS
  # anomalies: list of the following
  # anomaly_results$x_A: the ith row of this matrix is the anomalies found using the Sequential Grubbs algorithm in the ith window
  # anomaly_results$t_A:  the time stamp of the detected anomaly LATER
  # anomaly_results$t_window: the time stamp of the last observation in the relevant window
  # iters: total number of windows for streaming. How do we do this on the fly but efficiently?!?!

  # Set up memory storage  CHANGE TO A QUEUE LATER FOR STREAMING??
  anomaly_results=list(x_A=matrix(numeric(),iters,k),t_window=matrix(numeric(),iters),t_A=matrix(numeric(),iters,k))

  ####################################################################################
  ####################### INITIALISATION PHASE #######################################
  ####################################################################################

  initialised=resd_initialisation(xdata,wprime,w,tt)
  epsilon=initialised$epsilon
  ss=initialised$ss
  e0bar=initialised$e0bar
  x_forecast=initialised$x_forecast
  #trying just the mean x_forecast=rep(mean(xdata[1:wprime]),length(initialised$x_forecast))
  #par(mfrow=c(2,1))
  #plot(xdata,type="l")
  #plot(c(rep(mean(epsilon),wprime-w),epsilon,(xdata[(wprime+1):length(xdata)]-x_forecast)),type="l",ylab="epsilon")

  # testing
  if(sum(is.na(x_forecast)>0)){
    print("NAs in the stlmodel forecast, reset iters to:")
    iters=iters-sum(is.na(x_forecast))-1
    print(iters)
  }
  # if(iters>length(x_forecast)){
  #   print("WARNING: too many iterations requested.")
  # }
  # if(abs(length(epsilon)-w)>0.000001){
  #   print("problem with epsilon; length of epsilon does not equal w")
  # }
  # print("is this the problem?")
  # print(mean(epsilon))
  # print(e0bar)
  # if(abs(mean(epsilon)-e0bar)>0.000001){
  #   print("problem with e0bar initialisation")
  # }
  # if(abs(var(epsilon)*(w-1)-ss)>0.000001){
  #   print("problem with ss initialisation")
  # }

  # Initialise DEQUE by only carrying around the k largest and smallest values of epsilon
  if(deque==TRUE){
# IMPLEMENT FUNCTION?!    initialise_deque(k,buffer,epsilon)
    k_buffer=k+buffer
    epsilon_upper_k_buffer=sort(epsilon,decreasing=T)[1:k_buffer]
    epsilon_upper_k_buffer_index=rep(numeric(),k_buffer)
    for(i in 1:k_buffer){
      epsilon_upper_k_buffer_index[i] <- which(epsilon==epsilon_upper_k_buffer[i])
    }
    epsilon_lower_k_buffer=sort(epsilon,decreasing=F)[1:k_buffer]
    epsilon_lower_k_buffer_index=rep(numeric(),k_buffer)
    for(i in 1:k_buffer){
      epsilon_lower_k_buffer_index[i] <- which(epsilon==epsilon_lower_k_buffer[i])
    }
  }
  ####################################################################################
  ####################### STREAMING PHASE ############################################
  ####################################################################################

  ###################### Algorithm 1 Step 9  ######################
  # This loop slides the window by one observation for each i increment
  # zoomin=FALSE
  # if(zoomin==TRUE){
  #   tempiters=(which.min(xdata)-tt+w+1)-w
  #   if(tempiters<0){
  #     print(paste("tempiters is less than 0:",tempiters))
  #   }
  #   first_window_with_xmin=which.min(xdata)-tt-1
  #   last_window_with_xmin=which.min(xdata)-tt-1+w-1
  # }
  # for(i in first_window_with_xmin:last_window_with_xmin){

  # for(i in 1:(first_window_with_xmin-1)){
  for(i in 1:iters){
    # print(paste("iteration number", i))
    # in an ideal world with lots of cpu time and small datasets we should refit the model here or every nth time etc? OR every time the epsilons exhibit trend. Here we are predicting from the original window model onto the new window = old window +i.

    ###################### Algorithm 1 Step 10 ######################
    # Pull out the first term in the window that is about to be deleted.
    if (deque==FALSE){
      e0=epsilon[1]
    }else { ### To drop epsilon fully, we need to recalculate epsilon_0 each time instead.
      # e0=xdata[tt-w+i]-predict(streg,data.frame(time=tt-w+i))[[1]]
    }
    # to test, print(c(epsilon[1],xdata[tt-w+i]-predict(streg,data.frame(time=tt-w+i))[[1]] ))
    ###################### Algorithm 1 Step 12 ######################
    # Predict the linear and seasonal trend from the fitted model in the streaming window
    # OLD WAY seas_trend_update=predict(streg,data.frame(time=tt+i))[[1]]
    # OLD WAYDecompose the data to obtain the stationary residuals epsilon=epsilon
    # OLD WAYew=xdata[i+tt]-seas_trend_update
    # Calculate $\epsilon_w = x_s - x^f_s$ using the forecasts found in line 2;
    ew=xdata[i+tt]-x_forecast[i]

    ###################### Algorithm 1 Step 11 ######################
    # drop the first data point in epsilon and add the new_term to form the new streaming window
    if(deque==FALSE){
      epsilon=c(epsilon[-1],ew)
    }else{
      # keep for now for testing purposes
      epsilon=c(epsilon[-1],ew)
      # if e0 is in the mvecdeque, bump it out.
      if(sum(e0==epsilon_upper_k_buffer)>0){
        print("e0 is in the upper mvecdeque and is")
        print(e0)
        print("epsilon_upper_k before")
        print(epsilon_upper_k_buffer)
        # replace with a numeric() and resort
        epsilon_upper_k_buffer = epsilon_upper_k_buffer[epsilon_upper_k_buffer!= e0]
        #INDEXING?!?!?
        print("epsilon_upper_k after")
        print(epsilon_upper_k_buffer)
      }
      if(sum(e0==epsilon_lower_k_buffer)>0){
        print("e0 is in the lower mvecdeque and is")
        print(e0)
        print("epsilon_lower_k before")
        print(epsilon_lower_k_buffer)
        # remove e0
        epsilon_lower_k_buffer = epsilon_lower_k_buffer[epsilon_lower_k_buffer!= e0]
        print("epsilon_lower_k after")
        print(epsilon_lower_k_buffer)
      }
      # check if buffer isn't empty
      max_upper = length(epsilon_upper_k_buffer)
      max_lower = length(epsilon_lower_k_buffer)
      if(max_upper<=k|max_lower<=k){
        print("need to recalculate mvecdeque")
       #? SOMETHING LIKE THIS HERE epsilon=initial_decomposition(???x,???time_stream,???streg)

      }
      # if ew deserves a place in the mvecdeque, slot it in
      if (ew>=epsilon_upper_k_buffer[max_upper]){
        print("epsilon_upper_k_buffer before")
        print(epsilon_upper_k_buffer)
        print("ew for ew>epsilon_upper_k[max_upper]")
        print(ew)
        epsilon_upper_k_buffer[max_upper ]=ew
        epsilon_upper_k_buffer_index[max_upper ]=i+tt
        epsilon_upper_k_buffer = sort(epsilon_upper_k_buffer,decreasing=TRUE)[1:max_upper ]
        print("epsilon_upper_k_buffer after")
        print(epsilon_upper_k_buffer)
      }
      if(ew<=epsilon_lower_k_buffer[max_lower]){
        # slot it in t
        print("epsilon_lower_k_buffer before")
        print(epsilon_lower_k_buffer)
        print("ew for ew>epsilon_lower_k[max_lower]")
        print(ew)
        epsilon_lower_k_buffer[max_lower]=ew
        epsilon_lower_k_buffer_index[max_lower]=i+tt
        epsilon_lower_k_buffer=sort(epsilon_lower_k_buffer,decreasing=FALSE)
        print("epsilon_lower_k_buffer after")
        print(epsilon_lower_k_buffer)
      }
    }
    ###################### Algorithm 1 Step 10 ######################
    # Perform a recursive update of the sum of squares, replacing the first observation from the previous window,
    ss=ss+(ew-e0)*((ew+e0-(2*e0bar)-(ew-e0)/w))
    # Maybe need to do on log scale?
    # test
    if(abs((w-1)*var(epsilon)-ss)>0.000000001){
     # print("problem with recursive update of ss in algorithm 1 step 10")
     # print(abs((w-1)*var(epsilon)-ss))
    }
    # Update ebar using a recursive formula rather than the full mean calculation
    e0bar=e0bar+(ew-e0)/w
    # test
    if(deque==FALSE){
      if(abs(e0bar-mean(epsilon))>0.00000001){
        print("problem with algorithm 1 step 10 recursive e0bar update")
      }
    }
    ###################### Algorithm 1 Step 11  ######################
    # Perform the ESD test for up to k anomalies in the data window
    if(deque==FALSE){
#       if(i<(first_window_with_xmin-1)){ # use this to watch the first windows in the zone of interest
      #  if(i<(last_window_with_xmin-10)){# use this to watch the last number of windows in the zone of interest

        esd_grubbs_results=esd_grubbs_test(e0bar,epsilon,k,alpha=alpha,ss,w)
 #     }else{
  #      cat("i is",i)
    #    esd_grubbs_results=esd_grubbs_test_with_plots(e0bar,epsilon,k,alpha,ss,w)
   #   }
      #print("esd_grubbs_results$x_A,t_A")
      #print(esd_grubbs_results$x_A)
      #print(esd_grubbs_results$t_A)
    }
    if(deque==TRUE){
      epsilon_deque=c(epsilon_upper_k_buffer,epsilon_lower_k_buffer)
      epsilon_deque_index=c(epsilon_upper_k_buffer_index,epsilon_lower_k_buffer_index)

      # implement ESD test on epsilon_deque
      esd_grubbs_results=esd_grubbs_test(e0bar,epsilon_deque,k,alpha,ss,w)
      # print("esd_grubbs_results_deque$x_A,t_A")
      # print(esd_grubbs_results_deque$x_A)
      # print(esd_grubbs_results_deque$t_A)

      for(j in 1:length(esd_grubbs_results$x_A)){
        if(is.na(esd_grubbs_results$x_A[j])==FALSE){
          esd_grubbs_results$t_A[j]=epsilon_deque_index[(which(epsilon_deque==esd_grubbs_results$x_A[j]))]
        }
      }
    }
    # Store the results
    # anomaly_results$x_A[i,]=esd_grubbs_results$x_A # THIS IS ACTUALLY EPSILON NOT X FOR NOW!!!
    anomaly_results$t_window[i]=tt+i # This is an integer for now. Later...timepoint
    anomaly_results$t_A[i,]=esd_grubbs_results$t_A+wprime-w+i #INDEX CHECK HERE
    #print(anomaly_results$t_A[i,])
  }
   return(anomaly_results)
}




# # plot_results=function(xdata,test){
# #   unique_anomalies=unique(as.vector(test$t_A))
# #   unique_anomalies
# #   plot(xdata,pch=4,type="l")
# #   points(y=xdata[unique_anomalies],x=unique_anomalies,col="green",pch=4)
# # }
# # library(compiler)
# # # initial_decomposition<-cmpfun(initial_decomposition)
# # recursive_esd<-cmpfun(recursive_esd)
# # esd_grubbs_test<-cmpfun(esd_grubbs_test)
# # model_fitting<-cmpfun(model_fitting)
# # plot_results<-cmpfun(plot_results)
#

# function to automatically choose window sizes
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
  # print(paste("wprime and w are:",wprime,w))
  tt=wprime
  deque=FALSE
  test=resd_streaming_algorithm(xdata=xdata,wprime=wprime,w=w,tt=tt,k=k,alpha=alpha,iters=iters,deque=FALSE)
  #pack plot_results(xdata,test)
  #pack abline(v = wprime,col="red")
  #pack abline(v = wprime-w,col="red")
  #pack points(y=xdata[anomalies_known],x=anomalies_known,col="red",pch=)
  print(paste("precision,recall,f1score"))
  print(precision_recall(test,anomalies_known))
  #return(test)
  return(precision_recall(test,anomalies_known))
}

precision_recall<-function(test,anomalies_known){
  # precision: the proportion of true positive anomalies of all detected anomalies
  # recall: the ratio of true positive anomalies to the sum of true positive anomalies and false negative anomalies
  detected_anomalies=as.vector(na.omit(unique(as.vector(test$t_A))))
  num_true_positive_anomalies=sum(detected_anomalies%in%anomalies_known)
  precision=num_true_positive_anomalies/length(detected_anomalies)

  false_negative_anomalies = length(anomalies_known)-num_true_positive_anomalies#You do have an anomaly and dont detect it
  recall = num_true_positive_anomalies/sum(num_true_positive_anomalies,false_negative_anomalies)
  f1score = 2*precision*recall / (precision + recall)
  prec_rec_f1 <-c(precision,recall,f1score)
  return(prec_rec_f1)
}

# precision_recall_twitter<-function(twitter_anoms,anomalies_known){
#   # precision: the proportion of true positive anomalies of all detected anomalies
#   # recall: the ratio of true positive anomalies to the sum of true positive anomalies and false negative anomalies
#   detected_anomalies=twitter_anoms$anoms[,1]
#   num_true_positive_anomalies=sum(detected_anomalies%in%anomalies_known)
#   precision=num_true_positive_anomalies/length(detected_anomalies)
#
#   false_negative_anomalies=length(anomalies_known)-num_true_positive_anomalies#You do have an anomaly and dont detect it
#   recall=num_true_positive_anomalies/sum(num_true_positive_anomalies,false_negative_anomalies)
#   return(c(precision,recall))
# }
#
# compare_twitter_us=function(xdata,r_esd_anoms,twitter_anoms,known=FALSE,ylabex,anomalies_known){
#   unique_anomalies=unique(as.vector(r_esd_anoms$t_A))
#   # par(mfrow=c(1,1))
#   # plot(xdata,pch=4,type="l",xlab="timepoint",ylab=ylabex)
#   # points(y=xdata[twitter_anoms$anoms[,1]],x=twitter_anoms$anoms[,1],col="blue")
#   # points(y=xdata[unique_anomalies],x=unique_anomalies,col="green",pch=4)
#   # if(known==TRUE){points(y=xdata[anomalies_known],x=anomalies_known,col="red",pch=0)
#   # legend("bottomleft", legend=c( "Known","Twitter","R-ESD"),
#   #        col=c( "red","blue","green"), pch=c(0,1,4))}
#   # else{
#   #   legend("bottomleft", legend=c("Twitter","R-ESD"),
#   #          col=c("blue","green"), pch=c(1,4))
#   # }
#   our_detected_anomalies=as.vector(na.omit(unique(as.vector(r_esd_anoms$t_A))))
#   length(our_detected_anomalies)
#   length(twitter_anoms$anoms[,1])
#   print(paste("count of Twitter anomalies:",length(twitter_anoms$anoms[,1])))
#   print(paste("count of R_ESD anomalies:",length(our_detected_anomalies)))
#   print(paste("count of agreeing anomalies:",sum(twitter_anoms$anoms[,1] %in% our_detected_anomalies)))
#   print(paste("R-ESD precision and recall are",precision_recall(r_esd_anoms,anomalies_known)[1],"and",precision_recall(r_esd_anoms,anomalies_known)[2]))
#   print(paste("Twitter's precision and recall are",precision_recall_twitter(twitter_anoms,anomalies_known)[1],"and",precision_recall_twitter(twitter_anoms,anomalies_known)[2]))
#   # Build a Time series data set
#   tsdata=xdata
#   tsindex=c(1:length(xdata))
#   #tsindextime=raw_data[,1] #for twitter only
#   tsindextime=tsindex
#   # tsindextime=manuf[,1] #for manuf only
#   res_dframe=data.frame(tsindex,tsdata,tsindextime)
#
#   #plot(res_lsd[,2],type="l")
#   ggplot(data = res_dframe, aes(x = tsindextime, y =tsdata))+
#     geom_line()  +
#     geom_point(data=res_dframe[twitter_anoms$anoms[,1], ], aes(x=tsindextime, y=tsdata), colour="blue", size=3,shape=1)+
#     geom_point(data=res_dframe[our_detected_anomalies, ], aes(x=tsindextime, y=tsdata), colour="green", size=3,shape=4)+
#    geom_point(data=res_dframe[anomalies_known, ], aes(x=tsindextime, y=tsdata), colour="red", size=3,shape=5)+
#   xlab('Timestamp') +
#     #ylab('Count')# +
#   ylab('Temperature')# +
#
# }
#
