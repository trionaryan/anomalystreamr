#' Extreme Studentised Deviate test for up to k anomalies in a vector of residuals
#' Recursive updates are enabled by expressing the ESD test statistic as function of the Grubbs ratio (Grubbs 1950)
#'
#' @param e0bar Mean of the vector of time series residuals.
#' @param epsilon Time series residuals.
#' @param k Number of outliers to test.
#' @param alpha Significance level.
#' @param ss Initial sums of squares.
#' @param w Size of window (This is denoted n in the Rosner paper)
#'
#' @return x_A Anomaly vector.
#' @return grubbs_results List
#' @return grubbs_results$x_A The anomalies found using the Sequential Grubbs test
#' @return grubbs_results$t_A  the time stamp or index of the detected anomaly LATER
#'
#' @importFrom stats qt
#'
esd_grubbs_test<-function(e0bar,epsilon,k,alpha,ss,w){
  # Grubbs test for up to k anomalies in a vector of residuals using recursive updates to speed up computation
  deque=FALSE
  # FUNCTION INPUTS
  # e0bar: mean of the vector of time series residuals
  # epsilon: time series residuals
  # k: number of outliers to test
  # alpha: significance level
  # ss: initial sums of squares
  # w: size of window (n in rosner paper)
  # FUNCTION OUTPUTS
  # x_A: Anomaly vector
  # grubbs_results=list(x_A=matrix(numeric(),iters,k), t_A=matrix(numeric(),k),t_window=numeric())
  # ..$x_A: the anomalies found using the Sequential Grubbs test
  # ..$t_A:  the time stamp or index of the detected anomaly LATER

  epsilon=as.vector(epsilon) # This may not be needed TEST
  epsilon_orig=epsilon
  e0bar_orig=e0bar

  # testing full ss calculation vs function input
  # ss_i = (w-1)*var(epsilon)
  # ss_i-ss<0.000001
  # TEST remove for full runs
  # testing
  # print("is this the problem?134")
  # print(mean(epsilon))
  # print(e0bar)
  if(deque==FALSE){
  if(abs(mean(epsilon)-e0bar)>0.000001){
    print("problem with e0bar in grubbs function inputs")
  }
  if(abs(var(epsilon)*(w-1)-ss)>0.000001){
    print("problem with ss initialisation in grubbs function inputs")
  }
  }
  ebar=e0bar
  # Set up memory storage
  x_A = rep(NA,k)
  t_A = rep(NA,k)

  ###################### Algorithm 2 Step 1  ######################
  # loop over k, the number of possible outliers to test
  for (j in 1:k){
    ###################### Algorithm 2 Step 2  ######################
    # Find e_n, the datapoint which is furthest away from the mean
    index_max=which.max(abs(epsilon-ebar))

    ###################### Algorithm 2 Step 3  ######################
    # Remove e_|w| and create a reduced dataset
    epsilon_reduced=epsilon[-index_max]
    # Calculate the recursive update of S

    #ss_reduced=ss-(w/(w-1))*((epsilon[index_max]-ebar)^2)
    #ss_reduced=(w-2)*var(epsilon_reduced)

    #wtempo=length(epsilon) if deque is false
    wtempo=w-j+1
    if(deque==FALSE){
      if(wtempo!=length(epsilon)){
        print("problem with wtemp0, alg2 step 3")
      }
    }
    ss_reduced=ss-(wtempo/(wtempo-1))*((epsilon[index_max]-ebar)^2)
    # this should be ss_reduced (length(epsilon_reduced)-1)*var(epsilon_reduced)
    # this should be ss (length(epsilon)-1)*var(epsilon)
    # Testing this calculation from the vanilla calculation of SS
    #test_ss_reduced=(w-1)*var(epsilon_reduced)
    if(deque==FALSE){
      if(abs((wtempo-2)*var(epsilon_reduced)-ss_reduced)>0.0001){
        print("problem with algorithm 2 step 3 ss_reduced recursive calculation")
        print(abs((wtempo-2)*var(epsilon_reduced)-ss_reduced))
      }
    }
    ####??? microbenchmark((w-1)*var(epsilon_reduced))
    #### ???microbenchmark(ss-(w/(w-1))*((200-ebar)^2) )
    # system.time (replicate(1000,(w-2)*var(epsilon_reduced)))
    # system.time(replicate(1000,ss-(w/(w-1))*((200-ebar)^2)))

    ###################### Algorithm 2 Step 4  ######################
    # Calculate a critical value γ for the Grubbs test  WIP
    # # TWITTER VERSION using Rosner
    # if(one_tail){
    #   p <- 1 - alpha/(n-i+1)
    # } else {
    #   p <- 1 - alpha/(2*(n-i+1))
    # }
    #
    # t <- qt(p,(n-i-1L)) # 1L is just an integer 1
    #
    # gamma=t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1)) # EQN 6 OF THEIR PAPER

    # MY VERISON
    # rosners l
    l=j-1 # order statistic
    p<-1-alpha/(2*(w-l))
    t<-qt(p,(w-l-2))

    gamma=(w-l-1)*t/sqrt((w-l-2+t**2)*(w-l)) # ROSNER EQN 2.5
    # Calculate T test statistic from Grubbs test statistic, the ratio S_n^2/S^2
    grubbs_ratio=ss_reduced/ss
    t_n=sqrt((w-1)*(1-grubbs_ratio)) # Convert from Grubbs ratio to Rosners R
    Rosner_test_statistic=((w-1)*t_n)/w
    Rosner_test_statistic=t_n

    #print(paste("rosner, s are", c(Rosner_test_statistic,sqrt(ss))))
    #print(paste("?should be?", Rosner_test_statistic*sqrt(ss)))
    # print(c(((w-1)*t_n)/w , t_n))
    # To make sure our test statistics are the same (ish)
    # Twitter_Rosner_incorrect_test_statistic_median=max(epsilon_orig-median(epsilon_orig))/sqrt(var(epsilon_orig))
    # or -min(epsilon_orig-median(epsilon_orig))/sqrt(var(epsilon_orig))
    #
    # Twitter_Rosner_test_statistic_mean=max(epsilon_orig-mean(epsilon_orig))/sqrt(var(epsilon_orig))
    # or -min(epsilon_orig-mean(epsilon_orig))/sqrt(var(epsilon_orig))
    # # par(ask=TRUE)
    # plot(epsilon,type="l")
    # points(y=epsilon[index_max],x=index_max,col="blue",pch=4)

    # print("Rosner_test_statistic, gamma (anomaly if TS>gamma)")
    # print(c(Rosner_test_statistic,gamma))
    if(Rosner_test_statistic>gamma){
      # print("anomaly detected ")
      # print(c(epsilon[index_max]))
      # print("Rosner_test_statistic, gamma (anomaly if TS>gamma)")
      # print(c(Rosner_test_statistic,gamma))

      # Store the results
      x_A[j]=epsilon[index_max] #Probably better to use an index but its a bit tricky? do later if needed
      if (sum(epsilon_orig==epsilon[index_max])>1){
        # print("repeated values in epsilon. ok to take the last one i think")
      }
      t_A[j]=which(epsilon_orig==epsilon[index_max])[sum(epsilon_orig==epsilon[index_max])] # For Deque, this indexes the concatenated epsilon_upper, epsilon_lower vector.
      # This is a bit tricky. For now store the actual value not the index. t_A[j]=which(epsilon==epsilon[index_max]) # Store the outlier index - faster way to do this?sometimes thye are the smallest and sometimes the biggest epsilon.
      # test didnt work if((index_max+j-1-which(epsilon==epsilon[index_max]))>0.00001){print("problem with indices")}

      ###################### Algorithm 2 Step 6  ######################
      # Find the mean of this reduced dataset x_n_bar
      # Note this is three times faster than finding the mean again directly using mean(epsilon
    }
      wtemp=w-j+1
      if(deque==FALSE){
        if(wtemp!=length(epsilon)){
          print("problem with wtemp, alg2 step 6")
        }
      }
      ebar=(ebar*wtemp-epsilon[index_max])/(wtemp-1)

      # ebar=mean(epsilon_reduced)
      # microbench indicates that this is slower but its not see system.time below
      # microbenchmark(mean(epsilon_reduced))
      # microbenchmark((ebar*wtemp-2)/(wtemp-1))
      # tester=runif(1000)
      # microbenchmark(mean(tester))
      # testbar=mean(tester)
      # microbenchmark((testbar*1000-2)/(900-1))
      # system.time(replicate(1000, mean(tester)))
      # system.time(replicate(1000, (testbar*1000-2)/(900-1)))

      # test
      if(deque==FALSE){
        if(abs(mean(epsilon_reduced)-ebar)>0.0000001){
          print("problem with recursive ebar update in algorithm 2 step 6")
        }
      }
    ###################### Algorithm 2 Step 7  ######################
    # Delete the outlier and reduce the dataset accordingly
    epsilon=epsilon_reduced
    # print("ss and ss reduced")
    # print(c(ss,ss_reduced))
    ss=ss_reduced
  }
  grubbs_results = list(x_A=x_A,t_A=t_A)
  return(grubbs_results)
}

# # grubbs_test_with_plots<-function(e0bar,epsilon,k,alpha,ss,w){
# #   # Sequential Grubbs test for up to k anomalies in a vector of residuals
# #
# #   # FUNCTION INPUTS
# #   # ebar: mean of the vector of time series residuals
# #   # epsilon: time series residuals
# #   # k: number of outliers to test
# #   # alpha: significance level
# #   # ss: initial sums of squares
# #   # w: size of window (n in rosner paper)
# #   # FUNCTION OUTPUTS
# #   # x_A: Anomaly vector
# #   # grubbs_results=list(x_A=matrix(numeric(),iters,k), t_A=matrix(numeric(),k),t_window=numeric())
# #   # ..$x_A: the anomalies found using the Sequential Grubbs test
# #   # ..$t_A:  the time stamp or index of the detected anomaly LATER
# #
# #   epsilon=as.vector(epsilon) # This may not be needed TEST
# #   epsilon_orig=epsilon
# #   # testing full ss calculation vs function input
# #   # ss_i = (w-1)*var(epsilon)
# #   # ss_i-ss<0.000001
# #   # TEST remove for full runs
# #   # testing
# #   # print("is this the problem?134")
# #   # print(mean(epsilon))
# #   # print(e0bar)
# #   if(deque==FALSE){
# #     if(abs(mean(epsilon)-e0bar)>0.000001){
# #       print("problem with e0bar in grubbs function inputs")
# #     }
# #     if(abs(var(epsilon)*(w-1)-ss)>0.000001){
# #       print("problem with ss initialisation in grubbs function inputs")
# #       break
# #     }
# #   }
# #   ebar=e0bar
# #   # Set up memory storage
# #   x_A = rep(NA,k)
# #   t_A = rep(NA,k)
# #
# #   ###################### Algorithm 2 Step 1  ######################
# #   # loop over k, the number of possible outliers to test
# #   for (j in 1:k){
# #     ###################### Algorithm 2 Step 2  ######################
# #     # Find e_n, the datapoint which is furthest away from the mean
# #     index_max = which.max(abs(epsilon-ebar))
# #
# #     ###################### Algorithm 2 Step 3  ######################
# #     # Remove e_|w| and create a reduced dataset
# #     epsilon_reduced=epsilon[-index_max]
# #     # Calculate the recursive update of S
# #
# #     #ss_reduced=ss-(w/(w-1))*((epsilon[index_max]-ebar)^2)
# #     #ss_reduced=(w-2)*var(epsilon_reduced)
# #
# #     #wtempo=length(epsilon) if deque is false
# #     wtempo=w-j+1
# #     if(deque==FALSE){
# #       if(wtempo!=length(epsilon)){
# #         print("problem with wtemp0, alg2 step 3")
# #       }
# #     }
# #     ss_reduced=ss-(wtempo/(wtempo-1))*((epsilon[index_max]-ebar)^2)
# #     # this should be ss_reduced (length(epsilon_reduced)-1)*var(epsilon_reduced)
# #     # this should be ss (length(epsilon)-1)*var(epsilon)
# #     # Testing this calculation from the vanilla calculation of SS
# #     #test_ss_reduced=(w-1)*var(epsilon_reduced)
# #     if(deque==FALSE){
# #       if(abs((wtempo-2)*var(epsilon_reduced)-ss_reduced)>0.0000001){
# #         print("problem with algorithm 2 step 3 ss_reduced recursive calculation")
# #       }
# #     }
# #     ####??? microbenchmark((w-1)*var(epsilon_reduced))
# #     #### ???microbenchmark(ss-(w/(w-1))*((200-ebar)^2) )
# #     # system.time (replicate(1000,(w-2)*var(epsilon_reduced)))
# #     # system.time(replicate(1000,ss-(w/(w-1))*((200-ebar)^2)))
# #
# #     ###################### Algorithm 2 Step 4  ######################
# #     # Calculate a critical value γ for the Grubbs test  WIP
# #     # # TWITTER VERSION using Rosner
# #     # if(one_tail){
# #     #   p <- 1 - alpha/(n-i+1)
# #     # } else {
# #     #   p <- 1 - alpha/(2*(n-i+1))
# #     # }
# #     #
# #     # t <- qt(p,(n-i-1L)) # 1L is just an integer 1
# #     #
# #     # gamma=t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1)) # EQN 6 OF THEIR PAPER
# #
# #     # MY VERISON
# #     # rosners l
# #     l=j-1 # order statistic
# #     p<-1-alpha/(2*(w-l))
# #     t<-qt(p,(w-l-2))
# #
# #     gamma=(w-l-1)*t/sqrt((w-l-2+t**2)*(w-l)) # ROSNER EQN 2.5
# #     # Calculate T test statistic from Grubbs test statistic, the ratio S_n^2/S^2
# #     grubbs_ratio=ss_reduced/ss
# #     t_n=sqrt((w-1)*(1-grubbs_ratio)) # Convert from Grubbs ratio to Rosners R
# #     Rosner_test_statistic=((w-1)*t_n)/w
# #
# #     # par(ask=TRUE)
# #     # plot(epsilon,type="l", ylim=c(-62,25))
# #     # points(y=epsilon[index_max],x=index_max,col="blue",pch=4)
# #     #
# #     # print("Rosner_test_statistic, gamma (anomaly if TS>gamma)")
# #     # print(c(Rosner_test_statistic,gamma))
# #     if (Rosner_test_statistic>gamma){
# #       # print("anomaly detected ")
# #       # print(c(epsilon[index_max]))
# #
# #       # Store the results
# #       x_A[j]=epsilon[index_max] #Probably better to use an index but its a bit tricky? do later if needed
# #       t_A[j]=which(epsilon_orig==epsilon[index_max]) # For Deque, this indexes the concatenated epsilon_upper, epsilon_lower vector.
# #       # This is a bit tricky. For now store the actual value not the index. t_A[j]=which(epsilon==epsilon[index_max]) # Store the outlier index - faster way to do this?sometimes thye are the smallest and sometimes the biggest epsilon.
# #       # test didnt work if((index_max+j-1-which(epsilon==epsilon[index_max]))>0.00001){print("problem with indices")}
# #
# #       ###################### Algorithm 2 Step 6  ######################
# #       # Find the mean of this reduced dataset x_n_bar
# #       # Note this is three times faster than finding the mean again directly using mean(epsilon
# #     }
# #     wtemp=w-j+1
# #     # if(deque==FALSE){
# #     #   if(wtemp!=length(epsilon)){
# #     #     print("problem with wtemp, alg2 step 6")
# #     #   }
# #     # }
# #     ebar=(ebar*wtemp-epsilon[index_max])/(wtemp-1)
# #
# #     # ebar=mean(epsilon_reduced)
# #     # microbench indicates that this is slower but its not see system.time below
# #     # microbenchmark(mean(epsilon_reduced))
# #     # microbenchmark((ebar*wtemp-2)/(wtemp-1))
# #     # tester=runif(1000)
# #     # microbenchmark(mean(tester))
# #     # testbar=mean(tester)
# #     # microbenchmark((testbar*1000-2)/(900-1))
# #     # system.time(replicate(1000, mean(tester)))
# #     # system.time(replicate(1000, (testbar*1000-2)/(900-1)))
# #
# #     # test
# #     if(deque==FALSE){
# #       if(abs(mean(epsilon_reduced)-ebar)>0.0000001){
# #         print("problem with recursive ebar update in algorithm 2 step 6")
# #       }
# #     }
# #     ###################### Algorithm 2 Step 7  ######################
# #     # Delete the outlier and reduce the dataset accordingly
# #     epsilon=epsilon_reduced
# #     print("ss and ss reduced")
# #     print(c(ss,ss_reduced))
# #     ss=ss_reduced
# #   }
# #   grubbs_results = list(x_A=x_A,t_A=t_A)
# #   return(grubbs_results)
# # }
# #
