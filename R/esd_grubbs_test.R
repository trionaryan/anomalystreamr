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
  epsilon=as.vector(epsilon) 
  epsilon_orig=epsilon
  e0bar_orig=e0bar

  if(abs(mean(epsilon)-e0bar)>0.000001){
    print("problem with e0bar in grubbs function inputs")
  }
  if(abs(var(epsilon)*(w-1)-ss)>0.000001){
    print("problem with ss initialisation in grubbs function inputs")
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
    # Testing wtempo=length(epsilon)
    wtempo=w-j+1
    ss_reduced=ss-(wtempo/(wtempo-1))*((epsilon[index_max]-ebar)^2)
    # Testing this calculation from the vanilla calculation of SS
    #test_ss_reduced=(w-1)*var(epsilon_reduced)
    if(abs((wtempo-2)*var(epsilon_reduced)-ss_reduced)>0.0001){
      print("problem with algorithm 2 step 3 ss_reduced recursive calculation")
      print(abs((wtempo-2)*var(epsilon_reduced)-ss_reduced))
    }

    ###################### Algorithm 2 Step 4  ######################
    # Calculate a critical value γ 
    # rosners l
    l=j-1 # order statistic
    p<-1-alpha/(2*(w-l))
    t<-qt(p,(w-l-2))

    gamma=(w-l-1)*t/sqrt((w-l-2+t**2)*(w-l)) # ROSNER EQN 2.5
    # Calculate t test statistic from Grubbs test statistic, the ratio S_n^2/S^2
    grubbs_ratio=ss_reduced/ss
    t_n=sqrt((w-1)*(1-grubbs_ratio)) # Convert from Grubbs ratio to Rosners R
    Rosner_test_statistic=((w-1)*t_n)/w
    Rosner_test_statistic=t_n

    if(Rosner_test_statistic>gamma){
      # Store the results
      x_A[j]=epsilon[index_max] 
      t_A[j]=which(epsilon_orig==epsilon[index_max])[sum(epsilon_orig==epsilon[index_max])] 
      
    }
    ###################### Algorithm 2 Step 6  ######################
    # Find the mean of this reduced dataset x_n_bar
    # Note this is three times faster than finding the mean again directly using mean(epsilon
    
    wtemp=w-j+1
    if(wtemp!=length(epsilon)){
        print("problem with wtemp, alg2 step 6")
      }

    ebar=(ebar*wtemp-epsilon[index_max])/(wtemp-1)
 
    if(abs(mean(epsilon_reduced)-ebar)>0.0000001){
      print("problem with recursive ebar update in algorithm 2 step 6")
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

