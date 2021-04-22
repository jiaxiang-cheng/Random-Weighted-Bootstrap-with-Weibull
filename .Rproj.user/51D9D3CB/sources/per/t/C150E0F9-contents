library(nleqslv)

# the number of transformer to predict, this can be customized
nsample <- 100

interval <- matrix(ncol = 2)

for (sample in 1:nsample) {
  # the probability generated from uniform distribution for generating Tib
  f <-
    runif(numB) # the number should be aligned with bootstrap samples
  # initialize the list for saving Tib
  Tb <- list()
  # initialize the matrix for saving Uib
  U <- matrix()
  # the current age of this sample transformer
  t_i <- time[sample]
  
  # simulate Tib from distribution the conditional probability of sample i
  for (prob in f) {
    fn <- function(tx) {
      return(prob - (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                               shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
    }
    Tb <- rbind(Tb, nleqslv(100, fn)$x)
  }
  
  # compute Uib with Tib and bootstrap samples
  for (i in 1:numB) {
    u <-
      (exp(-(t_i / B[i, 2]) ^ B[i, 1]) - exp(-(as.numeric(Tb[i]) / B[i, 2]) ^
                                               B[i, 1])) / (exp(-(t_i / B[i, 2]) ^ B[i, 1]))
    U <- rbind(U, c(u))
  }
  Ub <- U[1:numB + 1,] # save the computed Uib
  
  # compute the lower and upper alpha/2 sample quantiles of Uib
  Ub_sd <- sd(Ub)
  me <- qt(.95, 9) * sd(Ub) / sqrt(numB)
  u_l <- mean(Ub) - me
  u_u <- mean(Ub) + me
  
  # compute the lower bound of the predicted interval
  fn_l <- function(tx) {
    return(u_l -  (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                             shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
  }
  T_l <- nleqslv(100, fn_l)$x
  
  # compute the upper bound of the predicted interval
  fn_u <- function(tx) {
    return(u_u -  (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                             shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
  }
  T_u <- nleqslv(100, fn_u)$x
  
  # save the prediction interval
  interval <- rbind(interval, c(T_l, T_u))
  
}

# the prediction of remaining life with 90% confidence interval for individuals
interval_pred <- interval[1:nsample + 1,]
