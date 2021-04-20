library(nleqslv)
#library(pracma)

interval <- matrix(ncol = 2)

for (sample in 1:10) {
  f <- runif(num)
  Ts <- list()
  U <- matrix()
  t_i <- time[sample]
  
  for (prob in f) {
    fn <- function(tx) {
      #f =  (exp(-(t_i/scale_lt)^shape_lt)-exp(-(t/scale_lt)^shape_lt))/(1-exp(-(t_i/scale_lt)^shape_lt))
      return(prob - (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                               shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
    }
    Ts <- rbind(Ts, nleqslv(100, fn)$x)
  }
  
  #Tb <- Ts[2:num+1,]
  Tb <- Ts
  
  for (i in 1:num) {
    u <-
      (exp(-(t_i / B[i, 2]) ^ B[i, 1]) - exp(-(as.numeric(Tb[i]) / B[i, 2]) ^
                                               B[i, 1])) / (exp(-(t_i / B[i, 2]) ^ B[i, 1]))
    U <- rbind(U, c(u))
  }
  
  Ub <- U[2:num + 1, ]
  
  Ub_sd <- sd(Ub)
  me <- qt(.95, 9) * sd(Ub) / sqrt(num)
  u_l <- mean(Ub) - me
  u_u <- mean(Ub) + me
  
  fn_l <- function(tx) {
    #f =  (exp(-(t_i/scale_lt)^shape_lt)-exp(-(t/scale_lt)^shape_lt))/(1-exp(-(t_i/scale_lt)^shape_lt))
    return(u_l -  (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                             shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
  }
  T_l <- nleqslv(100, fn_l)$x
  
  fn_u <- function(tx) {
    #f =  (exp(-(t_i/scale_lt)^shape_lt)-exp(-(t/scale_lt)^shape_lt))/(1-exp(-(t_i/scale_lt)^shape_lt))
    return(u_u -  (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                             shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
  }
  T_u <- nleqslv(100, fn_u)$x
  
  interval <- rbind(interval, c(T_l, T_u))
}



