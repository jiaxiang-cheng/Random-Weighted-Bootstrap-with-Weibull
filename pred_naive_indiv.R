library(nleqslv)
#library(pracma)

interval_naive <- matrix(ncol = 2)

for (sample in 1:10) {

  t_i <- time[sample]
  
  fn_l <- function(tx) {
    #f =  (exp(-(t_i/scale_lt)^shape_lt)-exp(-(t/scale_lt)^shape_lt))/(1-exp(-(t_i/scale_lt)^shape_lt))
    return(0.05 -  (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                             shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
  }
  T_l_naive <- nleqslv(100, fn_l)$x
  
  fn_u <- function(tx) {
    #f =  (exp(-(t_i/scale_lt)^shape_lt)-exp(-(t/scale_lt)^shape_lt))/(1-exp(-(t_i/scale_lt)^shape_lt))
    return(0.95 -  (exp(-(t_i / scale_lt) ^ shape_lt) - exp(-(tx / scale_lt) ^
                                                             shape_lt)) / (exp(-(t_i / scale_lt) ^ shape_lt)))
  }
  T_u_naive <- nleqslv(100, fn_u)$x
  
  interval_naive <- rbind(interval_naive, c(T_l_naive, T_u_naive))
}