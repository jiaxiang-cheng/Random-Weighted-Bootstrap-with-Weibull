

x <- matrix(ncol = 2)
num <- 100

for (val in 1:num)
{
  z <- rgamma(n = 17860, shape = 1, scale = 1)
  
  logLikFun <- function(param) {
    beta <- param[1]
    ita <- param[2]
    c <- status[1:17860]
    v <- trunc[1:17860]
    t <- time[1:17860]
    tL <- time_lt[1:17860]
    #f = (beta/ita)*((t/ita)^(beta-1))*exp((-t/ita)^beta)
    f = log(beta / ita) + (beta - 1) * log(t / ita) - (t / ita) ^ beta
    #fu = 1-exp((-t/ita)^beta)
    fu = (t / ita) ^ beta
    #fuL = 1-exp((-tL/ita)^beta)
    fuL = (tL / ita) ^ beta
    sum(z * (
      c * v * f + c * (1 - v) * (f + fuL) + (1 - c) * v * (-fu) + (1 - c) * (1 -
                                                                               v) * (fuL - fu)
    ))
  }
  
  mle_b <-
    maxLik(logLik = logLikFun, start = c(beta = 2, ita = 100))
  #summary(mle_b)
  shape_lt_b <- mle_b$estimate[1]
  scale_lt_b <- mle_b$estimate[2]
  x <- rbind(x, c(shape_lt_b, scale_lt_b))
}

B <- x[1:num + 1, ]
