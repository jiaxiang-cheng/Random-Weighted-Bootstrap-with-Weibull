# The random weighted bootstrap
# to generate bootstrap samples of the scale and shape parameters

# initialize the matrix for saving the bootstrap samples
B <- matrix(ncol = 2)
numB <- 100 # the number of bootstrap sample

for (val in 1:numB)
{
  # Simulate random values Zi that are i.i.d. from a Gamma distribution
  z <- rgamma(n = ndim, shape = 1, scale = 1)
  
  # The random weighted likelihood function
  logLikFun <- function(param) {
    beta <- param[1]
    ita <- param[2]
    c <- status[1:ndim]
    v <- trunc[1:ndim]
    t <- time[1:ndim]
    tL <- time_lt[1:ndim]
    f = log(beta / ita) + (beta - 1) * log(t / ita) - (t / ita) ^ beta
    fu = (t / ita) ^ beta
    fuL = (tL / ita) ^ beta
    sum(z * (
      c * v * f + c * (1 - v) * (f + fuL) + (1 - c) * v * (-fu) + (1 - c) * (1 -
                                                                               v) * (fuL - fu)
    ))
  }
  
  # MLE using maxLik
  mle_b <-
    maxLik(logLik = logLikFun, start = c(beta = 2, ita = 100))
  # obtain the bootstrap samples and store in B
  shape_lt_b <- mle_b$estimate[1]
  scale_lt_b <- mle_b$estimate[2]
  B <- rbind(B, c(shape_lt_b, scale_lt_b))
  
}

# The final bootstrap samples (empty values removed)
B <- B[1:numB + 1,]