library(Rlab)

# the number of bootstrap samples used for simulation
ndim_bern <- 100 # cannot be greater than numB
tcut <- 2020 # the cutting year, end of current observation
tpred <- 2030 # the end year of prediction
tdelta <-
  1 # the accuracy of the prediction like 2021, 2022 or 2021.2, 2021.3..

# the number of samples still at risk (survived)
data_set_risk <- data_set[which(data_set$status == 0),]
nrisk <- dim(data.matrix(data_set_risk[, "id"]))[1]

# initialize the matrix for saving the simulated number of failures
K_B <-
  matrix(nrow = as.integer((tpred - tcut) / tdelta), ncol = ndim_bern)
# Mu_K <- matrix(nrow = 10, ncol = ndim_bern) # used for CI prediction
# Sig_K <- matrix(nrow = 10, ncol = ndim_bern) # used for CI prediction
# Ga_K <- matrix(nrow = 10, ncol = ndim_bern) # used for CI prediction

for (i_bern in 1:ndim_bern) {
  # one bootstrap sample one full simulation
  for (tfur in seq(tcut + 1, tpred, tdelta)) {
    # prediction from next year
    K_B[as.integer((tfur - tcut) / tdelta), i_bern] <-
      0 # initialize the predicted number
    # Mu_K[tfur - 2020, i_bern] <-  0 # used for CI prediction
    # Sig_K[tfur - 2020, i_bern] <- 0 # used for CI prediction
    # Ga_K[tfur - 2020, i_bern] <- 0 # used for CI prediction
    # mu_K <- 0 # used for CI prediction
    # sig_K <- 0 # used for CI prediction
    # ga_K <- 0 # used for CI prediction
    for (idx_i in 1:nrisk) {
      # get the current age of the transformer
      tcur_i <- data.matrix(data_set_risk[, "time"])[idx_i]
      # get the future age at future year tfur
      tfur_i <- tfur - tcut + tcur_i
      # get the conditional probability of failure at future year
      rou <-
        (exp(-(tcur_i / B[i_bern, 2]) ^ B[i_bern, 1]) - exp(-(tfur_i / B[i_bern, 2]) ^
                                                              B[i_bern, 1])) / (exp(-(tcur_i / B[i_bern, 2]) ^ B[i_bern, 1]))
      # simulate the outcomes at future year with Bernoulli
      if (runif(1) < dbern(1, rou, log = FALSE)) {
        I <- 1 # simulated as failure at the future year
        # mu_K <- mu_K + rou # used for CI prediction
        # sig_K <- sig_K + rou * (1 - rou) # used for CI prediction
        # ga_K <- ga_K + rou * (1 - rou) * (1 - 2 * rou) # used for CI prediction
      } else{
        I <- 0 # simulated as survived at the future year
      }
      
      # record the number of failured simulated for the future year
      K_B[as.integer((tfur - tcut) / tdelta), i_bern] <-
        K_B[as.integer((tfur - tcut) / tdelta), i_bern] + I
      
    }
    # sig_K <- sqrt(sig_K) # used for CI prediction
    # ga_K <- sig_K ^ (-3) * ga_K # used for CI prediction
    # Mu_K[tfur - 2020, i_bern] <-  mu_K # used for CI prediction
    # Sig_K[tfur - 2020, i_bern] <-  sig_K # used for CI prediction
    # Ga_K[tfur - 2020, i_bern] <-  ga_K # used for CI prediction
  }
}
