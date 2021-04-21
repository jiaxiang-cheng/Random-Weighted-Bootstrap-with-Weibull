data_set_risk <- data_set[which(data_set$status == 0), ]

k <- dim(data.matrix(data_set_risk[, "time"]))[1]

tcut <- 2020
tfur <- 2021

U_K <- matrix(nrow = 10, ncol = ndim_bern)
for (tfur in 2021:2030) {
  for (b in 1:ndim_bern) {
    x_K <-
      (K_B[tfur - 2020, b] / 17815 + 0.5 - Mu_K[tfur - 2020, b]) / Sig_K[tfur - 2020, b]
    U_K[tfur - 2020, b] <-
      pnorm(x_K) + Ga_K[tfur - 2020, b] * (1 - x_K ^ 2) * dnorm(x_K) / 6
  }
}

me_bern <- qt(.95, 9) * sd(U_K[1, ]) / sqrt(ndim_bern)
u_l_bern <- mean(U_K[1, ]) - me_bern
u_u_bern <- mean(U_K[1, ]) + me_bern

fn_l_bern <- function(tk) {
  return(u_l_bern -  (pnorm(tk) + mean(Ga_K[2021 - 2020, ]) * (1 - tk ^ 2) * dnorm(tk) / 6))
}
T_l_bern <- nleqslv(1, fn_l_bern)$x

fn_l_bern2 <- function(tk2) {
  return(T_l_bern -
           (tk2 / 17815 + 0.5 - mean(Mu_K[2021 - 2020, ])) / mean(Sig_K[2021 - 2020, ]))
}
T_l_bern2 <- nleqslv(1000, fn_l_bern2)$x






fn_u_bern <- function(tk) {
  x_K <-
    (tk / 17815 + 0.5 - mean(Mu_K[2021 - 2020, ])) / mean(Sig_K[2021 - 2020, ])
  return(u_u_bern -  (pnorm(x_K) + mean(Ga_K[2021 - 2020, ]) * (1 - x_K ^ 2) * dnorm(x_K) / 6))
}
T_u_bern <- nleqslv(10, fn_u_bern)$x

interval <- rbind(interval, c(T_l, T_u))
