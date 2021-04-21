library(survival)
library(ggplot2)
library(survminer)
library(Metrics)
library(maxLik)

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
  sum(c * v * f + c * (1 - v) * (f + fuL) + (1 - c) * v * (-fu) + (1 - c) *
        (1 - v) * (fuL - fu))
}

mle <- maxLik(logLik = logLikFun, start = c(beta = 2, ita = 100))
summary(mle)

shape_lt <- mle$estimate[1]
scale_lt <- mle$estimate[2]

#####---------------------------------------------------------------------------
# MLE without left truncation considered

logLikFun <- function(param) {
  beta <- param[1]
  ita <- param[2]
  c <- status[1:17000]
  #v <- trunc
  t <- time[1:17000]
  #tL <- time_lt
  #f = (beta/ita)*((t/ita)^(beta-1))*exp((-t/ita)^beta)
  f = log(beta / ita) + (beta - 1) * log(t / ita) - (t / ita) ^ beta
  #fu = 1-exp((-t/ita)^beta)
  fu = (t / ita) ^ beta
  #fuL = 1-exp((-tL/ita)^beta)
  #fuL = (tL/ita)^beta
  sum(c * f + (1 - c) * (-fu))
}

mle <- maxLik(logLik = logLikFun, start = c(beta = 2, ita = 100))
summary(mle)

shape_no_lt <- mle$estimate[1]
scale_no_lt <- mle$estimate[2]

#####---------------------------------------------------------------------------
# Plotting Weibull Distributions:

x_lower_wei <- 0
x_upper_wei <- 50

# Excluded ylimits this time.

ggplot(data.frame(x = c(x_lower_wei , x_upper_wei)), aes(x = x)) +
  xlim(c(x_lower_wei , x_upper_wei)) +
  stat_function(
    fun = dweibull,
    args = list(shape = shape_lt, scale = scale_lt),
    aes(colour = "with")
  ) +
  stat_function(
    fun = dweibull,
    args = list(shape = shape_no_lt, scale = scale_no_lt),
    aes(colour = "without")
  ) +
  #stat_function(fun = dweibull, args = list(shape = 3, scale = 4), aes(colour = "3 & 4")) +
  scale_color_manual("Legend", values = c("blue", "green")) +
  labs(x = "\n Time", y = "f(x) \n",
       title = "Weibull Probability Density Function") +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(
      face = "bold",
      colour = "black",
      size = 12
    ),
    axis.title.y = element_text(
      face = "bold",
      colour = "black",
      size = 12
    ),
    legend.title = element_text(face = "bold", size = 10),
    legend.position = "right"
  )