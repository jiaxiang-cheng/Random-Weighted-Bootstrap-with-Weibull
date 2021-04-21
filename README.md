# Prediction-of-Remaining-Life-of-Power-Transformers
Reproduction of the work by Hong, Y., Meeker, W. Q., &amp; McCalley, J. D. (2009). Prediction of remaining life of power transformers based on left truncated and right censored lifetime data. Annals of Applied Statistics, 3(2), 857-879.

## Data Preparation
You may prepare the data set alighed with _master_data.xlsx_. The general structure and information included in our data set are introduced here:

- id: the identification of the samples (transformers);
- status: = 0 when the sample is censored (survived), = 1 when the sample failed already;
- trunc: = 0 when the sample is left-truncated, = 1 when the sample is not;
> **How to know if a sample is left-truncated or not?** when preparing the data set, get clear about when the observation of failures started. This is important as there would be bias without consideration of left truncation problem. In our data set, the start of observing failures is set as 2010. So if the sample was installed before 2010, then this sample is regarded as left-truncated, _trunc_ = 0. Otherwise, if the sample was installed after 2010, then it's not left-truncated with _trunc_ = 1.
- time: the lifetime of the sample. For the censored data, it is the last record of survived lifetime. For the failed samples, it is the age of transformer when it failed;
- time_lt: is equal to (2010 - installation year), only for the left-truncated samples;
- manuf: the manufacturer, if you have the information;
> _clear_: this is only for our data set, as we need to clear some data we don't use. Not necessary for common studies.

## Loading Data
After you prepared your own data set, you can run loading_data.R to load your data, as follows:
```
library(readxl)

data_set_raw <- read_excel("master_data.xlsx", sheet = "master")
data_set <- data_set_raw[which(data_set_raw$clear == 0),]

id <- data.matrix(data_set[, "id"])
manuf <- as.factor(data.matrix(data_set[, "manuf"]))
time <- data.matrix(data_set[, "time"])
time_lt <- data.matrix(data_set[, "time_lt"])
status <- data.matrix(data_set[, "status"])
trunc <- data.matrix(data_set[, "trunc"])

ndim <- dim(id)[1]
```
For normal case, you only need one line to load the data:
```
data_set <- read_excel("your_file.xlsx", sheet = "your_sheet")
```

## Maximum Likelihood Estimation
After loading your data set or ours, you can do the maximum likelihood estimation to obtain the estimated scale and shape parameters for the two-parameter Weibull distribution. If you get left truncation problem into consideration, you only need to run the following lines:
```
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
```
Thus, you already obtained the shape and scale parameter as _**shape_lt**_ and _**scale_lt**_.

However, if you are interested, you can also run the whole script and you will obtain the comparison plot between the estimation results with and withou consideration of left truncation problems, as follows:
