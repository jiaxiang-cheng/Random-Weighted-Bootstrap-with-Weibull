# Prediction of Remaining Life of Power Transformers
Reproduction of the work by Hong, Y., Meeker, W. Q., &amp; McCalley, J. D. (2009). Prediction of remaining life of power transformers based on left truncated and right censored lifetime data. _Annals of Applied Statistics_, 3(2), 857-879.

Author: **Jiaxiang Cheng**, _Nanyang Technological University_ 

[![DOI](https://zenodo.org/badge/359709446.svg)](https://zenodo.org/badge/latestdoi/359709446)

## Data Preparation
You may prepare the data set alighed with _master_data.xlsx_. The general structure and information included in our data set are introduced here:

- `id`: the identification of the samples (transformers);
- `status`: = 0 when the sample is censored (survived), = 1 when the sample failed already;
- `trunc`: = 0 when the sample is left-truncated, = 1 when the sample is not;
> **How to know if a sample is left-truncated or not?** when preparing the data set, get clear about when the observation of failures started. This is important as there would be bias without consideration of left truncation problem. In our data set, the start of observing failures is set as 2010. So if the sample was installed before 2010, then this sample is regarded as left-truncated, _trunc_ = 0. Otherwise, if the sample was installed after 2010, then it's not left-truncated with _trunc_ = 1.
- `time`: the lifetime of the sample. For the censored data, it is the last record of survived lifetime. For the failed samples, it is the age of transformer when it failed;
- `time_lt`: is equal to (2010 - installation year), only for the left-truncated samples;
- `manuf`: the manufacturer, if you have the information;
> `clear`: this is only for our data set, as we need to clear some data we don't use. Not necessary for common studies.

## Loading Data
After you prepared your own data set, you can run `loading_data.R` to load your data, as follows:
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
After loading your data set or ours, you can do the maximum likelihood estimation with `mle.R` to obtain the estimated scale and shape parameters for the two-parameter Weibull distribution. If you get left truncation problem into consideration, you only need to run the following lines:
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
Thus, you already obtained the shape and scale parameter as `shape_lt` and `scale_lt`.

However, if you are interested, you can also run the whole script and you will obtain the comparison plot between the estimation results with and withou consideration of left truncation problems, as follows:

<img width="600" alt="MLE comparison" src="https://user-images.githubusercontent.com/67684198/115524752-061ec600-a2c1-11eb-9be1-b705c3b794f2.png">

## Bootstrap Samples
At this stage, you can already get the naive prediction of remaining life interval for individuals by running `pred_naive_indiv.R`. But you may obtain quite a large interval as the naive prediction doesn't consider the uncertainties of the estimated parameters.

So, instead, bootstrap method is used to compensate the uncertainty of parameter estimation. By running `bootstrap.R`, you may obtain the a number of bootstrap samples generated through random weighted bootstrap. Here you can define the number of bootstrap samples you want:
```
# initialize the matrix for saving the bootstrap samples
B <- matrix(ncol = 2)
numB <- 100 # the number of bootstrap sample
```
You can change `numB`. Normally, for a reliable bootstrap application, the number should be **10000 or more**. But due to the limited device I have, I only tried at maximum of 1000, but it should have been at least 10000 instead.

## Prediction of Remaining Life for Individuals
After obtaining the bootstrap samples, you can start the prediction of remaining life of individual transformer with `predict_individual.R`. And you can customize which transformers you want to predict, as follows:
```
# the number of transformer to predict, this can be customized
nsample <- 100
```
You can change the `nsample` here, and then the prediction will be conducted for the first _nsample_ transformers. This can be modified according to your own interests. No need to start counting from the beginning.

Then after running the codes, you will have you intervals predicted corresponding to you selected samples in `interval_pred`, as follows:
```
# the prediction of remaining life with 90% confidence interval for individuals
interval_pred <- interval[1:nsample + 1,]
```
The lower and upper bounds will be recorded accordingly in this matrix, like this:
|               | V1            | V2    |
|:-------------: |:-------------:|:-------------:|
| 1      | 185.1475 | 216.6756 |
| 2      | 189.3531      |   221.7536 |
| 3 | 187.1826      |    219.0999 |

You can do the visualization with the matrix in your own style. And I prefer to visualize it using Matlab as follows. It can be seen from the following figure that with more bootstrap samples, the prediction intervals are getting converged:
![trial_bootstrap](https://user-images.githubusercontent.com/67684198/115533818-b1cc1400-a2c9-11eb-9909-90aece26918a.png)

## Prediction of Number of failures for Population
Now you can predict the number of failures in the future within the current population by running `predict_population.R`. Please notice if any new samples added in the later years the population should be updated and the prediction would be updated as well.
```
# the number of bootstrap samples used for simulation
ndim_bern <- 100 # cannot be greater than numB
tcut <- 2020 # the cutting year, end of current observation
tpred <- 2030 # the end year of prediction
tdelta <-
  1 # the accuracy of the prediction like 2021, 2022 or 2021.2, 2021.3..
```
In this part, you can still choose your bootstrap samples to use by changing `ndim_bern`, which have been generated with `bootstrap.R`. And still, the number of bootstrap samples should be 10000 and more to achieve optimal results. The computational complexity is even more tremendous for this part. And you can also change the current year `tcut`, the end of observation, according to your own data set. And the prediction horizon and accuracy can be adjusted as well by `tpred` and `tdelta`.

Then you will receive the results in `K_B`, the number of rows will be decided by your prediction horizon and accuracy, and the number of columns will be determined by the number of bootstrap samples you used, the more the better. And then you can visualize the results freely. Normally, you will want to get a mean prediciton like follows:

![predict_population](https://user-images.githubusercontent.com/67684198/115645043-3bbfbf80-a352-11eb-800d-516deb12e0e8.png)

> **Acknowledgement** Many thanks for the great work by Hong, Y., Meeker, W. Q., &amp; McCalley, J. D. (2009). Prediction of remaining life of power transformers based on left truncated and right censored lifetime data. _Annals of Applied Statistics_, 3(2), 857-879. For any issues, please feel free to contact me JIAXIANG002@E.NTU.EDU.SG.
