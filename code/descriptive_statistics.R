# Descriptive Statistics
library(tidyverse)
# construct vectors x and y
x <- c(15.9, 15.1, 21.9, 13.3, 24.4)
y <- c(15.9, 15.1, 21.9, 53.3, 24.4)


# Central Tendency --------------------------------------------------------

# arithmetic mean -> more sensitive to outlier, always larger than ge mean
(mu_x <- sum(x) / length(x))
mean(x)

(mu_y <- sum(y) / length (y))
mean(y)


# geometric mean -> less sensitive to outlier
mu_x_ge <- prod(x)^(1/length(x))
exp(sum(log(x)) / length(x))

mu_y_ge <- prod(y)^(1/length(y))
(exp(sum(log(y)) / length(y)))


# median
(med_x <- median(x))
(med_y <- median(y))


# Variation ---------------------------------------------------------------

# variance
var_x <- sum((x - mean(x))^2) / length(x)
var_y <- sum((y - mean(y))^2) / length(y)

# standard deviation
sqrt(var_x)
sqrt(var_y)

# inter-quartile range (iqr)
x_l <- quantile(x, 0.25)
x_h <- quantile(x, 0.75)
(iqr_x <- x_h - x_l)

y_q <- quantile(y, c(0.25, 0.75))
(iqr_y <- y_q[2] - y_q[1])

# MAD
(ad_x <- abs(x - median(x)))
median(ad_x)

(ad_y <- abs(y - median(y)))  
median(ad_y)

# coefficient of variation
sd_x <- sqrt(var_x)
(cv_x <- sd_x / mu_x)

sd_y <- sqrt(var_y)
(cv_y <- sqrt(var_y))



# extra -------------------------------------------------------------------

mycv <- function(v)  {
  mu <- mean(v)
  s <- sd(v)
  cv <- s / mu
  
  return(cv)
}

mycv1 <- function(v) {
  mu <- mean(v)
  s <- sqrt(sum((v - mean(v))^2) / length(v))
  cv <- s / mu
  
  return(cv)
}

mycv
mycv(x)

mycv1(x)
