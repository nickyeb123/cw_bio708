library(tidyverse)

# Descriptive Statistics - Lab

# Central Tendency --------------------------------------------------------

z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))

# arithmetic mean
(mu_z <- (sum(z) / length(z)))

#geometric mean
(mu_z_ge <- prod(z)^(1 / length(z)))

# median
(med_z <- median(z))

# histogram of z

(z_tibble <- tibble(z))

z_tibble |> 
  ggplot(aes(z)) + 
  geom_histogram() + 
  geom_vline(xintercept = mu_z, color = "red") + 
  geom_vline(xintercept = mu_z_ge, color = "blue") +
  geom_vline(xintercept = med_z, color = "yellow")

# new vector z_rev as -z + max(z) + 0.5, and repeat step 1-4
(z_rev <- (-z + max(z) + 0.5))

# arithmetic mean z_rev
(mu_z_rev <- mean(z_rev))

# geometric mean z_rev
(mu_z_rev_ge <- exp(sum(log(z_rev)))^(1 / length(z_rev)))

# median
(med_z_rev <- median(z_rev))

# histogram of z_rev
(z_rev_tibble <- tibble(z_rev))

z_rev_tibble |> 
  ggplot(aes(z_rev)) + 
  geom_histogram() + 
  geom_vline(xintercept = mu_z_rev, color = "red") + 
  geom_vline(xintercept = mu_z_rev_ge, color = "blue") +
  geom_vline(xintercept = med_z_rev, color = "yellow")

# Variation ---------------------------------------------------------------
# defining w
w <- rnorm(100, mean = 10, sd = 1)

# show first 10 elements in w
head(w, n = 10)
?head

# convert unit of w to milligram and create new vector "m"
(m <- w*1000)

# calculate SD and MAD for w and m
(mu_w <- mean(w))
(mad_w <- median(w))
(var_w <- sum((w - mean(w))^2) / length(w))
(sd_w <- sqrt(var_w))
(ad_w <- abs(w - median(w)))
median(mad_w)

(sd_w <- sqrt(sum((w - mean(w))^2) / length(w)))
(mad_w <- median(abs(w - median(w))))

(sd_m <- sqrt(sum((m - mean(m))^2) / length(m)))
(mad_m <- median(abs(m - median(m))))

#calculate CV and MAD/Median for w and m
mycv <- function(v)  {
  mu <- mean(v)
  s <- sd(v)
  cv <- s / mu
  
  return(cv)
}

mycv(m)
mycv(w)

mad_m / median(m)
mad_w / median(w)