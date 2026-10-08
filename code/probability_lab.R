library(tidyverse)
# Normal Distribution -----------------------------------------------------
# Generate a variable with 50 observations
x <- rnorm(50, mean = 6, sd = 10)


x_min <- floor(min(x))
x_max <- ceiling(max(x))
bin <- seq(x_min, x_max, by = 1)
mu <- mean(x)
sigma <- sd(x)
df_x <- tibble(x = x)


# calculate probability for each bin

p <- NULL
for (i in 1:(length(bin) - 1)) {
  xu <- pnorm(bin[i+1], mean = mu, sd = sigma)
  x1 <- pnorm(bin[i], mean = mu, sd = sigma)
  
  p[i] <- xu - x1
}
 
df_prob <- tibble(
  p = p, 
  bin = bin[-length(bin)] + 0.5) |> 
         mutate(freq = p * length(x))

df_x |> 
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon",
            linetype = "dotted"
  )



# Poisson Distribution ----------------------------------------------------

# generate poisson-distrubted random numbers
z <- rpois(
  n = 1000, 
  lambda = 10)


# sample mean
lambda <- mean(z)

# prepare bins
bins <- seq(min(z), max(z), by = 1)

# probability mass: dpois() returns "probability" (not probability density)
# because its representation is discrete
# continuous distributions (e.g., normal) return "probability density"
pm <- dpois(
  x <- bins, 
  lambda = lambda
)

# prepare data frames
df_z <- tibble(z = z)

df_prob <- tibble(
  pm = pm,
  bins = bins
) |> 
  mutate(freq = pm * nrow(df_z))

# draw figures
df_z |> 
  ggplot(
    aes(x = z)
  ) + 
  geom_histogram(
    binwidth = 0.5,
    center = 0
  ) +
  geom_point(
    data = df_prob, # switch data frame
    aes(
      x = bins,
      y = pm
    ),
    color = "skyblue"
  ) +
  geom_line(
    data = df_prob,
    aes(
      x = bins,
      y = freq
      ),
    color = "skyblue",
    linetype = "dashed"
  )
  







