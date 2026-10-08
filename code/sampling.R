#' Sampling
#' # parameter --> a measureable constant representative of the population
rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)

## data frame 1
h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)

df_h1 <- tibble(plant_id = 1:10, # a vector from 1 to 10 by 1
                height = h, # height
                unit = "cm") # unit
df_h1


# nrow() returns the number of rows
# while piping, "." refers to the dataframe inherited 
# i.e., nrow(.) counts the number of rows in df_h1
df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h1)
         
# data frame 2
h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

# read csv file from source
df_h0 <- read_csv("data_src/data_plant_height.csv")

## true mean
mu <- mean(df_h0$height)

## true variance
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

# random sampling
df_i <- df_h0 |> 
  sample_n(size = 10)

# for loop

# for loop
mu_i <- var_i <- NULL
for (i in 1:1000) {
  df_i <- df_h0 |> 
    sample_n(size = 10)
  
  
  # mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  # var for a subset
  var_i[i] <- sum((df_i$height - mu_i[i])^2) / nrow(df_i)
  
  
}

df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)
df_sample

g_mu <- df_sample |> 
  ggplot(aes(x = mu_hat)) + 
  geom_histogram() + 
  geom_vline(xintercept = mu)
g_mu

g_var <- df_sample |> 
  ggplot(aes(x = var_hat)) + 
  geom_histogram() + 
  geom_vline(xintercept = sigma2)

g_var

# how patchwork works
g_hor <- g_mu + g_var # horizontal orientation
g_ver <- g_mu / g_var # vertical orientation


# bias-corrected version --------------------------------------------------
var_ub_i <- var(df_i$height)
var_ub_i[i] <- var(df_i$height)
# for loop
var_ub_i <- NULL
for (i in 1:1000) {
  df_i <- df_h0 |> 
    sample_n(size = 10)
  
  
  # mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  # var for a subset
  # var()'s denominator is N-1, not N
  var_ub_i[i] <- var(df_i$height)
}

df_sample1 <- tibble(mu_hat = mu_i,
                    var_ub_hat = var_i)
df_sample1

g_mu <- df_sample1 |> 
  ggplot(aes(x = mu_hat)) + 
  geom_histogram() + 
  geom_vline(xintercept = mu)
g_mu

g_var_ub <- df_sample1 |> 
  ggplot(aes(x = var_ub_hat)) + 
  geom_histogram() + 
  geom_vline(xintercept = sigma2)

g_var_ub

# adding a new column to df_sample
df_sample2 <- df_sample1 |> 
  mutate(var_ub_hat = var_ub_i)
df_sample2

g_var_ub <- df_sample |> 
  ggplot(aes(x = var_ub_i)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(min(c(var_i, var_ub_i)),
                               max(c(var_i, var_ub_i))))

# combined figure
g_mu / g_var / g_var_ub
