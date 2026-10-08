rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)
# loading plant height data from csv
df_h0 <- read_csv("data_src/data_plant_height.csv")
print(df_h0)



# 100 sub-data sets -> 50 sample size and 100 sample size -----------------

# 50-sample datasets
mu_i50 <- var_i50 <- NULL
for (i in 1:100) {
  df_i50 <- df_h0 |> 
    sample_n(size = 50)
  
  
  # mean for a subset
  mu_i50[i] <- mean(df_i50$height)
  
  # var for a subset
  var_i50[i] <- var(df_i50$height)
  
  
}
print(mu_i50)

# histogram for 50 sample dataset
df_sample50 <- tibble(mu_hat50 = mu_i50,
                    var_hat50 = var_i50)
df_sample50

g_mu50 <- df_sample50 |> 
  ggplot(aes(x = mu_hat50)) + 
  geom_histogram()
  
g_mu50

g_var50 <- df_sample50 |> 
  ggplot(aes(x = var_hat50)) + 
  geom_histogram()
  
g_var50

# 100-sample datasets
mu_i100 <- var_i100 <- NULL
for (i in 1:100) {
  df_i100 <- df_h0 |> 
    sample_n(size = 100)
  
  
  # mean for a subset
  mu_i100[i] <- mean(df_i100$height)
  
  # var for a subset
  var_i100[i] <- var(df_i100$height)
  
}

# histogram for 100 sample dataset
df_sample100 <- tibble(mu_hat100 = mu_i100,
                      var_hat100 = var_i100)
df_sample100

g_mu100 <- df_sample100 |> 
  ggplot(aes(x = mu_hat100)) + 
  geom_histogram()

g_mu100

g_var100 <- df_sample100 |> 
  ggplot(aes(x = var_hat100)) + 
  geom_histogram()

g_var100


# repeat step 1 with df_h10 instead of df_h0 ------------------------------

df_h10 <- df_h0 |> 
  filter(height >= 10)
df_h10

# 50 sample data set
mu_i10_50 <- var_i10_50 <- NULL
for (i in 1:100) {
  df_i10_50 <- df_h10 |> 
    sample_n(size = 50)
  
  
  # mean for a subset
  mu_i10_50[i] <- mean(df_i10_50$height)
  
  # var for a subset
  var_i10_50[i] <- var(df_i10_50$height)
  
  
}
print(mu_i10_50)

# histogram for 50 sample dataset for df_h10
df_sample10_50 <- tibble(mu_hat10_50 = mu_i10_50,
                      var_hat10_50 = var_i10_50)
df_sample10_50

g_mu10_50 <- df_sample10_50 |> 
  ggplot(aes(x = mu_hat10_50)) + 
  geom_histogram()

g_mu10_50

g_var10_50 <- df_sample10_50 |> 
  ggplot(aes(x = var_hat10_50)) + 
  geom_histogram()

g_var10_50

# 100 sample dataset
mu_i10_100 <- var_i10_100 <- NULL
for (i in 1:100) {
  df_i10_100 <- df_h10 |> 
    sample_n(size = 100)
  
  
  # mean for a subset
  mu_i10_100[i] <- mean(df_i10_100$height)
  
  # var for a subset
  var_i10_100[i] <- var(df_i10_100$height)
  
}

# histogram for 100 sample dataset
df_sample10_100 <- tibble(mu_hat10_100 = mu_i10_100,
                       var_hat10_100 = var_i10_100)
df_sample10_100

g_mu10_100 <- df_sample10_100 |> 
  ggplot(aes(x = mu_hat10_100)) + 
  geom_histogram()

g_mu10_100

g_var10_100 <- df_sample10_100 |> 
  ggplot(aes(x = var_hat10_100)) + 
  geom_histogram()

g_var10_100




