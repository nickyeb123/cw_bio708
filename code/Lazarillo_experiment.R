# Load packages
library(readxl)
library(tidyverse)

# Read Excel file directly into a tibble
lazv2 <- read_excel("data_src/Lazarillo_dataV1.xlsx",
                   sheet = "Sheet1")
lazv1 <- read_excel("data_src/Lazarillo_dataV1.xlsx",
sheet = "Sheet0")

# View the tibble
lazv1
lazv2

# re-tibalize

  
lazv2 |> 
  group_by(condition) |> 
  summarize(
    mean_Q2N = mean(Q2N, na.rm = TRUE),
    sd_norm = sd(Q2N, na.rm = TRUE)
  )



# combining two data sets
all_data <- bind_rows(lazv1, lazv2)
all_data

lazv2 <- lazv2 |> 
  mutate(condition = "lazarillo")

lazv1 <- lazv1 |> 
  mutate(condition = "lazaro")

all_data <- all_data |> 
  mutate(
    empathy_norm = rowMeans(select(all_data, Q2N, Q3N, Q4N),
                            na.rm = TRUE
                            )
  )

all_data <- all_data |> 
  mutate(
    empathy_norm = (Q2N + Q3N + Q4N) / 3
  )
all_data$empathy_norm
View(all_data)

all_data |> 
  group_by(condition) |> 
  summarize(
    q2_mean = mean(Q2, na.rm = TRUE),
    q3_mean = mean(Q3, na.rm = TRUE),
    q4_mean = mean(Q4, na.rm = TRUE)
  )

all_data |> 
  group_by(condition) |> 
  summarize(
    q2N_mean = mean(Q2N, na.rm = TRUE),
    q3N_mean = mean(Q3N, na.rm = TRUE),
    q4N_mean = mean(Q4N, na.rm = TRUE)
  )

all_data <- all_data |> 
  mutate(
    empathy_norm_mean = (Q2N + Q3N + Q4N)/3
  )

t.test(empathy_norm_mean ~ condition,
       data = all_data)


ggplot(all_data_norm_mean,
      aes(x = condition,
           y = empathy_norm_mean)) +
  stat_summary(fun = mean,
               geom = "bar") + 
  theme_minimal()

ggplot(all_data_norm,
       aes(x = condition,
           y = empathy_norm)) +
  stat_summary(fun = mean,
               geom = "bar") + 
  theme_minimal()

t.test(empathy_norm ~ condition, data = all_data)

t.test(Q2N ~ condition, data = all_data)
t.test(Q3N ~ condition, data = all_data)
t.test(Q4N ~ condition, data = all_data)