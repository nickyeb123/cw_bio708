pacman::p_load(tidyverse)
library(pwr)


# 1.) Create figures similar to Figure 11.1 -------------------------------

df_anova <- PlantGrowth
df_anova |> 
  mutate(
    group_label = case_when(
      group == "ctrl" ~ "Control",
      group == "trt1" ~ "Treatment 1",
      group == "trt2" ~ "Treatment 2"
    )
  ) |> 
  ggplot(aes(x = group,
             y = weight)) + 
  geom_violin(draw_quantiles = 0.5,
              alpha = 0.2) +
  geom_jitter(alpha = 0.2)


# Conduct an ANOVA --------------------------------------------------------


# between-group variability
mu <- mean(df_anova$weight)

df_g <- df_anova |> 
  group_by(group) |> 
  summarize(mu_g = mean(weight),
            dev_g = (mu_g - mu)^2,
            n = n())
print(df_g)

df_g <- df_g |> 
  mutate(ss = dev_g * n)

print(df_g)

s_b <- sum(df_g$ss)
print(s_b)
# within-group variability
df_i <- df_anova |> 
  group_by(group) |> 
  mutate(mu_g = mean(weight)) |> 
  ungroup() |> 
  mutate(dev_i = (weight - mu_g)^2)

df_i_g <- df_i |> 
  group_by(group) |> 
  summarize(ss = sum(dev_i))

print(df_i_g)

s_w <- sum(df_i_g$ss)
print(s_w)

n_g <- n_distinct(df_anova$group)
s2_b <- s_b / (n_g - 1)
print(s2_b)

s2_w <- s_w / (nrow(df_anova) - n_g)
print(s2_w)

# f-statistic f = (between-group variance)/(within-group variance)
f_value <- s2_b / s2_w
print(f_value)

m <- aov(formula = weight ~ group,
         data = df_anova)
print(m)

summary(m)



# Power Analysis ----------------------------------------------------------

pwr::pwr.anova.test(
k = 3,
f = 0.5,
sig.level = 0.05,
power = 0.8
)

pwr::pwr.anova.test(
  k = 3,
  n = 10,
  f = 0.5,
  sig.level = 0.05)

pwr::pwr.anova.test(
  k = 6,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8)

pwr::pwr.anova.test(
  k = 8,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8
)

pwr::pwr.anova.test(
  k = 5,
  f = 0.25,
  sig.level = 0.5,
  power = 0.8
)


