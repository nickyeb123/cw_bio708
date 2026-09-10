# install.packages("tidyverse")

library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>%
  group_by(Species) %>%
  sample_n(3) %>%
  ungroup()

print(iris_sub)

# Filter
filter(iris_sub, Species == "virginica")

filter(iris_sub, Sepal.Width != "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, Species == "virginica" | Species == "versicolor")

filter(iris_sub, Sepal.Width > 5,
       Species == "versicolor")


arrange(iris_sub, desc(Sepal.Length))

iris_3 <- filter(iris_sub, Sepal.Width > 3.0)
iris_3

iris_setosa <- filter(iris_sub, Species == "setosa")
iris_setosa

iris_3_setosa <- arrange(iris_sub, Sepal.Width >3.0,
        Species == "setosa")
iris_3_setosa

# Select
select(iris_sub, starts_with("Sepal"))

select(iris_sub, ends_with("Width"))

# Mutate
x_max <- nrow(iris_sub)
x <- 1:x_max

iris_sub_row <- mutate(iris_sub, row_id = x)
iris_sub_row

arrange(iris_sub_row, desc(row_id))


mutate(iris_sub, sl_two_times = 2*Sepal.Length)

mutate(iris_sub, id_name = paste(Species, Sepal.Length))

iris_pw <- select(iris_sub, c(Petal.Width, Species))
iris_pw

iris_petal <- select(iris_sub, starts_with("Petal"))
iris_petal

iris_pw_two <- mutate(iris_sub, pw_two_times = 2*Petal.Width)
iris_pw_two


# Pipe %>%

df_vir <- filter(iris_sub, Species == "virginica")
select(df_vir, Sepal.Length)

select(filter(iris_sub, Species == "virginica"), Sepal.Length)

iris_sub |> 
  filter(Species == "virginica") %>% 
  select(Sepal.Length)

iris_sub |> 
  filter(Species == "setosa") |> 
  mutate(pw_two_times = 2*Petal.Width)


# Grouping
iris_sub <- iris_sub |> 
  group_by(Species) |> 
  mutate(mu_sl = mean(Sepal.Length)) |> 
  ungroup()
iris_sub


# Summarize
iris_sub |> 
  group_by(Species) |> 
  summarize(mu_sl = mean(Sepal.Length))


