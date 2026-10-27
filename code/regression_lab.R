# regressions lab

library(tidyverse)

#12.4.1 Develop Rgression Models


iris
df_set <- iris |> 
  filter(Species == "setosa")
df_set
lm(Sepal.Width ~ Petal.Width,
   data = df_set)

df_versi <- iris |> 
  filter(Species == "versicolor")
df_versi
lm(Sepal.Width ~ Petal.Width,
   data = df_versi)

df_virg <- iris |> 
  filter(Species == "virginica")
lm(Sepal.Width ~ Petal.Width,
   data = df_virg)

# perform at once using lapply() or 'for' loop
v_sp <- unique(iris$Species)
list_lm_one <- lapply( X = 1:3,
                  function(s) {
                    data <- iris |> 
                      filter(Species == v_sp[s])
                    
                    lm(Sepal.Width ~ Petal.Width, data = data)
                  })

# for loop version
# 'list' is the object type that can store multiple types of data as a single object
# for example, list[[1]] dataframe, list [[2]] vector, list [[3]] model output, etc...
# in list, accessing each element uses sytnax [[]]

# 12.4.2 Multiple Explanatory Variables

v_sp <- unique(iris$Species)
list_lm_two <- lapply( X = 1:3,
                   function(s) {
                     data <- iris |> 
                       filter(Species == v_sp[s])
                     
                     lm(Sepal.Width ~ Petal.Width + Petal.Length, data = data)
                   })
list_lm_two[1] |> 
  summary()

df_virg <- iris |> 
  filter(Species == "virginica")
df_virg2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
   data = df_virg)

df_virg <- iris |> 
  filter(Species == "virginica")
df_virg1 <- lm(Sepal.Width ~ Petal.Width,
   data = df_virg)


df_set <- iris |> 
  filter(Species == "setosa")
df_set2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
   data = df_set)

df_set <- iris |> 
  filter(Species == "setosa")
df_set1 <- lm(Sepal.Width ~ Petal.Width,
   data = df_set)

df_versi <- iris |> 
  filter(Species == "versicolor")
df_versi2 <- lm(Sepal.Width ~ Petal.Width + Petal.Length,
   data = df_versi)

df_versi <- iris |> 
  filter(Species == "versicolor")
df_versi1 <- lm(Sepal.Width ~ Petal.Width,
   data = df_versi)

useless_test <- iris |> 
  mutate(useless = rnorm(150))

m0 <- lm(Sepal.Width ~ Petal.Width, data = useless_test)
m1 <- lm(Sepal.Width ~ Petal.Width + useless, data = useless_test)
summary(m0)
summary(m1)

summary(df_virg1)
summary(df_virg2)

summary(df_set1)
summary(df_set2)

summary(df_versi1)
summary(df_versi2)