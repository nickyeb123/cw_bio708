# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.
library(tidyverse)
library(janitor)
library(vegan)
library(MuMIn)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`
data_frame(CO2)

df_co2 <- tibble(CO2)
df_co2

# Q2
# Convert column names to lowercase and reassign to `df_co2`
df_co2 <- df_co2 |> 
  clean_names()

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# The figure should satify the following:
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.
df_co2 |> 
  ggplot(aes(x = conc,
             y = uptake,
             color = treatment)) + 
  geom_point() +
  facet_grid(. ~ type)


# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop a statistical model that examines the following effects simultaneously:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

m_co2 <- glm(uptake ~ conc * treatment,
             family = "gaussian",
             data = df_co2)
shapiro.test(residuals(m_co2))
qqnorm(residuals(m_co2))
qqline(residuals(m_co2), col = "red")

co2_quebec <- subset(df_co2,
                     type == "Quebec")

co2_miss <- subset(df_co2,
                  type == "Mississippi")
co2_quebec
co2_miss

mod_quebec <- glm(uptake ~ conc * treatment,
                  family = "gaussian",
                  data = co2_quebec)

mod_miss <- glm(uptake ~ conc * treatment,
                family = "gaussian",
                data = co2_miss)

summary(mod_quebec)
summary(mod_miss)

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:
# (no coding required for this question)

## The fitted model for Quebec plant origin showed a significant positive effect
# of CO2 assimilation rate on CO2 assimilation rate (Beta = 0.0224, p < 0.001),
# demonstrating that uptake increased as CO2 concentration increased.
# However, treatment (Beta = -4.16, p = 0.274) and the interaction between 
# concentration and treatment (Beta = 0.00134, p = 0.852) were not significant.
# This shows that chilled vs non-chilled Quebec plants had similar uptake rates
# and responded similarly to increasing CO2 concentrations.

# For Mississippi plants, ambient CO2 concentration had a significant positive
# effect on uptake (Beta = 0.0172, p < 0.001). Unlike Quebec plants, treatment 
# had a significant negative effect (B = -5.91, p = 0.023), demonstrating that
# chilled plants had lower CO2 assimilation rates than non-chilled plants. 
# Additionally, the interaction between concentration and treatment was
# significant (Beta = -0.00972),p = 0.048), showing that the effect of 
# increasing CO2 concentration depends on treatment. The response of uptake to
# increasing CO2 was weaker in chilled vs non-chilled Mississippi plants.

# Both plant origins showed increased CO2 assimilation as ambient CO2
# concentration increased, but the response to chilling differed between
# origins. Quebec plants were unaffected by chilling, whereas Mississippi plants
# were more sensitive to chilling, showing lower overall CO2 assimilation rates
# and reduced responses to increasing CO2 concentration under chilled conditions.


# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)
cnm

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")
df_bci

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)
df_env

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`
df_env <- df_env |> 
  clean_names()

df_env

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.
same_values <- sapply(df_env, function(x)
  length(unique(x)) == 1)
same_values

names(df_env)[same_values]

df_env_sub <- df_env[, !same_values]

df_env_sub


# Q8
# Calculate summary statistics for each plot using `df_bci`.
# Compute the following quantities by plot as columns:
# - n_sum: total count of all individuals across species
# - n1: count of the most abundant species (maximum count among species for each plot)
# - p: proportion of the most abundant species (n1 / n_sum)
#
# Ensure that each plot has one row representing a data point of the dominant species.
#
# Assign the resulting dataframe to `df_n`.
df_n <- df_bci |> 
  group_by(plot) |> 
  summarize(n_sum = sum(count),
            n1 = max(count),
            p = n1 / n_sum)
df_n
df_bci
# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.
df_m <- left_join(df_n,
                  df_env_sub, by = "plot")
df_m
df_env_sub
names(df_n)
names(df_env_sub)
# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species with a suitable error distribution. 
# - Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# - Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

# In the best predictive model, AIC = 517.3, included env_het and habitat. 
mod <- glm(cbind(n1, n_sum - n1) ~ env_het + stream + habitat,
           family = binomial,
           data = df_m)
mod
summary(mod)

options(na.action = "na.fail")
m <- mod
m_set <- dredge(m, rank = "AIC")
m_set
m_sub <- subset(m_set, delta < 2)

list_models <- get.models(object = m_set, subset = delta < 2)
list_models

list_models[[1]]

  
