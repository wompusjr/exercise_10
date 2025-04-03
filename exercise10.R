library(tidyverse)
library(mosaic)
#####
#step one
d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv", col_names = TRUE)
d <- d |>
  select(Species1,Family1, Order1, Beak.Width, Beak.Depth, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level,Trophic.Niche,Min.Latitude,Max.Latitude,Centroid.Latitude,Range.Size) |>
  mutate(Migration = as.factor(Migration)) #winnowing the dataset and changing migration into a factor
(p1 <- ggplot(data = d |> drop_na(Trophic.Level),
              aes(x = Trophic.Level, y = log(Mass))) +
    geom_boxplot())#boxplot for log(Mass) by trophic level
(p2 <- ggplot(data = d |> drop_na(Migration),
              aes(x = Migration, y = log(Mass))) +
    geom_boxplot()) #boxplot for log(Mass) by migration 
#step two
(m1 <- lm(log(Mass) ~ Trophic.Level, data = d)) 
(m2 <- lm(log(Mass) ~ Migration, data = d)) 
summary(m1) #the f-statistic is high enough to suggest that mass is associated trophic level #p-value of <2.2e-16
summary(m2) ##the f-statistic is high enough to suggest that mass is associated trophic level #p-value of <2.2e-16
#migration 1, the reference level, is significantly different from both migration2 (p <2e-16) and migration3 (p 3.02e-13)
d <- d |> mutate(Migration = relevel(Migration, ref = "2")) #switching ref level
(m22 <- lm(log(Mass) ~ Migration, data = d)) #comparing with new levels
summary(m22) #migration 2 is significantly different from both migration1 (p <2e-16) and migration3 (p 6.67e-09)
#step three
m2 <- aov(log(Mass)~ Migration, data = d)
posthoc <- TukeyHSD(m2, which = "Migration", ordered = TRUE, conf.level = 0.95)
posthoc  # all migration classes significantly differ
#step four
library(infer)
d <- d |> mutate(logMass = log(Mass)) # creating logMass so that specify can work
original.F <- aov(log(Mass) ~ Trophic.Level, data = d) |>
  broom::tidy() |>
  filter(term == "Trophic.Level") #getting the f-statistic
permuted.F <- d |>
  specify(logMass ~ Trophic.Level) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate(stat = "F")
(p.value <- permuted.F |> get_p_value(obs_stat = original.F$statistic, direction = "greater")) #pvalue equals 0
#####
