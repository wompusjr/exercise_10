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
#step one
d <- read_csv("https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/AVONETdataset1.csv", col_names = TRUE)
d <- d |>
  select(Species1,Family1, Order1, Beak.Width, Beak.Depth, Beak.Length_Culmen, Tarsus.Length, Wing.Length, Tail.Length, Mass, Habitat, Migration, Trophic.Level,Trophic.Niche, Primary.Lifestyle, Min.Latitude,Max.Latitude,Centroid.Latitude,Range.Size)
mbeak <- lm(log(Beak.Length_Culmen) ~ log(Mass), data = d)
mtarsus <- lm(log(Tarsus.Length) ~ log(Mass), data = d)
d <- d |> mutate(relative.beak.length = mbeak$residuals) 
d <- d |> mutate(relative.tarsus.length = mtarsus$residuals) 
#step two
(p01<- ggplot(data = d |> drop_na(Primary.Lifestyle),
               aes(x = Primary.Lifestyle, y = relative.tarsus.length)) +
    geom_boxplot()) 
(p02 <- ggplot(data = d |> drop_na(Trophic.Niche),
               aes(x = Trophic.Niche, y = relative.beak.length)) +
    geom_boxplot())
#step three
hist(d$Range.Size) #super not-normal i need to log it to be normal
d <- d |>
  na.omit(d$Migration) |>
  mutate(Migration = as.factor(Migration))
m3 <- lm(log(Range.Size) ~ Migration, data = d)
summary(m3) #all migration are different than the ref level [migration1] and the base is significant
#pvalue for m1 v. m2 [<2e-16]
#pvaulue for m1 v. m3 [<2e-16]
d <- d |> mutate(Migration = relevel(Migration, ref = "2")) #switching ref level
(m32 <- lm(log(Mass) ~ Migration, data = d)) #comparing with new levels
summary(m32) #pvalue for m2 v. m3 [3.74e-10] 
#posthoc
m3 <- aov(log(Range.Size)~ Migration, data = d)
posthoc2 <- TukeyHSD(m3, which = "Migration", ordered = TRUE, conf.level = 0.95)
posthoc2 #all significantly different with p-values at 0
#step four
d <- d |>
  filter(Order1 == "Passeriformes")
(p03<- ggplot(data = d |> drop_na(Primary.Lifestyle),
              aes(x = Primary.Lifestyle, y = relative.beak.length)) +
    geom_boxplot()) 
(p04 <- ggplot(data = d |> drop_na(Trophic.Niche),
               aes(x = Trophic.Niche, y = relative.beak.length)) +
    geom_boxplot())
m4 <- lm(relative.beak.length ~ Primary.Lifestyle, data = d)
summary(m4)
m5 <- lm(relative.beak.length ~ Trophic.Level, data = d)
summary(m5)
#step five
m6 <- lm(d$relative.beak.length ~ d$Primary.Lifestyle + d$Trophic.Level)
summary(m6)
#step six
m7 <- lm(relative.beak.length ~ Primary.Lifestyle + Trophic.Niche + Primary.Lifestyle:Trophic.Niche, data = d)
summary(m7)
#step seven
interaction.plot(x.factor = d$Primary.Lifestyle, xlab = "Primary Lifestyle", trace.factor = d$Trophic.Level, trace.label = "Trophic Level",
                 response = d$relative.beak.length, fun = base::mean, ylab = "Relative Beak Length")
#step eight

