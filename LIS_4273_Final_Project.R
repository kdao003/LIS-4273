library(tidyverse)
#Read in data
data <- read.csv("BEAT_AML_DATA.csv", header = TRUE)

#Replace BMBP and Age NA values to 0
data1 <- data %>%
  mutate(Bone.Marrow.Blast.Percentage = replace_na(Bone.Marrow.Blast.Percentage, 0)) %>%
  mutate(Age.at.Diagnosis = replace_na(Age.at.Diagnosis, 0)) %>%
  mutate(Overall.Survival..Months. = replace_na(Overall.Survival..Months., 0)) 

#Filter out rows that we replaced NA to 0
data1 <- data1 %>%
  filter(Bone.Marrow.Blast.Percentage != 0) %>%
  filter(Overall.Survival..Months. != 0)

#Renaming columns, selecting and editing data
data4 <- data1 %>%
  select(Overall.Survival..Months., Age.at.Diagnosis, Bone.Marrow.Blast.Percentage) %>%
  rename(BMBP = Bone.Marrow.Blast.Percentage) %>%
  rename(Survival = Overall.Survival..Months.) %>%
  rename(Age = Age.at.Diagnosis) %>%
  mutate(Survival = ifelse(Survival > 0.5, ceiling(Survival), floor(Survival))) %>%
  mutate(Survival_Group = case_when(Survival < 12 ~ "Less than 1-year Survival",
                            Survival > 36 ~ "Greater than 3-year Survival",
                            TRUE ~ "1-3 year Survival"))

library(ggplot2)
ggplot(
  data = data4,
  mapping = aes(x = Age, y = BMBP, color = Survival)
) +
  labs(
    y = "BMBP (%)",
    x = "Age (Years)",
    title = "Survival for Age vs. BMBP"
  )+
  scale_color_gradient2(
    name = "Survival (Months)",
    high = "blue",
    mid = "green",
    low = "red",
    limits = c(0,150),
    breaks = c(125,100,75,50,25,0),
    labels = c("125 Strong-Survival", "100", "75", "50", "25", "0 Poor-Survival")
  ) +
  geom_point()

#Anova Test
res.aov1 <- aov(Survival ~ Age + BMBP, data = data4)
summary(res.aov1)

#Anova with Interaction Effect
res.aov2 <- aov(Survival ~ Age * BMBP, data = data4)
summary(res.aov2)















