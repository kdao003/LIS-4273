library(tidyverse)
#Read in data
data <- read.csv("BEAT_AML_DATA.csv", header = TRUE)

#Replace BMBP and Age NA values to 0
data1 <- data %>%
  mutate(Bone.Marrow.Blast.Percentage = replace_na(Bone.Marrow.Blast.Percentage, 0)) %>%
  mutate(Age.at.Diagnosis = replace_na(Age.at.Diagnosis, 0)) %>%
  mutate(Overall.Survival..Months. = replace_na(Overall.Survival..Months., 0)) 

#Find Medians of BMBP and Age
BMBP_med <- median(data1$Bone.Marrow.Blast.Percentage)
age_med <- median(data1$Age.at.Diagnosis)

#Assigning the 2 groups
data1 <- data1 %>%
  mutate(BMBP_Groups =ifelse(Bone.Marrow.Blast.Percentage > BMBP_med, "High_BMBP", "Low_BMBP")) %>%
  mutate(Age_Groups =ifelse(Age.at.Diagnosis > age_med, "Older_Adults", "Younger_Adults"))

#Filter out rows that we replaced NA to 0
data1 <- data1 %>%
  filter(Bone.Marrow.Blast.Percentage != 0) %>%
  filter(Overall.Survival..Months. != 0)

#selecting columns we want to keep
data2 <- data1 %>%
  select(Bone.Marrow.Blast.Percentage, Age.at.Diagnosis, BMBP_Groups, Age_Groups, Overall.Survival..Months.)

#Perform pivot operation
data3 <- data2 %>%
  pivot_wider(names_from = c(BMBP_Groups, Age_Groups), values_from = Bone.Marrow.Blast.Percentage) %>%
  rename(Survival = Overall.Survival..Months.) %>%
  rename(Age = Age.at.Diagnosis)

#Anova Test
data4 <- data1 %>%
  select(Overall.Survival..Months., Age.at.Diagnosis, Bone.Marrow.Blast.Percentage) %>%
  rename(BMBP = Bone.Marrow.Blast.Percentage) %>%
  rename(Survival = Overall.Survival..Months.) %>%
  rename(Age = Age.at.Diagnosis)

res.aov1 <- aov(Survival ~ Age + BMBP, data = data4)
summary(res.aov1)






















