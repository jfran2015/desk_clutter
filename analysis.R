library(tidyverse)
library(stats)
library(rstatix)

#this runs the R-file creation program first to get the files to work with
source("file_creation.R")


#===============================
#Flanker Analysis
#===============================

#removing unusable data and turing it into NA
flanker_analysis <- flanker %>%
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 150, NA, RT),
         RT = ifelse(RT >= 2000, NA, RT),
         accuracy = ifelse(status==1 | status==2, status, NA),
         accuracy = ifelse(accuracy==2, 0, accuracy))

#formating flanker summary to a form that can be analyzed by aov()
flanker_summary <- flanker_analysis %>% 
  filter(#status == 1,
    trial > 12) %>% 
  group_by(subn, congruency, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(accuracy, na.rm=TRUE)) %>% 
  mutate(condition = as.factor(condition),
         congruency = as.factor(congruency))

#ANOVA with DV response time and between group IV condition and within group IV congruency
aov_RT_condition_congruency <- aov(meanRT ~ condition*congruency + Error(subn/congruency), data = flanker_summary)
summary(aov_RT_condition_congruency)

#ANOVA with DV accuracy and between group IV condition and within group IV congruency
aov_accuracy_condition_congruency <- aov(meanAccuracy ~ condition*congruency + Error(subn/congruency), data = flanker_summary)
summary(aov_accuracy_condition_congruency)

#create a table showing means
model.tables(aov_RT_condition_congruency, "means")
model.tables(aov_accuracy_condition_congruency, "means")

#===============================
#Cueing Analysis
#===============================

cueing_analysis <- cueing %>%
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 150, NA, RT),
         RT = ifelse(RT >= 2000, NA, RT),
         accuracy = ifelse(status==1 | status==2, status, NA),
         accuracy = ifelse(accuracy==2, 0, accuracy))

cueing_summary <- cueing_analysis %>% 
  group_by(subn, ValidityNum, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(accuracy, na.rm=TRUE)) %>% 
  mutate(condition = as.factor(condition),
         ValidityNum = as.factor(ValidityNum))

#ANOVA with DV response time and between group IV condition and within group IV congruency
cueing_aov_RT_condition_congruency <- aov(meanRT ~ condition*ValidityNum + Error(subn/ValidityNum), data = cueing_summary)
summary(aov_RT_condition_congruency)

#ANOVA with DV accuracy and between group IV condition and within group IV congruency
cueing_aov_accuracy_condition_congruency <- aov(meanAccuracy ~ condition*ValidityNum + Error(subn/ValidityNum), data = cueing_summary)
summary(aov_accuracy_condition_congruency)

#create a table showing means
model.tables(cueing_aov_RT_condition_congruency, "means")
model.tables(cueing_aov_accuracy_condition_congruency, "means")

#===============================
#N-back Analysis
#===============================

nback_analysis <- nback %>% 
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 150, NA, RT),
         RT = ifelse(block == 1, NA, RT),
         score = ifelse(block == 1, NA, score))

nback_summary <- nback_analysis %>% 
  group_by(subn, trialType, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(score, na.rm=TRUE)) %>% 
  mutate(condition = as.factor(condition),
         trialType = as.factor(trialType))


#===============================
#Go, no-go Analysis
#===============================

gonogo_analysis <- gonogo %>% 
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 150, NA, RT),
         errorStatus = ifelse(errorStatus== 1, 0, 1))

gonogo_summary <- gonogo_analysis %>% 
  group_by(subn, type, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(errorStatus, na.rm=TRUE)) %>% 
  mutate(condition = as.factor(condition),
         type = as.factor(type))

#ANOVA with DV response time and between group IV condition and within group IV congruency
gonogo_aov_RT_condition_type <- aov(meanRT ~ condition*type + Error(subn/type), data = gonogo_summary)
summary(gonogo_aov_RT_condition_type)

#ANOVA with DV accuracy and between group IV condition and within group IV congruency
gonogo_aov_accuracy_condition_type <- aov(meanAccuracy ~ condition*type + Error(subn/type), data = gonogo_summary)
summary(gonogo_aov_accuracy_condition_type)

#create a table showing means
model.tables(gonogo_aov_RT_condition_type, "means")
model.tables(gonogo_aov_accuracy_condition_type, "means")

