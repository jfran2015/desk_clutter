library(tidyverse)
library(stats)
library(rstatix)
library(CGPfunctions)
library(BayesFactor)
library(here)

#this runs the R-file creation program first to get the files to work with
source("file_creation.R")


#===============================
# Flanker Analysis ----
#===============================

#removing unusable data and turing it into NA
flanker_analysis <- flanker %>%
  mutate(subn = as.factor(subn),
         RT = ifelse(RT > 1500, NA, RT),
         RT = ifelse(RT <= 100, NA, RT),
         RT = ifelse(RT > mean(RT, na.rm=TRUE)+2.5*sd(RT, na.rm = TRUE), NA, RT),
         RT = ifelse(RT < mean(RT, na.rm=TRUE)-2.5*sd(RT, na.rm = TRUE), NA, RT),
         accuracy = ifelse(status==1 | status==2, status, NA),
         accuracy = ifelse(accuracy==2, 0, accuracy))

#formating flanker summary to a form that can be analyzed by aov()
flanker_summary <- flanker_analysis %>% 
  filter(trial > 12) %>% 
  group_by(subn, Congruency, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(accuracy, na.rm=TRUE)*100) %>% 
  mutate(condition = as.factor(condition),
         Congruency = as.factor(Congruency)) %>% 
  filter(meanAccuracy >= 80)

flanker_summary$condition <- recode_factor(flanker_summary$condition, 
                                           O = "Organized", C = "Cluttered")

flanker_summary$Congruency <- recode_factor(flanker_summary$Congruency, 
                                            '0' = "Incongruent", '1' = "Congruent")

flanker_summary %>% 
  ggplot(aes(x=meanRT))+
  geom_histogram()+
  facet_wrap(~Congruency*condition)

#ANOVA with DV response time and between group IV condition and within group IV congruency
aov_RT_condition_congruency <- aov(meanRT ~ condition*Congruency + Error(subn/Congruency), data = flanker_summary)
summary(aov_RT_condition_congruency)


#bayesian analysis of RT
# bayes_flanker_RT <- anovaBF(meanRT ~ condition*Congruency + subn, data = flanker_summary, whichRandom = "subn")
# bayes_flanker_RT
# 
# plot(bayes_flanker_RT)
# 
# mcmcOut = posterior(bayes_flanker_RT, index = 3, iterations=10000)
# mcmcOut
# 
# plot(mcmcOut[,"mu"])

# library(bayesplot)
# 
# # 
# mcmc_intervals(mcmcOut, pars = c("condition-Cluttered", "condition-Organized"),
#                prob_outer = .95)
# 
# mcmc_intervals(mcmcOut, pars = c("Congruency-Congruent", "Congruency-Incongruent"),
#                prob_outer = .95)
# 
# mcmc_areas(
#   mcmcOut, 
#   pars = c("condition-Cluttered", 
#            "condition-Organized"),
#   prob = 0.8, # 80% intervals
#   prob_outer = 0.95, # 95%
#   point_est = "mean"
# )
# 
# mcmc_areas(
#   mcmcOut, 
#   pars = c("Congruency-Congruent", "Congruency-Incongruent"),
#   prob = 0.8, # 80% intervals
#   prob_outer = 0.95, # 95%
#   point_est = "mean"
# )

#ttestBF(x=flanker_summary2$meanRT[flanker_summary$condition == "Cluttered"],
 #       y=flanker_summary2$meanRT[flanker_summary$condition == "Organized"])

#ttestBF(x=flanker_summary$meanRT[flanker_summary$Congruency == "Congruent"],
 #       y=flanker_summary$meanRT[flanker_summary$Congruency == "Incongruent"])


#ANOVA with DV accuracy and between group IV condition and within group IV congruency
aov_accuracy_condition_congruency <- aov(meanAccuracy ~ condition*Congruency + Error(subn/Congruency), data = flanker_summary)
summary(aov_accuracy_condition_congruency)

#create a table showing means
model.tables(aov_RT_condition_congruency, "means")
model.tables(aov_accuracy_condition_congruency, "means")


#graph

#line graphs for a quick view at them.
Plot2WayANOVA(meanRT ~ condition*Congruency, dataframe = flanker_summary)
Plot2WayANOVA(meanAccuracy ~ condition*Congruency, dataframe = flanker_summary)


#creating my own line graph with ggplot2
flanker_plot_RT = ggplot(flanker_summary, aes(x = condition, y = meanRT, linetype = Congruency, color = Congruency))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               position = position_dodge(width = .15),
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = T,
               position = position_dodge(width = .15),
               aes(group = Congruency))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = T,
               position = position_dodge(width = .15),
               width = .1)+
  labs(title = "Flanker task response time")+
  xlab("Condition")+ #set X_axis name
  ylab("Response Time(ms)")+
  theme_classic()


flanker_plot_accuracy = ggplot(flanker_summary, aes(x = condition, y = meanAccuracy, linetype = Congruency))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               position = position_dodge(width = .15),
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = TRUE,
               position = position_dodge(width = .15),
               aes(group = Congruency))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = TRUE,
               position = position_dodge(width = .15),
               width = .15)+
  labs(title = "Flanker task accuracy")+
  xlab("Condition")+ #set X_axis name
  ylab("Accuracy %")+  #set y_axis name
  scale_linetype_discrete(name = "Congruency", #set legend name
                          labels = c("Incongruent", "Congruent")) +
  theme_classic()
  
flanker_plot_RT
flanker_plot_accuracy

#===============================
# Cueing Analysis ----
#===============================

cueing_analysis <- cueing %>%
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 100, NA, RT),
         RT = ifelse(RT >= 1000, NA, RT),
         RT = ifelse(RT > mean(RT, na.rm=TRUE)+2.5*sd(RT, na.rm = TRUE), NA, RT),
         RT = ifelse(RT < mean(RT, na.rm=TRUE)-2.5*sd(RT, na.rm = TRUE), NA, RT),
         accuracy = ifelse(status==1 | status==2, status, NA),
         accuracy = ifelse(accuracy==2, 0, accuracy))

cueing_summary <- cueing_analysis %>% 
  group_by(subn, ValidityNum, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(accuracy, na.rm=TRUE)*100) %>% 
  mutate(condition = as.factor(condition),
         ValidityNum = as.factor(ValidityNum))

cueing_summary$condition <- recode_factor(cueing_summary$condition, O = "Organized", C = "Cluttered")

cueing_summary$ValidityNum <- recode_factor(cueing_summary$ValidityNum, '0' = "Incongruent", '1' = "Congruent")

cueing_summary$Congruency <-  cueing_summary$ValidityNum

#ANOVA with DV response time and between group IV condition and within group IV ValidityNum
cueing_aov_RT_condition_congruency <- aov(meanRT ~ condition*ValidityNum + Error(subn/ValidityNum), data = cueing_summary)
summary(cueing_aov_RT_condition_congruency)







#cueing task bayesian analysis
# bayes_cueing_RT <- anovaBF(meanRT ~ condition*ValidityNum + subn, data = cueing_summary, whichRandom = "subn")
# bayes_cueing_RT
# 
# plot(bayes_cueing_RT)
# 
# 
# plot(mcmcOut2[,"mu"])










#ANOVA with DV accuracy and between group IV condition and within group IV ValidityNum
cueing_aov_accuracy_condition_congruency <- aov(meanAccuracy ~ condition*ValidityNum + Error(subn/ValidityNum), data = cueing_summary)
summary(cueing_aov_accuracy_condition_congruency)

#create a table showing means
model.tables(cueing_aov_RT_condition_congruency, "means")
model.tables(cueing_aov_accuracy_condition_congruency, "means")

#Quick graphs to check
Plot2WayANOVA(meanRT ~ condition*ValidityNum, dataframe = cueing_summary)
Plot2WayANOVA(meanAccuracy ~ condition*ValidityNum, dataframe = cueing_summary)

#creating my own line graph with ggplot2
cueing_plot_RT = ggplot(cueing_summary, aes(x = condition, y = meanRT, linetype = Congruency, color = Congruency))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = TRUE,
               aes(group = Congruency))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = TRUE,
               width = .1)+
  labs(title = "Cueing task response time")+
  xlab("Condition")+ #set X_axis name
  ylab("Response Time(ms)") +
  theme_classic()

cueing_plot_RT

cueing_plot_accuracy = ggplot(cueing_summary, aes(x = condition, y = meanAccuracy, linetype = ValidityNum))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = TRUE,
               aes(group = ValidityNum))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = TRUE,
               width = .1)+
  labs(title = "Cueing task accuracy")+
  xlab("Condition")+ #set X_axis name
  ylab("Accuracy %")+  #set y_axis name
  scale_linetype_discrete(name = "Congruency", #set legend name
                          labels = c("Incongruent", "Congruent")) +
  theme_classic()

cueing_plot_RT
cueing_plot_accuracy



#===============================
# N-back Analysis ----
#===============================

nback_analysis <- nback %>% 
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 150, NA, RT),
         #RT = ifelse(, NA, RT),
         RT = ifelse(block == 1, NA, RT),
         score = ifelse(block == 1, NA, score))

nback_summary <- nback_analysis %>% 
  group_by(subn, trialType, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(score, na.rm=TRUE)) %>% 
  mutate(condition = as.factor(condition),
         trialType = as.factor(trialType))

nback_summary$condition <- recode_factor(nback_summary$condition, O = "Organized", C = "Cluttered")

nback_summary$trialType <- recode_factor(nback_summary$trialType, '1' = "matching", '0' = "non-matching")

#ANOVA with DV response time and between group IV condition and within group IV congruency
nback_aov_RT_condition_trialType <- aov(meanRT ~ condition*trialType + Error(subn/trialType), data = nback_summary)
summary(nback_aov_RT_condition_trialType)

#ANOVA with DV accuracy and between group IV condition and within group IV congruency
nback_aov_accuracy_condition_trialType <- aov(meanAccuracy ~ condition*trialType + Error(subn/trialType), data = nback_summary)
summary(nback_aov_accuracy_condition_trialType)

#create a table showing means
model.tables(nback_aov_RT_condition_trialType, "means")
model.tables(nback_aov_accuracy_condition_trialType, "means")


Plot2WayANOVA(meanRT ~ condition*trialType, dataframe = nback_summary)
Plot2WayANOVA(meanAccuracy ~ condition*trialType, dataframe = nback_summary)

#correlion between RT and accuracy
cor.test(nback_summary$meanRT, nback_summary$meanAccuracy, method = "pearson")

nback_plot_RT = ggplot(nback_summary, aes(x = condition, y = meanRT, linetype = trialType))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = T,
               aes(group = trialType))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = T,
               width = .1)+
  labs(title = "Nback task response time")+
  xlab("Condition")+ #set X_axis name
  ylab("Response Time(ms)")+  #set y_axis name
  scale_linetype_discrete(name = "Trial type", #set legend name
                          labels = c("Matching", "Non-matching"))

nback_plot_accuracy = ggplot(nback_summary, aes(x = condition, y = meanAccuracy, linetype = trialType, color = trialType)) +
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = TRUE,
               aes(group = trialType))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals
               geom = "errorbar", #graph error bars
               na.rm = TRUE,
               width = .1)+
  labs(title = "N-back 2 back task accuracy")+
  xlab("Condition")+ #set X_axis name
  ylab("Accuracy %")+  #set y_axis name
  scale_color_discrete(name = "Trial type",
                      labels = c("Matching", "Non-matching")) +   
  scale_linetype_discrete(name = "Trial type", #set legend name
                          labels = c("Matching", "Non-matching"))+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  theme_classic()

nback_plot_RT
nback_plot_accuracy

#===============================
# Go, no-go Analysis ----
#===============================

gonogo_analysis <- gonogo %>% 
  mutate(subn = as.factor(subn),
         RT = ifelse(RT < 150, NA, RT),
         errorStatus = ifelse(errorStatus== 1, 0, 1))

gonogo_summary <- gonogo_analysis %>% 
  group_by(subn, type, condition) %>% 
  summarise(meanRT = mean(RT, na.rm = TRUE),
            meanAccuracy = mean(errorStatus, na.rm=TRUE)*100) %>% 
  mutate(condition = as.factor(condition),
         type = as.factor(type))

gonogo_summary$condition <- recode_factor(gonogo_summary$condition, O = "Organized", C = "Cluttered")

#ANOVA with DV response time and between group IV condition and within group IV congruency
gonogo_aov_RT_condition_type <- aov(meanRT ~ condition*type + Error(subn/type), data = gonogo_summary)
summary(gonogo_aov_RT_condition_type)

#ANOVA with DV accuracy and between group IV condition and within group IV congruency
gonogo_aov_accuracy_condition_type <- aov(meanAccuracy ~ condition*type + Error(subn/type), data = gonogo_summary)
summary(gonogo_aov_accuracy_condition_type)

#create a table showing means
model.tables(gonogo_aov_RT_condition_type, "means")
model.tables(gonogo_aov_accuracy_condition_type, "means")

gonogo_plot_RT = ggplot(gonogo_summary, aes(x = condition, y = meanRT, linetype = type))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               position = position_dodge(width = .15),
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = TRUE,
               position = position_dodge(width = .15),
               aes(group = type))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = TRUE,
               position = position_dodge(width = .15),
               width = .1)+
  labs(title = "Go, no-go task response time")+
  xlab("Condition")+ #set X_axis name
  ylab("Response Time(ms)")+  #set y_axis name
  scale_linetype_discrete(name = "Trial type", #set legend name
                          labels = c("Go", "No go"))

gonogo_plot_accuracy = ggplot(gonogo_summary, aes(x = condition, y = meanAccuracy, linetype = type))+ #IV2 groups differ by line type
  stat_summary(fun = mean,      #get means
               geom = "point",  #graph points
               position = position_dodge(width = .15),
               na.rm = TRUE)+      #NAs are removed (not taken into computation) if there's any
  stat_summary(fun = mean,
               geom = "line",   #graph lines
               na.rm = TRUE,
               position = position_dodge(width = .15),
               aes(group = type))+
  stat_summary(fun.data = mean_cl_normal,#get 95% Confidence Intervals; you can also set other kinds of error bars  
               geom = "errorbar", #graph error bars
               na.rm = TRUE,
               position = position_dodge(width = .15),
               width = .1)+
  labs(title = "Go, no-go task accuracy")+
  xlab("Condition")+ #set X_axis name
  ylab("Accuracy %")+  #set y_axis name
  scale_linetype_discrete(name = "Trial Type", #set legend name
                          labels = c("Go", "No go"))

gonogo_plot_RT
gonogo_plot_accuracy

