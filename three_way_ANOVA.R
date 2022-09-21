# This file is intended to be run after the file_creation.R, analysis.R, and qualtrics_analysis files
source("file_creation.R")
source("analysis.R")
source("qualtrics_analysis.R")

#### Flanker task ####

flanker_summary_three_way <- left_join(flanker_summary, neuroticism2)

flanker_three_way_neuroticisim_RT <- aov(meanRT ~ condition*Congruency*group + Error(subn/Congruency+group ), data = flanker_summary_three_way)
summary(flanker_three_way_neuroticisim_RT)

#### Cueing task ####

cueing_summary_three_way <- left_join(cueing_summary, neuroticism2)

cueing_three_way_neuroticism_RT <- aov(meanRT ~ condition*ValidityNum*group + Error(subn/ValidityNum + group), data = cueing_summary_three_way)
summary(cueing_three_way_neuroticism_RT)

model.tables(cueing_three_way_neuroticism_RT, "means")

### Three way cueing task graph ###
cueing_plot_RT_three_way = ggplot(cueing_summary, aes(x = condition, y = meanRT, linetype = Congruency, color = Congruency))+ #IV2 groups differ by line type
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
  theme_classic() +
  facet_wrap(vars(group))

cueing_plot_RT_three_way



