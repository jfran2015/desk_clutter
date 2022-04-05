library(tidyverse)
library(qualtRics)

#function that passes files from 
read <- function(data){
  full <- 0
  for (file in data) {
    #get condition of either organized or cluttered
    condition = str_sub(file,-8, -8)
    #get subject number
    subn = str_sub(file,-6, -5)
    #read in data
    individual <- read.table(file = file)
    #create new variables
    individual$condition <- condition
    individual$subn <- subn
    full <- rbind(full, individual)
  }
  fixed <- full[-c(1),]
  return(fixed)
}

#==========================
#flanker task
#==========================

#-----------importing data----------------------
flanker <- list.files(path = "data/flanker",
                      pattern = "*.txt", full.names = TRUE)

flanker <- read(flanker)
colnames(flanker)<-c("trial","text","Congruency","status","RT", "condition", "subn") #rename variables

write.csv(flanker, file = "data/flanker.csv")

#-----------------------------------------------------------
#status is coded as 1=correct, 2=error, 3=too slow
#congruency is coded as 1 = congruent, 2 = incongruent
#condition is coded as C = cluttered, O = Organized
#-----------------------------------------------------------

#==========================
#Cueing task
#==========================

#importing data
#participant numbers are off from the excel sheet. I'm unsure if that effect analysis
cueing <- list.files(path = "data/cueing", pattern = "*.txt", full.names = TRUE)

cueing <- read(cueing)
colnames(cueing)<-c("cueSide","targetSide","cueValidity","cued","ValidityNum", "RT", "status", "condition", "subn") #rename variables

write.csv(cueing, file = "data/cueing.csv")

#==========================
#N-back task
#==========================

#importing data

nback <- list.files(path = "data/n-back", pattern = "*.txt", full.names = TRUE)

nback <- read(nback)
colnames(nback)<-c("block","trial","trialType","score","match", "miss","falseAlarm","RT","Memory","currentLetter", "nback1","nback2","condition", "subn") #rename variables

write.csv(nback, file = "data/nback.csv")

#==========================
#Go, no-go task
#==========================

#importing data

gonogo <- list.files(path = "data/go-nogo", pattern = "*.txt", full.names = TRUE)

gonogo <- read(gonogo)
colnames(gonogo)<-c("type","RT","errorStatus","condition", "subn") #rename variables

write.csv(gonogo, file = "data/go-nogo.csv")

#==========================
#Qualtrics resonses
#==========================

#survey_response <- read_survey("data/qualtrics/WorkingEnvironments0324.csv")

#STAI <- survey_response %>% 
  #select(Q1, Q6:Q25)

#neuroticism <- survey_response %>% 
  #select(Q1, Q27:)




