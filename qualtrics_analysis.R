# This file is for the qualtrics survey data

library(psych)

#######################################
####  State trait anxiety inventory  ####
#######################################

STAI$Q1 <- as.numeric(STAI$Q1)

STAI <- STAI %>% 
  filter(!is.na(Q1))

STAI2 <- data.frame(matrix(ncol = ncol(STAI), nrow = nrow(STAI)))

colnames(STAI2) <- colnames(STAI)

STAI2 <- STAI2 %>% 
  mutate_all(as.numeric)


# This recodes the responses to numbers which are easier to work with
for (col in 1:ncol(STAI)) {
  for (row in 1:nrow(STAI)) {
    if (STAI[row, col] == "Not at all") {
      STAI2[row, col] = 1
    } else if (STAI[row, col] == "Somewhat"){
      STAI2[row, col] = 2
    } else if (STAI[row, col] == "Moderately so"){
      STAI2[row, col] = 3
    } else if (STAI[row, col] == "Very much so"){
      STAI2[row, col] = 4
    } else {
      #This particular line makes it so that the participant numbers still get passed on
      STAI2[row, col] = STAI[row, col]
    }
      }
}



###########################
####  Neuroticism Scale  ####
##########################

neuroticism$Q1 <- as.numeric(neuroticism$Q1)

neuroticism <- neuroticism %>% 
  filter(!is.na(Q1))

neuroticism2 <- data.frame(matrix(ncol = ncol(neuroticism), nrow = nrow(neuroticism)))

colnames(neuroticism2) <- colnames(neuroticism)

neuroticism2 <- neuroticism2 %>% 
  mutate_all(as.numeric)

for (col in 1:ncol(neuroticism)) {
  for (row in 1:nrow(neuroticism)) {
    if (neuroticism[row, col] == "disagree") {
      neuroticism2[row, col] = 1
    } else if (neuroticism[row, col] == "slightly disagree"){
      neuroticism2[row, col] = 2
    } else if (neuroticism[row, col] == "neutral"){
      neuroticism2[row, col] = 3
    } else if (neuroticism[row, col] == "slightly agree"){
      neuroticism2[row, col] = 4
    } else if (neuroticism[row, col] == "agree"){
      neuroticism2[row, col] = 4
    }else {
      #This particular line makes it so that the participant numbers still get passed on
      neuroticism2[row, col] = STAI[row, col]
    }
  }
}

neuroticism2 <- neuroticism2 %>% 
  mutate(neuro_score = 38 - Q28 + Q29 - Q30 + Q31 - Q32 - Q33 - Q34 - Q35 - Q36 - Q37,
         group = ifelse(neuro_score > median(neuro_score), "high", "low"),
         subn = as.factor(Q1))

describe(neuroticism2)
