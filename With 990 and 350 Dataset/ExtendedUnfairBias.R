# #########################Reading dices dataset#######################
dices1=read.csv("diverse_safety_adversarial_dialog_350.csv")
dices2=read.csv("diverse_safety_adversarial_dialog_990.csv")
colnm<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q_overall")
colnm2<-c("rater_id","rater_gender","rater_race","rater_race_raw","rater_age","phase","rater_education","item_id","degree_of_harm","Q_overall")
colnm3<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q_overall")

dices1<-dices1[colnm]
dices3<-dices2[colnm2]
colnames(dices3)<-colnm3
dices=rbind(dices1,dices3)

dices1=read.csv("diverse_safety_adversarial_dialog_350.csv")
dices2=read.csv("diverse_safety_adversarial_dialog_990.csv")
colnm<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q2_harmful_content_overall",
         "Q3_bias_overall", "Q4_misinformation","Q_overall")
colnm2<-c("rater_id","rater_gender","rater_race","rater_race_raw","rater_age","phase","rater_education","item_id","degree_of_harm","Q2_harmful_content_overall",
          "Q3_unfair_bias_overall", "Q4_misinformation_overall","Q_overall")
colnm3<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q2_harmful_content_overall",
          "Q3_bias_overall", "Q4_misinformation","Q_overall")

dices1<-dices1[colnm]
dices3<-dices2[colnm2]
colnames(dices3)<-colnm3


dices3<-dices3[!(is.na(dices$degree_of_harm) | dices3$degree_of_harm==""), ]

dices<-rbind(dices1,dices3) 

# ###################Turning Q_Overall rating to numeric from character################

dices<-dices %>% mutate(
  
  rater_ethinicity = case_when(
    
    
    rater_raw_race %in% c("White") ~ "White",
    
    rater_raw_race %in% c(
      
      "Asian", "East or South-East Asian") ~ "Asian",
    
    rater_raw_race %in% c("Black or African American") ~ "Black",
    
    rater_raw_race %in% c(
      
      "Indian",
      
      "Indian subcontinent (including Bangladesh, Bhutan, India, Maldives, Nepal, Pakistan, and Sri Lanka)"
      
    ) ~ "Indian Subcontinent",
    
    rater_raw_race %in% c(
      
      "American Indian or Alaska Native",
      
      "LatinX, Latino, Hispanic or Spanish Origin, American Indian or Alaska Native",
      
      "LatinX, Latino, Hispanic or Spanish Origin, Mexican Indigenous",
      
      "Native Hawaiian or other Pacific Islander",
      
      "White, American Indian or Alaska Native"
      
    ) ~ "Indigenous",
    
    rater_raw_race %in% c(
      
      "Latino, Hispanic or Spanish Origin",
      
      "LatinX, Latino, Hispanic or Spanish Origin") ~ "Latin(x)e",
    
    rater_raw_race %in% c(
      
      "Black or African American, East or South-East Asian",
      
      "LatinX, Latino, Hispanic or Spanish Origin, East or South-East Asian",
      
      "White, East or South-East Asian",
      
      "White, LatinX, Latino, Hispanic or Spanish Origin",
      
      "Mixed") ~ "Multiracial",
    
    rater_raw_race %in% c(
      
      "Middle Eastern or North African",
      
      "Other",
      
      "Prefer not to answer",
      
      ""
    ) ~ "Other"
    
  )
  
) %>%
  
  # make white people the reference group
  
  mutate(rater_ethinicity = relevel(factor(rater_ethinicity), "White"))
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)


#########################Models###################################

formula1 <- Q_overall ~ rater_raw_race * (rater_gender + rater_age+phase+ rater_education) + (1 | rater_id) + (1 | item_id)

formula2 <- Q_overall ~ rater_raw_race * (rater_gender + rater_age+phase+ rater_education + degree_of_harm) + (1 | rater_id) + (1 | item_id)

formula3 <- Q_overall ~ rater_raw_race * (rater_gender + rater_age+phase+ rater_education + degree_of_harm) + (degree_of_harm | rater_id) + (1 | item_id)

formula4<- Q_overall ~ rater_age * (rater_raw_race +rater_gender +rater_education) + (1 | rater_id) + (1 | item_id)

formula5<-Q_overall ~ rater_age * (rater_raw_race + rater_gender + degree_of_harm+rater_education) + (1 | rater_id) + (1 | item_id)

formula6 <- Q_overall ~ rater_age *(rater_raw_race + rater_gender + degree_of_harm+rater_education) + (degree_of_harm | rater_id) + (1 | item_id)

formula7<- Q_overall ~ rater_education * (rater_raw_race +phase+rater_gender +rater_age) + (1 | rater_id) + (1 | item_id)

formula8<-Q_overall ~ rater_education * (rater_raw_race +phase+ rater_gender + degree_of_harm+rater_age) + (1 | rater_id) + (1 | item_id)

formula9 <- Q_overall ~ rater_education *(rater_raw_race + phase+rater_gender + degree_of_harm+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)

formula10<-Q_overall ~ degree_of_harm * (rater_raw_race + rater_gender + rater_education+rater_age) + (1 | rater_id) + (1 | item_id)

formula11 <- Q_overall ~ degree_of_harm *(rater_raw_race + rater_gender + rater_education+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)

formula12<- Q_overall ~ rater_gender * (rater_raw_race +phase+ rater_age+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula13<-Q_overall ~ rater_gender * (rater_raw_race +phase+ rater_age + degree_of_harm+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula14<- Q_overall ~ rater_gender *(rater_raw_race +phase+ rater_age + degree_of_harm+rater_education+phase) + (degree_of_harm | rater_id) + (1 | item_id)

ModelQHarmfulContent.Intersectional.AD.Race. <- brm(
  formula = formula1,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)
save(ModelQHarmfulContent.Intersectional.AD,file="ModelQHarmfulContentIntersectionalADRace.RData")


ModelQHarmfulContent.Intersectional.QS.Race <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)
save(ModeQHarmfulContent.Intersectional.QS,file="ModelQHarmfulContentIntersectionalQSRace.RData")


ModelQHarmfulContent.Intersectional.QSGE.Race <- brm(
  formula = formula3,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)
save(ModelQHarmfulContent.Intersectional.QSGE,file="ModelQHarmfulContentIntersectionalQSGERace.RData")




ModelQHarmfulContent.Intersectional.AD.Age <- brm(
  formula = formula4,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.AD.Age,file="ModelQHarmfulContentADAge.RData")

ModelQHarmfulContent.Intersectional.QS.Age <- brm(
  formula = formula5,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QS.Age,file="ModelQHarmfulContentQSAge.RData")

ModelQHarmfulContent.Intersectional.QSGE.Age <- brm(
  formula = formula6,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QSGE.Age,file="ModelQHarmfulContentQSGEAge.RData")


ModelQHarmfulContent.Intersectional.AD.Education <- brm(
  formula = formula7,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.AD.Education,file="ModelQHarmfulContentADEducation.RData")

ModelQHarmfulContent.Intersectional.QS.Education <- brm(
  formula = formula8,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QS.Education,file="ModelQHarmfulContentQSEducation.RData")

ModelQHarmfulContent.Intersectional.QSGE.Education <- brm(
  formula = formula9,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QSGE.Education,file="ModelQHarmfulContentQSGEEducation.RData")


ModelQHarmfulContent.Intersectional.QS.DegreeOfHarm <- brm(
  formula = formula10,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QS.DegreeOfHarm,file="ModelQHarmfulContentQSDegreeOfHarm.RData")

ModelQHarmfulContent.Intersectional.QSGE.DegreeOfHarm <- brm(
  formula = formula11,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QSGE.DegreeOfHarm,file="ModelQHarmfulContentQSGEDegreeOfHarm.RData")


ModelQHarmfulContent.Intersectional.AD.Gender <- brm(
  formula = formula12,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.AD.Gender,file="ModelQHarmfulContentADGender.RData")

ModelQHarmfulContent.Intersectional.QS.Gender <- brm(
  formula = formula13,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QS.Gender,file="ModelQHarmfulContentQSGender.RData")

ModelQHarmfulContent.Intersectional.QSGE.Gender <- brm(
  formula = formula14,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 123,
  backend = 'rstan',
  cores = 8
)

save(ModelQHarmfulContent.Intersectional.QSGE.Gender,file="ModelQHarmfulContentQSGEGender.RData")
