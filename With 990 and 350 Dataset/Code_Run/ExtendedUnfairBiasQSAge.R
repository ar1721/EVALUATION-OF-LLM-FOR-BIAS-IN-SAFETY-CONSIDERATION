setwd("/home/al3170/Bayesian_Multilevel")
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("brms", type="binary")
# install.packages("lme4", type="binary")
# install.packages("lmerTest", type="binary")
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("ellipsis" ,type="binary")
#library(tidybayes)
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


#library(tidybayes)
library(brms) # for the analysis
library(haven) # to load the SPSS .sav file
# library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(loo)
# #########################Reading dices dataset#######################



dices1=read.csv("diverse_safety_adversarial_dialog_350.csv")
dices2=read.csv("diverse_safety_adversarial_dialog_990.csv")
colnm<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_bias_overall")


colnm2<-c("rater_id","rater_gender","rater_race","rater_race_raw","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_unfair_bias_overall")
colnm3<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_bias_overall")dices1<-dices1[colnm]
dices3<-dices2[colnm2]
colnames(dices3)<-colnm3
dices=rbind(dices1,dices3)

      
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
dices$Q3_bias_overall <- factor(dices$Q3_bias_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)


#########################Models###################################

formula1 <- Q3_bias_overall ~ rater_ethinicity * (rater_gender + rater_age+phase+ rater_education) + (1 | rater_id) + (1 | item_id)

formula2 <- Q3_bias_overall ~ rater_ethinicity * (rater_gender + rater_age+phase+ rater_education + degree_of_harm) + (1 | rater_id) + (1 | item_id)

formula3 <- Q3_bias_overall ~ rater_ethinicity * (rater_gender + rater_age+phase+ rater_education + degree_of_harm) + (degree_of_harm | rater_id) + (1 | item_id)

formula4<- Q3_bias_overall ~ rater_age * (rater_ethinicity +rater_gender +rater_education) + (1 | rater_id) + (1 | item_id)

formula5<-Q3_bias_overall ~ rater_age * (rater_ethinicity + rater_gender + degree_of_harm+rater_education) + (1 | rater_id) + (1 | item_id)

formula6 <- Q3_bias_overall ~ rater_age *(rater_ethinicity + rater_gender + degree_of_harm+rater_education) + (degree_of_harm | rater_id) + (1 | item_id)

formula7<- Q3_bias_overall ~ rater_education * (rater_ethinicity +phase+rater_gender +rater_age) + (1 | rater_id) + (1 | item_id)

formula8<-Q3_bias_overall ~ rater_education * (rater_ethinicity +phase+ rater_gender + degree_of_harm+rater_age) + (1 | rater_id) + (1 | item_id)

formula9 <- Q3_bias_overall ~ rater_education *(rater_ethinicity + phase+rater_gender + degree_of_harm+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)

formula10<-Q3_bias_overall ~ degree_of_harm * (rater_ethinicity + rater_gender + rater_education+rater_age) + (1 | rater_id) + (1 | item_id)

formula11 <- Q3_bias_overall ~ degree_of_harm *(rater_ethinicity + rater_gender + rater_education+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)

formula12<- Q3_bias_overall ~ rater_gender * (rater_ethinicity +phase+ rater_age+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula13<-Q3_bias_overall ~ rater_gender * (rater_ethinicity +phase+ rater_age + degree_of_harm+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula14<- Q3_bias_overall ~ rater_gender *(rater_ethinicity +phase+ rater_age + degree_of_harm+rater_education+phase) + (degree_of_harm | rater_id) + (1 | item_id)


prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,2.5), class="b")
)

# 
# ModelQUnfairBias.Intersectional.AD.Race. <- brm(
#   formula = formula1,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# save(ModelQUnfairBias.Intersectional.AD,file="ModelQUnfairBiasIntersectionalADRace.RData")

# 
# ModelQUnfairBias.Intersectional.QS.Race <- brm(
#   formula = formula2,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# save(ModeQHarmfulContent.Intersectional.QS,file="ModelQUnfairBiasIntersectionalQSRace.RData")
# 
# ModelQUnfairBias.Intersectional.QSGE.Race <- brm(
#   formula = formula3,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   init=0,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# save(ModelQUnfairBias.Intersectional.QSGE,file="ModelQUnfairBiasIntersectionalQSGERace.RData")
# 

# 
# 
# ModelQUnfairBias.Intersectional.AD.Age <- brm(
#   formula = formula4,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.AD.Age,file="ModelQUnfairBiasADAge.RData")
# # 
ModelQUnfairBias.Intersectional.QS.Age <- brm(
  formula = formula5,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,init=0,
  backend = 'rstan',
  cores =4
)

save(ModelQUnfairBias.Intersectional.QS.Age,file="ModelQUnfairBiasQSAge.RData")
# 
# ModelQUnfairBias.Intersectional.QSGE.Age <- brm(
#   formula = formula6,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QSGE.Age,file="ModelQUnfairBiasQSGEAge.RData")
# 
# 
# ModelQUnfairBias.Intersectional.AD.Education <- brm(
#   formula = formula7,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.AD.Education,file="ModelQUnfairBiasADEducation.RData")
# 
# ModelQUnfairBias.Intersectional.QS.Education <- brm(
#   formula = formula8,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QS.Education,file="ModelQUnfairBiasQSEducation.RData")
# 
# ModelQUnfairBias.Intersectional.QSGE.Education <- brm(
#   formula = formula9,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QSGE.Education,file="ModelQUnfairBiasQSGEEducation.RData")
# 
# 
# ModelQUnfairBias.Intersectional.QS.DegreeOfHarm <- brm(
#   formula = formula10,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QS.DegreeOfHarm,file="ModelQUnfairBiasQSDegreeOfHarm.RData")
# 
# ModelQUnfairBias.Intersectional.QSGE.DegreeOfHarm <- brm(
#   formula = formula11,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QSGE.DegreeOfHarm,file="ModelQUnfairBiasQSGEDegreeOfHarm.RData")
# 
# 
# ModelQUnfairBias.Intersectional.AD.Gender <- brm(
#   formula = formula12,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.AD.Gender,file="ModelQUnfairBiasADGender.RData")
# 
# ModelQUnfairBias.Intersectional.QS.Gender <- brm(
#   formula = formula13,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QS.Gender,file="ModelQUnfairBiasQSGender.RData")
# 
# ModelQUnfairBias.Intersectional.QSGE.Gender <- brm(
#   formula = formula14,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 123,
#   backend = 'rstan',
#   cores =4
# )
# 
# save(ModelQUnfairBias.Intersectional.QSGE.Gender,file="ModelQUnfairBiasQSGEGender.RData")
