# setwd("/home/al3170/Bayesian_Multilevel")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("brms", type="binary")
# install.packages("lme4", type="binary")
# install.packages("lmerTest", type="binary")
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("ellipsis" ,type="binary")
# install.packages("tidybayes")
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


library(tidybayes)
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


# dices3<-dices3[!(is.na(dices$degree_of_harm) | dices3$degree_of_harm==""), ]

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
###################Models###########################
# Gender
formula8<- Q_overall ~ rater_gender * (rater_raw_race +phase+ rater_age+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula9<-Q_overall ~ rater_gender * (rater_raw_race +phase+ rater_age + degree_of_harm+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula10 <- Q_overall ~ rater_gender *(rater_raw_race +phase+ rater_age + degree_of_harm+rater_education+phase) + (degree_of_harm | rater_id) + (1 | item_id)

prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,1), class="b")
)

# Model.intersectional.AD.Gender <- brm(
#   formula = formula8,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 42,init=0,
#   backend = 'rstan',
#   cores = 4
# )
# 
# save(Model.intersectional.AD.Gender,file="ModelIntersectionalADGender.RData")

Model.intersectional.QS.Gender <- brm(
  formula = formula9,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,init=0,
  backend = 'rstan',
  cores = 4
)

save(Model.intersectional.QS.Gender,file="ModelIntersectionalQSGender.RData")
# 
# Model.intersectional.QSGE.Gender <- brm(
#   formula = formula10,
#   data = dices,
#   family = cumulative("probit"),
#   prior = prior_thresholds,
#   warmup = 1000,
#   iter = 4000,
#   chains = 4,
#   seed = 123,
#   backend = 'rstan',
#   cores = 4
# )
# 
# save(Model.intersectional.QSGE.Gender,file="ModelIntersectionalQSGEGender.RData")


##################################Fitness########################
