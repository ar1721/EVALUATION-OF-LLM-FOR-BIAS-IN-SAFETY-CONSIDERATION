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
colnm<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_bias_overall")
colnm2<-c("rater_id","rater_gender","rater_race","rater_race_raw","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_bias_overall")
colnm3<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_bias_overall")

dices1<-dices1[colnm]
dices3<-dices2[colnm2]
colnames(dices3)<-colnm3
dices=rbind(dices1,dices3)

dices1=read.csv("diverse_safety_adversarial_dialog_350.csv")
dices2=read.csv("diverse_safety_adversarial_dialog_990.csv")
colnm<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm",
         "Q3_bias_overall")
colnm2<-c("rater_id","rater_gender","rater_race","rater_race_raw","rater_age","phase","rater_education","item_id","degree_of_harm",
          "Q3_unfair_bias_overall")
colnm3<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q3_bias_overall",
          )

dices1<-dices1[colnm]
dices3<-dices2[colnm2]
colnames(dices3)<-colnm3


# dices3<-dices3[!(is.na(dices$degree_of_harm) | dices3$degree_of_harm==""), ]

dices<-rbind(dices1,dices3) 

# ###################Turning Q3_bias_overall rating to numeric from character################

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


ModelQUnfairBias.Intersectional.AD.Race. <- brm(
  formula = formula1,
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
save(ModelQUnfairBias.Intersectional.AD,file="ModelQUnfairBiasIntersectionalADRace.RData")


ModelQUnfairBias.Intersectional.QS.Race <- brm(
  formula = formula2,
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
save(ModeQHarmfulContent.Intersectional.QS,file="ModelQUnfairBiasIntersectionalQSRace.RData")

ModelQUnfairBias.Intersectional.QSGE.Race <- brm(
  formula = formula3,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  init=0,
  seed = 42,init=0,
  backend = 'rstan',
  cores =4
)
save(ModelQUnfairBias.Intersectional.QSGE,file="ModelQUnfairBiasIntersectionalQSGERace.RData")




ModelQUnfairBias.Intersectional.AD.Age <- brm(
  formula = formula4,
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

save(ModelQUnfairBias.Intersectional.AD.Age,file="ModelQUnfairBiasADAge.RData")

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

ModelQUnfairBias.Intersectional.QSGE.Age <- brm(
  formula = formula6,
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

save(ModelQUnfairBias.Intersectional.QSGE.Age,file="ModelQUnfairBiasQSGEAge.RData")


ModelQUnfairBias.Intersectional.AD.Education <- brm(
  formula = formula7,
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

save(ModelQUnfairBias.Intersectional.AD.Education,file="ModelQUnfairBiasADEducation.RData")

ModelQUnfairBias.Intersectional.QS.Education <- brm(
  formula = formula8,
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

save(ModelQUnfairBias.Intersectional.QS.Education,file="ModelQUnfairBiasQSEducation.RData")

ModelQUnfairBias.Intersectional.QSGE.Education <- brm(
  formula = formula9,
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

save(ModelQUnfairBias.Intersectional.QSGE.Education,file="ModelQUnfairBiasQSGEEducation.RData")


ModelQUnfairBias.Intersectional.QS.DegreeOfHarm <- brm(
  formula = formula10,
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

save(ModelQUnfairBias.Intersectional.QS.DegreeOfHarm,file="ModelQUnfairBiasQSDegreeOfHarm.RData")

ModelQUnfairBias.Intersectional.QSGE.DegreeOfHarm <- brm(
  formula = formula11,
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

save(ModelQUnfairBias.Intersectional.QSGE.DegreeOfHarm,file="ModelQUnfairBiasQSGEDegreeOfHarm.RData")


ModelQUnfairBias.Intersectional.AD.Gender <- brm(
  formula = formula12,
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

save(ModelQUnfairBias.Intersectional.AD.Gender,file="ModelQUnfairBiasADGender.RData")

ModelQUnfairBias.Intersectional.QS.Gender <- brm(
  formula = formula13,
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

save(ModelQUnfairBias.Intersectional.QS.Gender,file="ModelQUnfairBiasQSGender.RData")

ModelQUnfairBias.Intersectional.QSGE.Gender <- brm(
  formula = formula14,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 123,
  backend = 'rstan',
  cores =4
)

save(ModelQUnfairBias.Intersectional.QSGE.Gender,file="ModelQUnfairBiasQSGEGender.RData")



##################################Fitness########################

loo_results <- list(
  Intersectional_AD = loo(Model.intersectional.AD.Gender),
  Intersectional_QS = loo(Model.intersectional.QS.Gender),
  Intersectional_QSGE = loo(Model.intersectional.QSGE.Gender)
)

waic_results <- list( 
  Intersectional_AD = waic(Model.intersectional.AD.Gender),
  Intersectional_QS = waic(Model.intersectional.QS.Gender),
  Intersectional_QSGE = waic(Model.intersectional.QSGE.Gender) )



r2_results<-list( 
  Modelraw_Intersectional_AD_r2=performance::r2(Model.intersectional.AD.Gender),
  Modelraw_Intersectional_QS_r2=performance::r2(Model.intersectional.QS.Gender),
  Modelraw_Intersectional_QSGE_r2=performance::r2(Model.intersectional.QSGE.Gender))

save(loo_results,file="LooModelReplication.RData")
save(waic_results,file="WAICModelReplication.RData")
save(r2_results,file="R2ModelReplication.RData")


loo_results_estimate <- list(
  Intersectional_AD_estimate = loo_results$Intersectional_AD$estimates,
  Intersectional_QS_estimate = loo_results$Intersectional_QS$estimates,
  Intersectional_QSGE_estimate = loo_results$Intersectional_QSGE$estimates
)

waic_results_estimate <- list( 
  Intersectional_AD_estimate = waic_results$Intersectional_AD$estimates,
  Intersectional_QS_estimate = waic_results$Intersectional_QS$estimates,
  Intersectional_QSGE_estimate = waic_results$Intersectional_QSGE$estimates)


# Define the parameter of interest (e.g., rater_age)
parameter_df<-data_frame(Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(Model.intersectional.AD.Gender)
colPosteriorall<-colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior){
  print(i)
  # Calculate the median of the posterior distribution
  median_estimate <- median(parameter_samples[[i]])
  
  # Calculate the 95% Bayesian credible interval
  credible_interval <- quantile(parameter_samples[[i]], c(0.025, 0.975))
  
  # Calculate the probability of direction (96% chance of being positive)
  probability_direction <- mean(parameter_samples[[i]] > 0)
  
  # Calculate the probability of practical significance (95% chance of being > 0.05)
  probability_practical_significance <- mean(parameter_samples[[i]] > 0.05)
  
  # Calculate the probability of having a large effect (89% chance of being > 0.30)
  probability_large_effect <- mean(parameter_samples[[i]] > 0.30)
  
  parameter_df<-rbind(parameter_df,c(median_estimate,credible_interval,probability_direction,probability_practical_significance,probability_large_effect))
  
}
parameter_df<-parameter_df[-1,]
# Print the results


parameter_dfQS<-data_frame(Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(Model.intersectional.QSGE.Gender)
colPosteriorall<-colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior){
  print(i)
  # Calculate the median of the posterior distribution
  median_estimate <- median(parameter_samples[[i]])
  
  # Calculate the 95% Bayesian credible interval
  credible_interval <- quantile(parameter_samples[[i]], c(0.025, 0.975))
  
  # Calculate the probability of direction (96% chance of being positive)
  probability_direction <- mean(parameter_samples[[i]] > 0)
  
  # Calculate the probability of practical significance (95% chance of being > 0.05)
  probability_practical_significance <- mean(parameter_samples[[i]] > 0.05)
  
  # Calculate the probability of having a large effect (89% chance of being > 0.30)
  probability_large_effect <- mean(parameter_samples[[i]] > 0.30)
  
  parameter_dfQS<-rbind(parameter_df,c(median_estimate,credible_interval,probability_direction,probability_practical_significance,probability_large_effect))
  
}
parameter_dfQS<-parameter_df[-1,]


##################################Plots##########################
# plot(mod_plot, plot = FALSE)[[1]] +facet_wrap("rater_raw_race")


plottingBar <- function(m,l,k) { # create a function with the name my_function
  titlex=paste("Probability of No by rater_gender and ",k)
  titlex=paste(titlex,l)
  gr=m$data
  gr1=gr[gr$effect2__=="No",]
  nrc=length(unique(dices$rater_raw_race))*length(unique(dices[[k]]))
  gr1=gr[1:nrc,]
  m=ggplot(gr1, aes(x = rater_raw_race, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
    labs(
      title = titlex ,
      x = "Rater Race",
      y = "Probability Of No rating"
    )+scale_y_continuous(
      limits =   c(0,100))+
    theme_minimal()
  return (m)
}
######################################Model.Intersection.AD################
modelname="AD Intersection"
# rater_gender
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")
ggsave(filename = "rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")
ggsave(filename = "rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")

ggsave(filename = "rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 






######################################Model.Intersection.QS################
modelname="QS Intersection"
# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")

ggsave(filename = "QS_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "QS_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")

ggsave(filename = "QS_rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
m=plot(mod_plot)[[1]] + facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")
ggsave(filename = "QS_rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,modelname,"degree_of_harm")


ggsave(filename = "QS_rater_gender_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


######################################Model.Intersection.QSGE################

modelname="QSGE Intersection"
# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")

ggsave(filename = "QSGE_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "QSGE_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")
ggsave(filename = "QSGE_rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")
ggsave(filename = "QSGE_rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,modelname,"degree_of_harm")
ggsave(filename = "QSGE_rater_gender_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 
