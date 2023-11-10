dices1=read.csv('https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)


# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)

sumdices<-summary(dices)
###################Models###########################

# Age
formula11<- Q_overall ~ rater_age * (rater_raw_race +phase+rater_gender +rater_education) + (1 | rater_id) + (1 | item_id)

formula12<-Q_overall ~ rater_age * (rater_raw_race +phase+ rater_gender + degree_of_harm+rater_education) + (1 | rater_id) + (1 | item_id)

formula13 <- Q_overall ~ rater_age *(rater_raw_race +phase+ rater_gender + degree_of_harm+rater_education) + (degree_of_harm | rater_id) + (1 | item_id)


Model.intersectional.AD.Age <- brm(
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
Model.intersectional.QS.Age <- brm(
  formula = formula12,
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
Model.intersectional.QSGE.Age <- brm(
  formula = formula13,
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
save(Model.intersectional.AD.Age,file="ModelIntersectionalADAge.RData")
save(Model.intersectional.QS.Age,file="ModelIntersectionalQSAge.RData")
save(Model.intersectional.QSGE.Age,file="ModelIntersectionalQSGEAge.RData")

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
parameter_samples <- posterior_samples(Modelraw.Intersectional.AD)
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
parameter_samples <- posterior_samples(Modelraw.Intersectional.QSGE)
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
# rater_race
modelname="AD Intersectional"
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender ="Man",rater_raw_race=unique(dices$rater_raw_race))
mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "rater_race.jpeg", plot = m, width =24, height = 12) 


# rater_race,rater-race
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender =unique(dices$rater_gender))

mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")
ggsave(filename = "rater_gender_and_rater_raw_race.jpeg", plot = m, width =24, height = 12) 




# rater_raw_race,rater-education
conditions <- expand.grid(rater_gender ="Man",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")
ggsave(filename = "rater_race_and_rater_education.jpeg", plot = m, width =24, height = 12) 


# rater_race,rater-age
conditions <- expand.grid(rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")

ggsave(filename = "rater_race_and_rater_age.jpeg", plot = m, width =24, height = 12) 

######################################Model.Intersection.QS################

# rater_race
modelname="QS Intersection"
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")
mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "QS_rater_race.jpeg", plot = m, width =24, height = 12) 


# rater_race,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")
ggsave(filename = "QS_rater_gender_and_rater_raw_race.jpeg", plot = m, width =24, height = 12) 




# rater_race,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_gender ="Man",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")

ggsave(filename = "QS_rater_race_and_rater_education.jpeg", plot = m, width =24, height = 12) 


# rater_race,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] + facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")
ggsave(filename = "QS_rater_race_and_rater_age.jpeg", plot = m, width =24, height = 12) 


# rater_race,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,modelname,"degree_of_harm")


ggsave(filename = "QS_rater_race_and_degree_of_harm.jpeg", plot = m, width =24, height = 12) 

######################################Model.Intersection.QSGE################

# rater_race
modelname="QSGE Intersection"
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")
mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")

ggsave(filename = "QSGE_rater_gender.jpeg", plot = m, width =24, height = 12) 


# rater_race,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender=unique(dices$rater_gender))

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")

ggsave(filename = "QSGE_rater_gender_and_rater_raw_race.jpeg", plot = m, width =24, height = 12) 




# rater_race,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_gender ="Man",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")

ggsave(filename = "QSGE_rater_race_and_rater_education.jpeg", plot = m, width =24, height = 12) 


# rater_race,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")
ggsave(filename = "QSGE_rater_race_and_rater_age.jpeg", plot = m, width =24, height = 12) 


# rater_race,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,modelname,"degree_of_harm")
ggsave(filename = "QSGE_rater_race_and_degree_of_harm.jpeg", plot = m, width =24, height = 12) 
