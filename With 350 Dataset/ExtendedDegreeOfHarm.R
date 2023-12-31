dices1=read.csv('diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)


# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)

sumdices<-summary(dices)
###################Models###########################

# Degree of harm

formula17<-Q_overall ~ degree_of_harm * (rater_raw_race + rater_gender +rater_education+rater_age) + (1 | rater_id) + (1 | item_id)

formula18 <- Q_overall ~ degree_of_harm *(rater_raw_race + rater_gender + rater_education+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)


prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,2.5), class="b")
)


Model.intersectional.QS.DegreeOfHarm <- brm(
  formula = formula17,
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
Model.intersectional.QSGE.DegreeOfHarm <- brm(
  formula = formula18,
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

save(Model.intersectional.QS.DegreeOfHarm,file="ModelIntersectionalQSDegreeOfHarm.RData")
save(Model.intersectional.QSGE.DegreeOfHarm,file="ModelIntersectionalQSGEDegreeOfHarm.RData")


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



plottingBar <- function(gr1,l,k) { # create a function with the name my_function
  titlex=paste("Probability of \"No\" by degree_of_harm and ",k)
  titlex=paste(titlex,l)
  
  m=ggplot(gr1, aes(x = degree_of_harm, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
    labs(
      title = titlex ,
      x = "Degree Of Harm",
      y = "Probability Of No rating"
    )+scale_y_continuous(
      limits =   c(0,100))+
    theme_minimal()
  return (m)
}


# For Model.Intersection.QS
# Condition 5: rater_age
conditions5 <- expand.grid( rater_age = "gen x+", degree_of_harm = unique(dices$degree_of_harm), rater_education="College degree or higher", rater_gender = "Man", rater_raw_race = "White")

# Condition 6: rater_age, rater-race
conditions6 <- expand.grid(rater_age = "gen x+", degree_of_harm = unique(dices$degree_of_harm), rater_education="College degree or higher", rater_gender = "Man", rater_raw_race = unique(dices$rater_raw_race))

# Condition 7: rater_age, rater-gender
conditions7 <- expand.grid(rater_age = "gen x+", rater_raw_race = "White", rater_education="College degree or higher", rater_gender = unique(dices$rater_gender), degree_of_harm = unique(dices$degree_of_harm))

# Condition 8: rater_age, rater-age
conditions8 <- expand.grid(rater_age = "gen x+", degree_of_harm = unique(dices$degree_of_harm), rater_education = unique(dices$rater_education), rater_gender = "Man", rater_raw_race = "White")

# Condition 9: rater_age, degree of harm
conditions9 <- expand.grid(rater_age= unique(dices$rater_age), rater_education="College degree or higher", degree_of_harm = unique(dices$degree_of_harm), rater_gender = "Man", rater_raw_race = "White")

# For Model.Intersection.QSGE
# Condition 10: rater_age
conditions10 <- expand.grid(rater_age = "gen x+", degree_of_harm = unique(dices$degree_of_harm), rater_education="College degree or higher", rater_gender = "Man", rater_raw_race = "White")

# Condition 11: rater_age, rater-race
conditions11 <- expand.grid(rater_age = "gen x+", degree_of_harm = unique(dices$degree_of_harm), rater_education="College degree or higher", rater_gender = "Man", rater_raw_race = unique(dices$rater_raw_race))

# Condition 12: rater_age, rater-gender
conditions12 <- expand.grid(rater_age = "gen x+", rater_raw_race = "White", rater_education="College degree or higher", rater_gender = unique(dices$rater_gender), degree_of_harm = unique(dices$degree_of_harm))

# Condition 13: rater_age, rater-age
conditions13 <- expand.grid(rater_age = "gen x+", degree_of_harm = unique(dices$degree_of_harm), rater_education = unique(dices$rater_education), rater_gender = "Man", rater_raw_race = "White")

# Condition 14: rater_age, degree of harm
conditions14 <- expand.grid(rater_age= unique(dices$rater_age), rater_education="College degree or higher", degree_of_harm = unique(dices$degree_of_harm), rater_gender = "Man", rater_raw_race = "White")




######################################Model.Intersection.QS################
modelname=" QS Intersection"
# rater_education
conditions <- conditions5
mod_plot <- conditional_effects(Model.intersectional.QS.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_DegreeOfHarm_QS_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- conditions6
mod_plot <- conditional_effects(Model.intersectional.QS.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_raw_race"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_DegreeOfHarm_QS_degree_of_harm_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_education,rater_gender
conditions <- conditions7
mod_plot <- conditional_effects(Model.intersectional.QS.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_DegreeOfHarm_QS_degree_of_harm_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,rater-age
conditions <- conditions8
mod_plot <- conditional_effects(Model.intersectional.QS.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)
m=plot(mod_plot)[[1]] + facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_DegreeOfHarm_QS_degree_of_harm_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,degree of harm
conditions <- conditions9
mod_plot <- conditional_effects(Model.intersectional.QS.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)


ggsave(filename = "Overall_DegreeOfHarm_QS_rater_education_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


######################################Model.Intersection.QSGE################

modelname=" QSGE Intersection"
# rater_education
conditions <- conditions10
mod_plot <- conditional_effects(Model.intersectional.QSGE.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_DegreeOfHarm_QSGE_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- conditions11
mod_plot <- conditional_effects(Model.intersectional.QSGE.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_raw_race"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_DegreeOfHarm_QSGE_degree_of_harm_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_education,rater_gender
conditions <-conditions12
mod_plot <- conditional_effects(Model.intersectional.QSGE.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_DegreeOfHarm_QSGE_degree_of_harm_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,rater-age
conditions <- conditions13
mod_plot <- conditional_effects(Model.intersectional.QSGE.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_DegreeOfHarm_QSGE_degree_of_harm_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,degree of harm
conditions <- conditions14

mod_plot <- conditional_effects(Model.intersectional.QSGE.DegreeOfHarm,categorical = TRUE, effect ="degree_of_harm" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_DegreeOfHarm_QSGE_rater_education_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 
