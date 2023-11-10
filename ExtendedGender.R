dices1=read.csv('https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)


# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)

sumdices<-summary(dices)
###################Models###########################
# Gender
formula8<- Q_overall ~ rater_gender * (rater_raw_race + rater_age+rater_education) + (1 | rater_id) + (1 | item_id)

formula9<-Q_overall ~ rater_gender * (rater_raw_race + rater_age + degree_of_harm+rater_education) + (1 | rater_id) + (1 | item_id)

formula10 <- Q_overall ~ rater_gender *(rater_raw_race + rater_age + degree_of_harm+rater_education) + (degree_of_harm | rater_id) + (1 | item_id)

prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,1), class="b")
)

Model.intersectional.AD.Gender <- brm(
  formula = formula8,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)

save(Model.intersectional.AD.Gender,file="ModelIntersectionalADGender.RData")

Model.intersectional.QS.Gender <- brm(
  formula = formula9,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 6
)

save(Model.intersectional.QS.Gender,file="ModelIntersectionalQSGender.RData")

Model.intersectional.QSGE.Gender <- brm(
  formula = formula10,
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

save(Model.intersectional.QSGE.Gender,file="ModelIntersectionalQSGEGender.RData")


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


plottingBar <- function(gr1,l,k) { # create a function with the name my_function
  titlex=paste("Probability of \"No\" by rater_gender and ",k)
  titlex=paste(titlex,l)
  
  m=ggplot(gr1, aes(x = rater_gender, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
    labs(
      title = titlex ,
      x = "Rater Gender",
      y = "Probability Of No rating"
    )+scale_y_continuous(
      limits =   c(0,100))+
    theme_minimal()
  return (m)
}
######################################Model.Intersection.AD################
modelname=" AD Intersection"
# rater_gender
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_raw_race"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 






######################################Model.Intersection.QS################
modelname=" QS Intersection"
# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "QS_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_raw_race"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "QS_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "QS_rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
m=plot(mod_plot)[[1]] + facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "QS_rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)


ggsave(filename = "QS_rater_gender_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


######################################Model.Intersection.QSGE################

modelname=" QSGE Intersection"
# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "QSGE_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_raw_race"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "QSGE_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "QSGE_rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "QSGE_rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

ud1=length(unique(dices$rater_gender)) 
 ud2=length(unique(dices[[k]])) 
 selected_rows_indices <- c() 
 i=1 
 for (lo in range(ud1)){ 
 hel=c(seq(i, nrow(gr1), by = ud1*ud1)) 
 selected_rows_indices <- c(selected_rows_indices, hel) 
 i=i+1 
 }
gr2=gr1[selected_rows_indices,]
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "QSGE_rater_gender_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 
