install.packages("haven")
install.packages("tidyverse")
install.packages("brms", type="binary")
install.packages("lme4", type="binary")
install.packages("lmerTest", type="binary")
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("ellipsis" ,type="binary")
install.packages("tidybayes")
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("bayestestR")
library(cmdstanr)

library(tidybayes)

library(bayestestR)
library(dplyr)
library(brms) # for the analysis
library(haven) # to load the SPSS .sav file
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(loo)

# #########################Reading dices dataset#######################
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dices1=read.csv("diverse_safety_adversarial_dialog_350.csv")
dices=dices1



# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)


###################Modelraws###########################

# Create formula objects
formula1 <- Q_overall ~ 1 + (1 | rater_id) + (1 | item_id)

formula2 <- Q_overall ~ rater_raw_race + rater_gender + rater_age+ rater_education+ (1 | rater_id) + (1 | item_id)

formula3 <- Q_overall ~ rater_raw_race + rater_gender + rater_age+ rater_education + degree_of_harm + (1 | rater_id) + (1 | item_id)

formula4 <- Q_overall ~ rater_raw_race * (rater_gender + rater_age+ rater_education) + (1 | rater_id) + (1 | item_id)

formula5 <- Q_overall ~ rater_raw_race * (rater_gender + rater_age+ rater_education + degree_of_harm) + (1 | rater_id) + (1 | item_id)

formula6 <- Q_overall ~ rater_raw_race * (rater_gender + rater_age+ rater_education + degree_of_harm) + (degree_of_harm | rater_id) + (1 | item_id)

formula7 <- Q_overall ~ rater_raw_race +rater_gender + rater_age+ rater_education + degree_of_harm + (degree_of_harm | rater_id) + (1 | item_id)

# Get prior specifications for the models
prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,2.5), class="b")
)

priors1 <- get_prior(formula1, data = dices, family = cumulative("probit"))

# Create brm models
Model.null <- brm(
  formula = formula1,
  data = dices,
  family = cumulative("probit"),
  prior = priors1,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 8
)
save(Model.null,file="Modelrawnull.RData")

Modelraw.linear.AD <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  threads = threading(8),
  cores = 8
)
save(Modelraw.linear.AD,file="ModelrawlinearAD.RData")

Modelraw.linear.QS <- brm(
  formula = formula3,
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
save(Modelraw.linear.QS,file="ModelrawlinearQS.RData")


Modelraw.linear.QSGE <- brm(
  formula = formula7,
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
save(Modelraw.linear.QSGE,file="ModelrawlinearQSGE.RData")

Modelraw.Intersectional.AD <- brm(
  formula = formula4,
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
save(Modelraw.Intersectional.AD,file="ModelrawIntersectionalAD.RData")


Modelraw.Intersectional.QS <- brm(
  formula = formula5,
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
save(Modelraw.Intersectional.QS,file="ModelrawIntersectionalQS.RData")


Modelraw.Intersectional.QSGE <- brm(
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
save(Modelraw.Intersectional.QSGE,file="ModelrawIntersectionalQSGE.RData")


########################Metrics and Transformation#####################

model_summaries <- list()

# Store the summary of each model with "summary" added to the variable name
model_summaries$summary_Model.null <- summary(Model.null)
model_summaries$summary_Modelraw.linear.AD <- summary(Modelraw.linear.AD)
model_summaries$summary_Modelraw.linear.QS <- summary(Modelraw.linear.QS)
model_summaries$summary_Modelraw.linear.QSGE <- summary(Modelraw.linear.QSGE)
model_summaries$summary_Modelraw.Intersectional.AD <- summary(Modelraw.Intersectional.AD)
model_summaries$summary_Modelraw.Intersectional.QS <- summary(Modelraw.Intersectional.QS)
model_summaries$summary_Modelraw.Intersectional.QSGE <- summary(Modelraw.Intersectional.QSGE)


loo_results <- list(
  Linear_AD = loo(Modelraw.linear.AD),
  Linear_QS = loo(Modelraw.linear.QS),
  Linear_QSGE = loo(Modelraw.linear.QSGE),
  Intersectional_AD = loo(Modelraw.Intersectional.AD),
  Intersectional_QS = loo(Modelraw.Intersectional.QS),
  Intersectional_QSGE = loo(Modelraw.Intersectional.QSGE)
)

waic_results <- list( 
                      Linear_AD = waic(Modelraw.linear.AD),
                      Linear_QS = waic(Modelraw.linear.QS),
                      Linear_QSGE = waic(Modelraw.linear.QSGE),
                      Intersectional_AD = waic(Modelraw.Intersectional.AD),
                      Intersectional_QS = waic(Modelraw.Intersectional.QS),
                      Intersectional_QSGE = waic(Modelraw.Intersectional.QSGE) )



r2_results<-list(
                  Modelraw_linear_AD_r2=performance::r2(Modelraw.linear.AD),
                  Modelraw_linear_QS_r2=performance::r2(Modelraw.linear.QS),
                  Modelraw_linear_QSGE_r2=performance::r2(Modelraw.linear.QSGE),
                  Modelraw_Intersectional_AD_r2=performance::r2(Modelraw.Intersectional.AD),
                  Modelraw_Intersectional_QS_r2=performance::r2(Modelraw.Intersectional.QS),
                  Modelraw_Intersectional_QSGE_r2=performance::r2(Modelraw.Intersectional.QSGE))
save(loo_results,file="LooModelReplication.RData")
save(waic_results,file="WAICModelReplication.RData")
save(r2_results,file="R2ModelReplication.RData")


Model.nullTransformed<-ggs(Model.null)
Model.null.fixed_effects <- fixef(Model.null)
Model.null.random_effects <- ranef(Model.null)

Modelraw.Intersectional.ADTransformed<-ggs(Modelraw.Intersectional.AD)
Modelraw.linear.AD.fixed_effects <- fixef(Modelraw.linear.AD)
Modelraw.linear.AD.random_effects <- ranef(Modelraw.linear.AD)

Modelraw.linear.QS.fixed_effects <- fixef(Modelraw.linear.QS)
Modelraw.linear.QS.random_effects <- ranef(Modelraw.linear.QS)

Modelraw.linear.QSGE.fixed_effects <- fixef(Modelraw.linear.QSGE)
Modelraw.linear.QSGE.random_effects <- ranef(Modelraw.linear.QSGE)

Modelraw.Intersectional.AD.fixed_effects <- fixef(Modelraw.Intersectional.AD)
Modelraw.Intersectional.AD.random_effects <- ranef(Modelraw.Intersectional.AD)

Modelraw.Intersectional.QS.fixed_effects <- fixef(Modelraw.Intersectional.QS)
Modelraw.Intersectional.QS.random_effects <- ranef(Modelraw.Intersectional.QS)

Modelraw.Intersectional.QSGE.fixed_effects <- fixef(Modelraw.Intersectional.QSGE)
Modelraw.Intersectional.QSGE.random_effects <- ranef(Modelraw.Intersectional.QSGE)

elpd<-list(loo_results$Null_Modelraw$elpd_loo,
           loo_results$Linear_AD$elpd_loo,
           loo_results$Intersectional_AD$elpd_loo,
           loo_results$Linear_QS$elpd_loo,
           loo_results$Linear_QSGE$elpd_loo,
           loo_results$Intersectional_QS$elpd_loo,
           loo_results$Intersectional_QSGE$elpd_loo)


looic<-list(loo_results$Null_Modelraw$looic,
            loo_results$Linear_AD$looic,
            loo_results$Intersectional_AD$looic,
            loo_results$Linear_QS$looic,
            loo_results$Linear_QSGE$looic,
            loo_results$Intersectional_QS$looic,
            loo_results$Intersectional_QSGE$looic)

waic<-list(waic_results$Null_Modelraw$waic,
           waic_results$Linear_AD$waic,
           waic_results$Intersectional_AD$waic,
           waic_results$Linear_QS$waic,
           waic_results$Linear_QSGE$waic,
           waic_results$Intersectional_QS$waic,
           waic_results$Intersectional_QSGE$waic)

conditional_r2<-list(r2_results$Modelraw_null_r2$R2_Bayes,
                     r2_results$Modelraw_linear_AD_r2$R2_Bayes,
                     r2_results$Modelraw_Intersectional_AD_r2$R2_Bayes,
                     r2_results$Modelraw_linear_QS_r2$R2_Bayes,
                     r2_results$Modelraw_linear_QSGE_r2$R2_Bayes,
                     r2_results$Modelraw_Intersectional_QS_r2$R2_Bayes,
                     r2_results$Modelraw_Intersectional_QSGE_r2$R2_Bayes)

marginal_r2<-list(r2_results$Modelraw_null_r2$R2_Bayes_marginal,
                  r2_results$Modelraw_linear_AD_r2$R2_Bayes_marginal,
                  r2_results$Modelraw_Intersectional_AD_r2$R2_Bayes_marginal,
                  r2_results$Modelraw_linear_QS_r2$R2_Bayes_marginal,
                  r2_results$Modelraw_linear_QSGE_r2$R2_Bayes_marginal,
                  r2_results$Modelraw_Intersectional_QS_r2$R2_Bayes_marginal,
                  r2_results$Modelraw_Intersectional_QSGE_r2$R2_Bayes_marginal)

estimate_fixed<-list(fixed_Modelraw_null <- model_summaries$summary_Model.null$fixed,
                     fixed_Modelraw_linear_AD <- model_summaries$summary_Modelraw.linear.AD$fixed,
                     fixed_Modelraw_linear_QS <- model_summaries$summary_Modelraw.linear.QS$fixed,
                     fixed_Modelraw_linear_QSGE <- model_summaries$summary_Modelraw.linear.QSGE$fixed,
                     fixed_Modelraw_Intersectional_AD <- model_summaries$summary_Modelraw.Intersectional.AD$fixed,
                     fixed_Modelraw_Intersectional_QS <- model_summaries$summary_Modelraw.Intersectional.QS$fixed,
                     fixed_Modelraw_Intersectional_QSGE <- model_summaries$summary_Modelraw.Intersectional.QSGE$fixed)
estimate_random<-list(random_Modelraw_null <- model_summaries$summary_Model.null$random,
                      random_Modelraw_linear_AD <- model_summaries$summary_Modelraw.linear.AD$random,
                      random_Modelraw_linear_QS <- model_summaries$summary_Modelraw.linear.QS$random,
                      random_Modelraw_linear_QSGE <- model_summaries$summary_Modelraw.linear.QSGE$random,
                      random_Modelraw_Intersectional_AD <- model_summaries$summary_Modelraw.Intersectional.AD$random,
                      random_Modelraw_Intersectional_QS <- model_summaries$summary_Modelraw.Intersectional.QS$random,
                      random_Modelraw_Intersectional_QSGE <- model_summaries$summary_Modelraw.Intersectional.QSGE$random)


a1<-random_Modelraw_null$item_id
a2<-random_Modelraw_linear_AD$item_id
a3<-random_Modelraw_linear_QS$item_id
a4<-random_Modelraw_linear_QSGE$item_id
a5<-random_Modelraw_Intersectional_AD$item_id
a6<-random_Modelraw_Intersectional_QS$item_id
a7<-random_Modelraw_Intersectional_QSGE$item_id

b1<-random_Modelraw_null$rater_id
b2<-random_Modelraw_linear_AD$rater_id
b3<-random_Modelraw_linear_QS$rater_id
b4<-random_Modelraw_linear_QSGE$rater_id
b5<-random_Modelraw_Intersectional_AD$rater_id
b6<-random_Modelraw_Intersectional_QS$rater_id
b7<-random_Modelraw_Intersectional_QSGE$rater_id

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

  titlex=paste("Probability of No by rater_race and ",k)
  titlex=paste(titlex,l)
  gr=m$data
  gr1=gr[gr$effect2__=="No",]
  nrc=length(unique(dices$rater_raw_race))*length(unique(dices[[k]]))
  gr1=gr[1:nrc,]
  # print(gr)
  m=ggplot(gr1, aes(x = rater_raw_race, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
    labs(
      title = titlex ,
      x = "Rater Race",
      y = "Probability Of No rating"
    )+scale_y_continuous(
      limits =   c(0,100))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return (m)
}
######################################Model.Intersection.AD################
# rater_race
modelname=" AD Intersectional"
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender ="Man",rater_raw_race=unique(dices$rater_raw_race))
mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_raw_race")

plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "rater_race.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,rater_gender
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender =unique(dices$rater_gender))

mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

plot(mod_plot)[[1]] +facet_wrap("rater_gender")
titlex=paste("Probability of No by rater_race and ","rater_gender")
titlex=paste(titlex,modelname)
gr=m$data

nrc=length(unique(dices$rater_raw_race))*length(unique(dices[["rater_gender"]]))
gr1=gr[1:nrc,]

m=ggplot(gr1, aes(x = rater_raw_race, y = estimate__*100, fill = !! sym("rater_gender"), colour = !! sym("rater_gender"))) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
  labs(
    title = titlex ,
    x = "Rater Race",
    y = "Probability Of No rating"
  )+scale_y_continuous(
    limits =   c(0,100))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# plotb=plottingBar(m,modelname,"rater_gender")
ggsave(filename = "rater_gender_and_rater_raw_race.jpeg", plot = m, width =24, height = 12) 




# rater_raw_race,rater-education
conditions <- expand.grid(rater_gender ="Man",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")
ggsave(filename = "rater_race_and_rater_education.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,rater-age
conditions <- expand.grid(rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.AD,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")

ggsave(filename = "rater_race_and_rater_age.jpeg", plot = plotb, width =24, height = 12) 

######################################Model.Intersection.QS################

# rater_race
modelname=" QS Intersection"
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")
mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")
ggsave(filename = "QS_rater_race.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")
ggsave(filename = "QS_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width =24, height = 12) 




# rater_race,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_gender ="Man",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")

ggsave(filename = "QS_rater_race_and_rater_education.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] + facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")
ggsave(filename = "QS_rater_race_and_rater_age.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QS,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,modelname,"degree_of_harm")


ggsave(filename = "QS_rater_race_and_degree_of_harm.jpeg", plot = plotb, width =24, height = 12) 

######################################Model.Intersection.QSGE################

# rater_race
modelname=" QSGE Intersection"
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")
mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_raw_race")
plotb=plottingBar(m,modelname,"rater_raw_race")

ggsave(filename = "QSGE_rater_gender.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_gender=unique(dices$rater_gender))

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_gender")
plotb=plottingBar(m,modelname,"rater_gender")

ggsave(filename = "QSGE_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width =24, height = 12) 




# rater_race,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_gender ="Man",rater_age="gen x+", rater_raw_race =unique(dices$rater_raw_race),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,modelname,"rater_education")

ggsave(filename = "QSGE_rater_race_and_rater_education.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")
plotb=plottingBar(m,modelname,"rater_age")
ggsave(filename = "QSGE_rater_race_and_rater_age.jpeg", plot = plotb, width =24, height = 12) 


# rater_race,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_raw_race =unique(dices$rater_raw_race),rater_gender ="Man")

mod_plot <- conditional_effects(Modelraw.Intersectional.QSGE,categorical = TRUE, effect ="rater_raw_race" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,modelname,"degree_of_harm")
ggsave(filename = "QSGE_rater_race_and_degree_of_harm.jpeg", plot = plotb, width =24, height = 12) 
