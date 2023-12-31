install.packages("haven")
install.packages("tidyverse")
install.packages("brms", type="binary")
install.packages("lme4", type="binary")
install.packages("lmerTest", type="binary")
install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
install.packages("ellipsis" ,type="binary")
install.packages("tidybayes")
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)

library(tidybayes)

library(brms) # for the analysis
library(haven) # to load the SPSS .sav file
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(loo)

# #########################Reading dices dataset#######################
dices1=read.csv("https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv")
dices=dices1



# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)


###################Models###########################

# Create formula objects
formula1 <- Q_overall ~ 1 + (1 | rater_id) + (1 | item_id)

formula2 <- Q_overall ~ rater_race + rater_gender + rater_age+ rater_education+ (1 | rater_id) + (1 | item_id)

formula3 <- Q_overall ~ rater_race + rater_gender + rater_age+ rater_education + degree_of_harm + (1 | rater_id) + (1 | item_id)

formula4 <- Q_overall ~ rater_race * (rater_gender + rater_age+ rater_education) + (1 | rater_id) + (1 | item_id)

formula5 <- Q_overall ~ rater_race * (rater_gender + rater_age+ rater_education + degree_of_harm) + (1 | rater_id) + (1 | item_id)

formula6 <- Q_overall ~ rater_race * (rater_gender + rater_age+ rater_education + degree_of_harm) + (degree_of_harm | rater_id) + (1 | item_id)

formula7 <- Q_overall ~ rater_race +rater_gender + rater_age+ rater_education + degree_of_harm + (degree_of_harm | rater_id) + (1 | item_id)

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
save(Model.null,file="Modelnull.RData")

Model.linear.AD <- brm(
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
save(Model.linear.AD,file="ModellinearAD.RData")

Model.linear.QS <- brm(
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
save(Model.linear.QS,file="ModellinearQS.RData")


Model.linear.QSGE <- brm(
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
save(Model.linear.QSGE,file="ModellinearQSGE.RData")

Model.Intersectional.AD <- brm(
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
save(Model.Intersectional.AD,file="ModelIntersectionalAD.RData")


Model.Intersectional.QS <- brm(
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
save(Model.Intersectional.QS,file="ModelIntersectionalQS.RData")


Model.Intersectional.QSGE <- brm(
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
save(Model.Intersectional.QSGE,file="ModelIntersectionalQSGE.RData")


########################Metrics and Transformation#####################

model_summaries <- list()

# Store the summary of each model with "summary" added to the variable name
model_summaries$summary_Model.null <- summary(Model.null)
model_summaries$summary_Model.linear.AD <- summary(Model.linear.AD)
model_summaries$summary_Model.linear.QS <- summary(Model.linear.QS)
model_summaries$summary_Model.linear.QSGE <- summary(Model.linear.QSGE)
model_summaries$summary_Model.Intersectional.AD <- summary(Model.Intersectional.AD)
model_summaries$summary_Model.Intersectional.QS <- summary(Model.Intersectional.QS)
model_summaries$summary_Model.Intersectional.QSGE <- summary(Model.Intersectional.QSGE)


loo_results <- list(
  Null_Model = loo(Model.null),
  Linear_AD = loo(Model.linear.AD),
  Linear_QS = loo(Model.linear.QS),
  Linear_QSGE = loo(Model.linear.QSGE),
  Intersectional_AD = loo(Model.Intersectional.AD),
  Intersectional_QS = loo(Model.Intersectional.QS),
  Intersectional_QSGE = loo(Model.Intersectional.QSGE)
)

waic_results <- list( Null_Model = waic(Model.null),
                      Linear_AD = waic(Model.linear.AD),
                      Linear_QS = waic(Model.linear.QS),
                      Linear_QSGE = waic(Model.linear.QSGE),
                      Intersectional_AD = waic(Model.Intersectional.AD),
                      Intersectional_QS = waic(Model.Intersectional.QS),
                      Intersectional_QSGE = waic(Model.Intersectional.QSGE) )



r2_results<-list( Model_null_r2=performance::r2(Model.null),
                  Model_linear_AD_r2=performance::r2(Model.linear.AD),
                  Model_linear_QS_r2=performance::r2(Model.linear.QS),
                  Model_linear_QSGE_r2=performance::r2(Model.linear.QSGE),
                  Model_Intersectional_AD_r2=performance::r2(Model.Intersectional.AD),
                  Model_Intersectional_QS_r2=performance::r2(Model.Intersectional.QS),
                  Model_Intersectional_QSGE_r2=performance::r2(Model.Intersectional.QSGE))

save(loo_results,file="LooModelReplication.RData")
save(waic_results,file="WAICModelReplication.RData")
save(r2_results,file="R2ModelReplication.RData")

Model.nullTransformed<-ggs(Model.null)
Model.null.fixed_effects <- fixef(Model.null)
Model.null.random_effects <- ranef(Model.null)

Model.Intersectional.ADTransformed<-ggs(Model.Intersectional.AD)
Model.linear.AD.fixed_effects <- fixef(Model.linear.AD)
Model.linear.AD.random_effects <- ranef(Model.linear.AD)

Model.linear.QS.fixed_effects <- fixef(Model.linear.QS)
Model.linear.QS.random_effects <- ranef(Model.linear.QS)

Model.linear.QSGE.fixed_effects <- fixef(Model.linear.QSGE)
Model.linear.QSGE.random_effects <- ranef(Model.linear.QSGE)

Model.Intersectional.AD.fixed_effects <- fixef(Model.Intersectional.AD)
Model.Intersectional.AD.random_effects <- ranef(Model.Intersectional.AD)

Model.Intersectional.QS.fixed_effects <- fixef(Model.Intersectional.QS)
Model.Intersectional.QS.random_effects <- ranef(Model.Intersectional.QS)

Model.Intersectional.QSGE.fixed_effects <- fixef(Model.Intersectional.QSGE)
Model.Intersectional.QSGE.random_effects <- ranef(Model.Intersectional.QSGE)

elpd<-list(loo_results$Null_Model$elpd_loo,
           loo_results$Linear_AD$elpd_loo,
           loo_results$Intersectional_AD$elpd_loo,
           loo_results$Linear_QS$elpd_loo,
           loo_results$Linear_QSGE$elpd_loo,
           loo_results$Intersectional_QS$elpd_loo,
           loo_results$Intersectional_QSGE$elpd_loo)


looic<-list(loo_results$Null_Model$looic,
            loo_results$Linear_AD$looic,
            loo_results$Intersectional_AD$looic,
            loo_results$Linear_QS$looic,
            loo_results$Linear_QSGE$looic,
            loo_results$Intersectional_QS$looic,
            loo_results$Intersectional_QSGE$looic)

waic<-list(waic_results$Null_Model$waic,
           waic_results$Linear_AD$waic,
           waic_results$Intersectional_AD$waic,
           waic_results$Linear_QS$waic,
           waic_results$Linear_QSGE$waic,
           waic_results$Intersectional_QS$waic,
           waic_results$Intersectional_QSGE$waic)

conditional_r2<-list(r2_results$Model_null_r2$R2_Bayes,
                     r2_results$Model_linear_AD_r2$R2_Bayes,
                     r2_results$Model_Intersectional_AD_r2$R2_Bayes,
                     r2_results$Model_linear_QS_r2$R2_Bayes,
                     r2_results$Model_linear_QSGE_r2$R2_Bayes,
                     r2_results$Model_Intersectional_QS_r2$R2_Bayes,
                     r2_results$Model_Intersectional_QSGE_r2$R2_Bayes)

marginal_r2<-list(r2_results$Model_null_r2$R2_Bayes_marginal,
                  r2_results$Model_linear_AD_r2$R2_Bayes_marginal,
                  r2_results$Model_Intersectional_AD_r2$R2_Bayes_marginal,
                  r2_results$Model_linear_QS_r2$R2_Bayes_marginal,
                  r2_results$Model_linear_QSGE_r2$R2_Bayes_marginal,
                  r2_results$Model_Intersectional_QS_r2$R2_Bayes_marginal,
                  r2_results$Model_Intersectional_QSGE_r2$R2_Bayes_marginal)

estimate_fixed<-list(fixed_Model_null <- model_summaries$summary_Model.null$fixed,
                     fixed_Model_linear_AD <- model_summaries$summary_Model.linear.AD$fixed,
                     fixed_Model_linear_QS <- model_summaries$summary_Model.linear.QS$fixed,
                     fixed_Model_linear_QSGE <- model_summaries$summary_Model.linear.QSGE$fixed,
                     fixed_Model_Intersectional_AD <- model_summaries$summary_Model.Intersectional.AD$fixed,
                     fixed_Model_Intersectional_QS <- model_summaries$summary_Model.Intersectional.QS$fixed,
                     fixed_Model_Intersectional_QSGE <- model_summaries$summary_Model.Intersectional.QSGE$fixed)
estimate_random<-list(random_Model_null <- model_summaries$summary_Model.null$random,
                      random_Model_linear_AD <- model_summaries$summary_Model.linear.AD$random,
                      random_Model_linear_QS <- model_summaries$summary_Model.linear.QS$random,
                      random_Model_linear_QSGE <- model_summaries$summary_Model.linear.QSGE$random,
                      random_Model_Intersectional_AD <- model_summaries$summary_Model.Intersectional.AD$random,
                      random_Model_Intersectional_QS <- model_summaries$summary_Model.Intersectional.QS$random,
                      random_Model_Intersectional_QSGE <- model_summaries$summary_Model.Intersectional.QSGE$random)

fixed_Model_null
fixed_Model_linear_AD
fixed_Model_linear_QS
fixed_Model_linear_QSGE
fixed_Model_Intersectional_AD
fixed_Model_Intersectional_QS
fixed_Model_Intersectional_QSGE

a1<-random_Model_null$item_id
a2<-random_Model_linear_AD$item_id
a3<-random_Model_linear_QS$item_id
a4<-random_Model_linear_QSGE$item_id
a5<-random_Model_Intersectional_AD$item_id
a6<-random_Model_Intersectional_QS$item_id
a7<-random_Model_Intersectional_QSGE$item_id

b1<-random_Model_null$rater_id
b2<-random_Model_linear_AD$rater_id
b3<-random_Model_linear_QS$rater_id
b4<-random_Model_linear_QSGE$rater_id
b5<-random_Model_Intersectional_AD$rater_id
b6<-random_Model_Intersectional_QS$rater_id
b7<-random_Model_Intersectional_QSGE$rater_id

# Define the parameter of interest (e.g., rater_age)
parameter_df<-data_frame(Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(Model.Intersectional.AD)
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
parameter_samples <- posterior_samples(Model.Intersectional.QSGE)
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



##########GRAPHS##############
conditional_QS_Intersectional<-conditional_effects(Model.Intersectional.QS)
conditional_AD_Intersectional<-conditional_effects(Model.Intersectional.AD)
conditional_QSGE_Intersectional<-conditional_effects(Model.Intersectional.QSGE)


a=plot(conditional_QS_Intersectional)
b=plot(conditional_AD_Intersectional)
c=plot(conditional_QSGE_Intersectional)

output_dir="/Users/amanraj/Desktop/Master Project/EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION/Plots_fulldataset/"

effects=names(a)
k=1
for(m in a){
  
  effect_name=effects[k]
  effect_name<-gsub(':','',effect_name)
  plot_filename <- paste0(output_dir, effect_name, "_plot1.png")  # Adjust the file format if needed
  ggsave(filename = plot_filename, plot = m, width = 8, height = 6)  # Adjust width and height as needed
  k=k+1
  }

effects=names(b)
k=1
for(n in b){
  effect_name=effects[k]
  effect_name<-gsub(':','',effect_name)
  plot_filename <- paste0(output_dir, effect_name, "_plot2.png")  # Adjust the file format if needed
  ggsave(filename = plot_filename, plot = n, width = 8, height = 6)  # Adjust width and height as needed
  k=k+1
}

effects=names(c)
k=1
for(o in c){
  effect_name=effects[k]
  effect_name<-gsub(':','',effect_name)
  plot_filename <- paste0(output_dir, effect_name, "_plot3.png")  # Adjust the file format if needed
  ggsave(filename = plot_filename, plot = o, width = 8, height = 6)  # Adjust width and height as needed
  k=k+1
}
