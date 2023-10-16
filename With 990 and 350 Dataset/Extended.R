# #########################Reading dices dataset#######################
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

##################################Plots##########################
conditional_QS_Intersectional<-conditional_effects(Model.intersectional.AD.Gender)
conditional_AD_Intersectional<-conditional_effects(Model.intersectional.QS.Gender)
conditional_QSGE_Intersectional<-conditional_effects(Model.intersectional.QSGE.Gender)

a1=plot(conditional_QS_Intersectional)
b1=plot(conditional_AD_Intersectional)
c1=plot(conditional_QSGE_Intersectional)

output_dir="/Users/amanraj/Desktop/Master Project/EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION/Plots_extended/"

effects=names(a1)
k=1
for(m in a1){
  
  effect_name=effects[k]
  effect_name<-gsub(':','',effect_name)
  plot_filename <- paste0(output_dir, effect_name, "_plot1.png")  # Adjust the file format if needed
  ggsave(filename = plot_filename, plot = m, width = 8, height = 6)  # Adjust width and height as needed
  k=k+1
}

effects=names(b1)
k=1
for(n in b1){
  effect_name=effects[k]
  effect_name<-gsub(':','',effect_name)
  plot_filename <- paste0(output_dir, effect_name, "_plot2.png")  # Adjust the file format if needed
  ggsave(filename = plot_filename, plot = n, width = 8, height = 6)  # Adjust width and height as needed
  k=k+1
}

effects=names(c1)
k=1
for(o in c1){
  effect_name=effects[k]
  effect_name<-gsub(':','',effect_name)
  plot_filename <- paste0(output_dir, effect_name, "_plot3.png")  # Adjust the file format if needed
  ggsave(filename = plot_filename, plot = o, width = 8, height = 6)  # Adjust width and height as needed
  k=k+1
}
#####################Reference Level Complement######################

# Age
formula11<- Q_overall ~ rater_age * (rater_raw_race +rater_gender +rater_education) + (1 | rater_id) + (1 | item_id)

formula12<-Q_overall ~ rater_age * (rater_raw_race + rater_gender + degree_of_harm+rater_education) + (1 | rater_id) + (1 | item_id)

formula13 <- Q_overall ~ rater_age *(rater_raw_race + rater_gender + degree_of_harm+rater_education) + (degree_of_harm | rater_id) + (1 | item_id)


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

# Education
formula14<- Q_overall ~ rater_education * (rater_raw_race +rater_gender +rater_age) + (1 | rater_id) + (1 | item_id)

formula15<-Q_overall ~ rater_education * (rater_raw_race + rater_gender + degree_of_harm+rater_age) + (1 | rater_id) + (1 | item_id)

formula16 <- Q_overall ~ rater_education *(rater_raw_race + rater_gender + degree_of_harm+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)

Model.intersectional.AD.Education <- brm(
  formula = formula14,
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
Model.intersectional.QS.Education <- brm(
  formula = formula15,
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
Model.intersectional.QSGE.Education <- brm(
  formula = formula16,
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
save(Model.intersectional.AD.Education,file="ModelIntersectionalADEducation.RData")
save(Model.intersectional.QS.Education,file="ModelIntersectionalQSEducation.RData")
save(Model.intersectional.QSGE.Education,file="ModelIntersectionalQSGEEducation.RData")

# Degree of harm

formula17<-Q_overall ~ degree_of_harm * (rater_raw_race + rater_gender + rater_education+rater_age) + (1 | rater_id) + (1 | item_id)

formula18 <- Q_overall ~ degree_of_harm *(rater_raw_race + rater_gender + rater_education+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)


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

