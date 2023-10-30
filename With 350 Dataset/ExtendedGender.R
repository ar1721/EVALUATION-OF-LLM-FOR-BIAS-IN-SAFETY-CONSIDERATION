dices1=read.csv('https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)

install.packages("bayestestR")
library(bayestestR)
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


sum=summary(Model.intersectional.AD.Gender)

##################################Plots##########################
# plot(mod_plot, plot = FALSE)[[1]] +facet_wrap("rater_raw_race")


######################################Model.Intersection.AD################
# rater_gender
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
ggsave(filename = "rater_gender.jpeg", plot = m, width = 24, height = 16) 


# rater-gender,rater-race
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
ggsave(filename = "rater_gender_and_rater_raw_race.jpeg", plot = m, width = 24, height = 16) 




# rater_gender,rater-education
conditions <- expand.grid(rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
ggsave(filename = "rater_gender_and_rater_education.jpeg", plot = m, width = 24, height = 16) 


# rater_gender,rater-age
conditions <- expand.grid(rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] + facet_wrap("cats__")+facet_wrap("rater_raw_race")
ggsave(filename = "rater_gender_and_rater_age.jpeg", plot = m, width = 24, height = 16) 






######################################Model.Intersection.QS################
# rater_gender,degree of harm
conditions <- data.frame(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(k,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

plot(mod_plot)[[1]] + facet_wrap("cats__")+facet_wrap("degree_of_harm")
ggsave(filename = "rater_gender_and_degree_of_harm.jpeg", plot = m, width = 24, height = 16) 
######################################Model.Intersection.QSGE################
# rater_gender,degree of harm
conditions <- data.frame(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(k,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

plot(mod_plot)[[1]] + facet_wrap("cats__")+facet_wrap("degree_of_harm")
ggsave(filename = "rater_gender_and_degree_of_harm.jpeg", plot = m, width = 24, height = 16) 


# 
# 
# 
# 
# conditional_AD_Intersectional<-conditional_effects(Model.intersectional.AD.Gender)
# conditional_QS_Intersectional<-conditional_effects(Model.intersectional.QS.Gender)
# conditional_QSGE_Intersectional<-conditional_effects(Model.intersectional.QSGE.Gender)
# 
# a1=plot(conditional_QS_Intersectional)
# b1=plot(conditional_AD_Intersectional)
# c1=plot(conditional_QSGE_Intersectional)
# 
# output_dir="/Users/amanraj/Desktop/Master Project/EVALUATION-OF-LLM-FOR-BIAS-IN-SAFETY-CONSIDERATION/Plots_extended/"
# 
# effects=names(a1)
# k=1
# for(m in a1){
#   
#   effect_name=effects[k]
#   effect_name<-gsub(':','',effect_name)
#   plot_filename <- paste0(output_dir, effect_name, "_plot1.png")  # Adjust the file format if needed
#   ggsave(filename = plot_filename, plot = m, width = 8, height = 6)  # Adjust width and height as needed
#   k=k+1
# }
# 
# effects=names(b1)
# k=1
# for(n in b1){
#   effect_name=effects[k]
#   effect_name<-gsub(':','',effect_name)
#   plot_filename <- paste0(output_dir, effect_name, "_plot2.png")  # Adjust the file format if needed
#   ggsave(filename = plot_filename, plot = n, width = 8, height = 6)  # Adjust width and height as needed
#   k=k+1
# }
# 
# effects=names(c1)
# k=1
# for(o in c1){
#   effect_name=effects[k]
#   effect_name<-gsub(':','',effect_name)
#   plot_filename <- paste0(output_dir, effect_name, "_plot3.png")  # Adjust the file format if needed
#   ggsave(filename = plot_filename, plot = o, width = 8, height = 6)  # Adjust width and height as needed
#   k=k+1
# }