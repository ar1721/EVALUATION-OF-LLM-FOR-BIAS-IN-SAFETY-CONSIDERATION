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


sum=summary(Model.intersectional.AD.Gender)

##################################Plots##########################
# plot(mod_plot, plot = FALSE)[[1]] +facet_wrap("rater_raw_race")

plottingBar <- function(m,k) { # create a function with the name my_function
  titlex=paste("Probability of No by rater_gender and ",k)
  gr=m$data
  gr1=gr[gr$effect2__=="No",]
  nrc=length(unique(dices$rater_gender))*length(unique(dices[[k]]))
  gr1=gr[1:nrc,]
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
# rater_gender
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
plotb=plottingBar(m,"rater_gender")
ggsave(filename = "rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
plotb=plottingBar(m,"rater_raw_race")
ggsave(filename = "rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,"rater_education")
ggsave(filename = "rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.AD.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")
plotb=plottingBar(m,"rater_age")

ggsave(filename = "rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 






######################################Model.Intersection.QS################

# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
plotb=plottingBar(m,"rater_gender")

ggsave(filename = "QS_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
plotb=plottingBar(m,"rater_raw_race")
ggsave(filename = "QS_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,"rater_education")

ggsave(filename = "QS_rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
m=plot(mod_plot)[[1]] + facet_wrap("rater_age")
plotb=plottingBar(m,"rater_age")
ggsave(filename = "QS_rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QS.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,"degree_of_harm")


ggsave(filename = "QS_rater_gender_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


######################################Model.Intersection.QSGE################

# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
plotb=plottingBar(m,"rater_gender")

ggsave(filename = "QSGE_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
plotb=plottingBar(m,"rater_raw_race")
ggsave(filename = "QSGE_rater_gender_and_rater_raw_race.jpeg", plot = plotb, width = 16, height = 8) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
plotb=plottingBar(m,"rater_education")
ggsave(filename = "QSGE_rater_gender_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")
plotb=plottingBar(m,"rater_age")
ggsave(filename = "QSGE_rater_gender_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Gender,categorical = TRUE, effect ="rater_gender" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
plotb=plottingBar(m,"degree_of_harm")
ggsave(filename = "QSGE_rater_gender_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 

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
#   ggsave(filename = plot_filename, plot = plotb, width = 8, height = 6)  # Adjust width and height as needed
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