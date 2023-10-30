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

##################################Plots##########################

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

mod_plot <- conditional_effects(Model.intersectional.AD.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")
ggsave(filename = "rater_gender_and_rater_age.jpeg", plot = m, width = 24, height = 16) 






######################################Model.Intersection.QS################

# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
ggsave(filename = "QS_rater_gender.jpeg", plot = m, width = 24, height = 16) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
ggsave(filename = "QS_rater_gender_and_rater_raw_race.jpeg", plot = m, width = 24, height = 16) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
ggsave(filename = "QS_rater_gender_and_rater_education.jpeg", plot = m, width = 24, height = 16) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] + facet_wrap("rater_age")
ggsave(filename = "QS_rater_gender_and_rater_age.jpeg", plot = m, width = 24, height = 16) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
ggsave(filename = "QS_rater_gender_and_degree_of_harm.jpeg", plot = m, width = 24, height = 16) 


######################################Model.Intersection.QSGE################

# rater_gender
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race="White")
mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_gender",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_gender")
ggsave(filename = "QSGE_rater_gender.jpeg", plot = m, width = 24, height = 16) 


# rater-gender,rater-race
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_raw_race=unique(dices$rater_raw_race))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_raw_race")
ggsave(filename = "QSGE_rater_gender_and_rater_raw_race.jpeg", plot = m, width = 24, height = 16) 




# rater_gender,rater-education
conditions <- expand.grid(degree_of_harm="Moderate",rater_raw_race="White",rater_age="gen x+", rater_gender =unique(dices$rater_gender),rater_education =unique(dices$rater_education))

mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_education")
ggsave(filename = "QSGE_rater_gender_and_rater_education.jpeg", plot = m, width = 24, height = 16) 


# rater_gender,rater-age
conditions <- expand.grid(degree_of_harm="Moderate",rater_education = "College degree or higher",rater_age=unique(dices$rater_age), rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")
ggsave(filename = "QSGE_rater_gender_and_rater_age.jpeg", plot = m, width = 24, height = 16) 


# rater_gender,degree of harm
conditions <- expand.grid(rater_education = "College degree or higher",rater_age="gen x+", degree_of_harm=unique(dices$degree_of_harm),rater_gender =unique(dices$rater_gender),rater_raw_race="White")

mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_gender" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")
ggsave(filename = "QSGE_rater_gender_and_degree_of_harm.jpeg", plot = m, width = 24, height = 16) 
