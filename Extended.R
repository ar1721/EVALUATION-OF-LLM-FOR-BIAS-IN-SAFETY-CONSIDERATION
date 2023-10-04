# #########################Reading dices dataset#######################
dices1=read.csv('https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)


# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)


###################Models###########################
# Gender
formula8<- Q_overall ~ rater_gender * (rater_raw_race + rater_age+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula9<-Q_overall ~ rater_gender * (rater_raw_race + rater_age + degree_of_harm+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula10 <- Q_overall ~ rater_gender *(rater_raw_race + rater_age + degree_of_harm+rater_education+phase) + (degree_of_harm | rater_id) + (1 | item_id)

priors8 <- get_prior(formula8, data = dices, family = cumulative("probit"))
priors9 <- get_prior(formula9, data = dices, family = cumulative("probit"))
priors10 <- get_prior(formula10, data = dices, family = cumulative("probit"))


Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)
Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)
Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)


# Age
formula11<- Q_overall ~ rater_age * (rater_raw_race +rater_gender +rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula12<-Q_overall ~ rater_age * (rater_raw_race + rater_gender + degree_of_harm+rater_education+phase) + (1 | rater_id) + (1 | item_id)

formula13 <- Q_overall ~ rater_age *(rater_raw_race + rater_gender + degree_of_harm+rater_education+phase) + (degree_of_harm | rater_id) + (1 | item_id)

priors11 <- get_prior(formula8, data = dices, family = cumulative("probit"))
priors12 <- get_prior(formula9, data = dices, family = cumulative("probit"))
priors13 <- get_prior(formula10, data = dices, family = cumulative("probit"))


Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)
Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)
Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)


# Education
formula14<- Q_overall ~ rater_education * (rater_raw_race +rater_gender +rater_age+phase) + (1 | rater_id) + (1 | item_id)

formula15<-Q_overall ~ rater_education * (rater_raw_race + rater_gender + degree_of_harm+rater_age+phase) + (1 | rater_id) + (1 | item_id)

formula16 <- Q_overall ~ rater_education *(rater_raw_race + rater_gender + degree_of_harm+rater_age+phase) + (degree_of_harm | rater_id) + (1 | item_id)

priors14 <- get_prior(formula8, data = dices, family = cumulative("probit"))
priors15 <- get_prior(formula9, data = dices, family = cumulative("probit"))
priors16 <- get_prior(formula10, data = dices, family = cumulative("probit"))


Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)
Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)
Model.intersectional.QS <- brm(
  formula = formula2,
  data = dices,
  family = cumulative("probit"),
  prior = priors2,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)

#####################Reference Level Complement######################

