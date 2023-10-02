# Gender
formula8<- Q_overall ~ rater_gender * (rater_raw_race + rater_age) + (1 | rater_id) + (1 | item_id)

formula9<-Q_overall ~ rater_gender * (rater_raw_race + rater_age + degree_of_harm) + (1 | rater_id) + (1 | item_id)

formula10 <- Q_overall ~ rater_gender *(rater_raw_race + rater_age + degree_of_harm) + (degree_of_harm | rater_id) + (1 | item_id)

priors8 <- get_prior(formula8, data = dices, family = cumulative("probit"))
priors9 <- get_prior(formula9, data = dices, family = cumulative("probit"))
priors10 <- get_prior(formula10, data = dices, family = cumulative("probit"))

# Age


# Education


#####################Reference Level Complement######################

