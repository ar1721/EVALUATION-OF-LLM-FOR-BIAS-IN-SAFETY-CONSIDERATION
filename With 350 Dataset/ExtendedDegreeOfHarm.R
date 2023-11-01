dices1=read.csv('https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)


# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)

sumdices<-summary(dices)
###################Models###########################

# Degree of harm

formula17<-Q_overall ~ degree_of_harm * (rater_raw_race + rater_gender +phase+ rater_education+rater_age) + (1 | rater_id) + (1 | item_id)

formula18 <- Q_overall ~ degree_of_harm *(rater_raw_race + rater_gender +phase+ rater_education+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)


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


##################################Plots##########################
