dices1=read.csv('https://raw.githubusercontent.com/google-research-datasets/dices-dataset/main/350/diverse_safety_adversarial_dialog_350.csv')
dices=dices1
raters=unique(dices$rater_id)


# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)

sumdices<-summary(dices)
###################Models###########################

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