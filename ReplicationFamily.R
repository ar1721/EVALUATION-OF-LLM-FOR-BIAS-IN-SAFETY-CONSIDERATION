# #########################Reading dices dataset#######################
dices1=read.csv("https://github.com/google-research-datasets/dices-dataset/blob/35edd4d9369466114fd5830098f4693c52527128/350/diverse_safety_adversarial_dialog_350.csv?raw=true")
dices=dices1



# ###################Turning Q_Overall rating to numeric from character################
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)
# l<-c("Q_overall")
# for(m in l){
#   k=unique(dices[,m])
#   i=0
#   for(j in k){
#     dices[dices[m]==j,][m]<-i
#     i=i+1
#   }
#   dices[m]<-as.numeric(as.character(dices[[m]]))
# }
# Create formula objects
formula1 <- Q_overall ~ 1 + (1 | rater_id) + (1 | item_id)

formula2 <- Q_overall ~ rater_race + rater_gender + rater_age + (1 | rater_id) + (1 | item_id)

formula3 <- Q_overall ~ rater_race + rater_gender + rater_age + degree_of_harm + (1 | rater_id) + (1 | item_id)

formula4 <- Q_overall ~ rater_race * (rater_gender + rater_age) + (1 | rater_id) + (1 | item_id)

formula5 <- Q_overall ~ rater_race * (rater_gender + rater_age + degree_of_harm) + (1 | rater_id) + (1 | item_id)

formula6 <- Q_overall ~ rater_race * (rater_gender + rater_age + degree_of_harm) + (degree_of_harm | rater_id) + (1 | item_id)

formula7 <- Q_overall ~ rater_race +rater_gender + rater_age + degree_of_harm + (degree_of_harm | rater_id) + (1 | item_id)

# Get prior specifications for the models
priors1 <- get_prior(formula1, data = dices, family = cumulative("probit"))
priors2 <- get_prior(formula2, data = dices, family = cumulative("probit"))
priors3 <- get_prior(formula3, data = dices, family = cumulative("probit"))
priors4 <- get_prior(formula4, data = dices, family = cumulative("probit"))
priors5 <- get_prior(formula5, data = dices, family = cumulative("probit"))
priors6 <- get_prior(formula6, data = dices, family = cumulative("probit"))
priors7 <- get_prior(formula7, data = dices, family = cumulative("probit"))
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
  cores = 4
)

Model.linear.AD <- brm(
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

Model.linear.QS <- brm(
  formula = formula3,
  data = dices,
  family = cumulative("probit"),
  prior = priors3,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)

Model.linear.QSGE <- brm(
  formula = formula7,
  data = dices,
  family = cumulative("probit"),
  prior = priors7,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)

Model.Intersectional.AD <- brm(
  formula = formula4,
  data = dices,
  family = cumulative("probit"),
  prior = priors4,
  warmup = 1000,
  iter = 400,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)

Model.Intersectional.QS <- brm(
  formula = formula5,
  data = dices,
  family = cumulative("probit"),
  prior = priors5,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)

Model.Intersectional.QSGE <- brm(
  formula = formula6,
  data = dices,
  family = cumulative("probit"),
  prior = priors6,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)




###################Models###########################
formula1<-Q_overall  ~ 1 + (1 | rater_id)+(1 | item_id)

# Get prior specifications for the model
priors <- get_prior(formula1, data = dices, family = cumulative("probit"))

Model.null <- brm(
  formula = formula1, 
  data = dices,
  family = cumulative("probit"),
  prior = priors,           # Apply the specified priors
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,
  backend='rstan',
  cores = 4
)








Model.linear.AD<- brm(Q_overall ~ rater_race+rater_gender+rater_age +
                        (1 | rater_id)+(1 | item_id), 
                      data   = dices, 
                      warmup = 1000, 
                      iter   = 4000, 
                      chains = 4, 
                      init  = "random",
                      
                      cores  = 4,
                      seed=42) 



Model.linear.QS<-brm(Q_overall ~ rater_race+rater_gender+rater_age+degree_of_harm+
                       (1 | rater_id)+(1 | item_id), 
                     data  = dices, warmup = 1000,
                     iter  = 4000, chains = 4, 
                     seed  = 42, control = list(adapt_delta = 0.97),
                     cores = 2)




Model.linear.QSGE<-brm(Q_overall ~ rater_race+rater_gender+rater_age+degree_of_harm+
                         (degree_of_harm | rater_id)+(1 | item_id), 
                       data  = dices, warmup = 1000,
                       iter  = 4000, chains = 4, 
                       seed  = 42, control = list(adapt_delta = 0.97),
                       cores = 2)




Model.Intersectional.AD<-brm(Q_overall ~ rater_race*(rater_gender+rater_age)+
                               (1 | rater_id)+(1 | item_id), 
                             data  = dices, warmup = 1000,
                             iter  = 4000, chains = 4, 
                             seed  = 42, control = list(adapt_delta = 0.97),
                             cores = 2)





Model.Intersectional.QS<-brm(Q_overall ~ rater_race*(rater_gender+rater_age+degree_of_harm)+
                               (1 | rater_id)+(1 | item_id), 
                             data  = dices, warmup = 1000,
                             iter  = 4000, chains = 4, 
                             seed  = 42, control = list(adapt_delta = 0.97),
                             cores = 2)




Model.Intersectional.QSGE<-brm(Q_overall ~ rater_race*(rater_gender+rater_age+degree_of_harm)+
                                 (degree_of_harm| rater_id)+(1 | item_id), 
                               data  = dices, warmup = 1000,
                               iter  = 4000, chains = 4, 
                               seed  = 42, control = list(adapt_delta = 0.97),
                               cores = 2)

########################Metrics and Transformation#####################

# WAIC,ELPD, LOOIC,COnditional R2,Marginal R2
# Model.nullTransformed<-ggs(Model.null)
# Model.linear.ADTransformed<-ggs(Model.linear.AD)
# Model.linear.QSTransformed<-ggs(Model.linear.QS)
# Model.linear.QSGETransformed<-ggs(Model.linear.QS)

Model.null.loo<-loo(Model.null)
Model.null.r2<-performance::r2(Model.null)
Model.linear.AD.loo<-loo(Model.linear.AD)
Model.linear.AD.r2<-performance::r2(Model.linear.AD)
Model.linear.QS.loo<-loo(Model.linear.QS)
Model.linear.QS.r2<-performance::r2(Model.linear.QS)
Model.linear.QSGE.loo<-loo(Model.linear.QSGE)
Model.linear.QSGE.r2<-performance::r2(Model.linear.QSGE)


# Model.null.fixed_effects <- fixef(Model.null)
# Model.null.random_effects <- ranef(Model.null)
# 
# Model.linear.AD.fixed_effects <- fixef(Model.linear.AD)
# Model.linear.AD.random_effects <- ranef(Model.linear.AD)
# 
# Model.linear.QS.fixed_effects <- fixef(Model.linear.QS)
# Model.linear.QS.random_effects <- ranef(Model.linear.QS)
# 
# Model.linear.QSGE.fixed_effects <- fixef(Model.linear.QSGE)
# Model.linear.QSGE.random_effects <- ranef(Model.linear.QSGE)


Model.Intersectional.ADTransformed<-ggs(Model.Intersectional.AD)
Model.Intersectional.AD.loo<-loo(Model.Intersectional.AD)
Model.Intersectional.AD.r2<-performance::r2(Model.Intersectional.AD)
Model.Intersectional.AD.fixed_effects <- fixef(Model.Intersectional.AD)
Model.Intersectional.AD.random_effects <- ranef(Model.Intersectional.AD)


Model.Intersectional.QSTransformed<-ggs(Model.Intersectional.QS)
Model.Intersectional.QS.loo<-loo(Model.Intersectional.QS)
Model.Intersectional.QS.r2<-performance::r2(Model.Intersectional.QS)
Model.Intersectional.QS.fixed_effects <- fixef(Model.Intersectional.QS)
Model.Intersectional.QS.random_effects <- ranef(Model.Intersectional.QS)


Model.Intersectional.QSGETransformed<-ggs(Model.Intersectional.QSGE)
Model.Intersectional.QSGE.loo<-loo(Model.Intersectional.QSGE)
Model.Intersectional.QSGE.r2<-performance::r2(Model.Intersectional.QSGE)
Model.Intersectional.QSGE.fixed_effects <- fixef(Model.Intersectional.QSGE)
Model.Intersectional.QSGE.random_effects <- ranef(Model.Intersectional.QSGE)

Model.null.waic<-waic(Model.null)
Model.linear.AD.waic<-waic(Model.linear.AD)
Model.linear.QS.waic<-waic(Model.linear.QS)
Model.linear.QSGE.waic<-waic(Model.linear.QSGE)
Model.Intersectional.AD.waic<-waic(Model.Intersectional.AD)
Model.Intersectional.QS.waic<-waic(Model.Intersectional.QS)
Model.Intersectional.QSGE.waic<-waic(Model.Intersectional.QSGE)





# Coefficient table Table 6
coefficients_table<-data.frame(col1="",col2="",col3="",col4="")
# Null table
model_summary.null <- summary(Model.null)
fix=model_summary.null$fixed
rand=model_summary.null$random
fixrow=rownames(fix)

for(name in fixrow){
  insert_row<-fix[name,]
  coefficients_table<-data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`)
}

coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))


# Linear AD
model_summary.linear.AD<- summary(Model.linear.AD)
fix=model_summary.linear.AD$fixed
rand=model_summary.linear.AD$random
fixrow=rownames(fix)
for(name in fixrow){
  insert_row<-fix[name,]
  coefficients_table<-rbind(coefficients_table, data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`))
}
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))


# Intersectional AD
model_summary.Intersectional.AD<- summary(Model.Intersectional.AD)
fix=model_summary.Intersectional.AD$fixed
rand=model_summary.Intersectional.AD$random
fixrow=rownames(fix)
for(name in fixrow){
  insert_row<-fix[name,]
  coefficients_table<-rbind(coefficients_table, data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`))
}
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))

write_csv(coefficients_table,"Table_6:AD_coeffecient.csv")





# Coefficient table Table 5
coefficients_table<-data.frame(col1="",col2="",col3="",col4="")

# Linear QS
model_summary.linear.QS<- summary(Model.linear.QS)
fix=model_summary.linear.QS$fixed
rand=model_summary.linear.QS$random
fixrow=rownames(fix)
for(name in fixrow){
  insert_row<-fix[name,]
  coefficients_table<-rbind(coefficients_table, data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`))
}
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))


# Linear QSGE
model_summary.linear.QSGE<- summary(Model.linear.QSGE)
fix=model_summary.linear.QSGE$fixed
rand=model_summary.linear.QSGE$random
fixrow=rownames(fix)
for(name in fixrow){
  insert_row<-fix[name,]
  coefficients_table<-rbind(coefficients_table, data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`))
}
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))

# Intersectional QS
model_summary.Intersectional.QS<- summary(Model.Intersectional.QS)
fix=model_summary.Intersectional.QS$fixed
rand=model_summary.Intersectional.QS$random
fixrow=rownames(fix)
for(name in fixrow){ 
  insert_row<-fix[name,]
  coefficients_table<-rbind(coefficients_table, data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`))
}
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))


# Intersectional QSGE
model_summary.Intersectional.QSGE<- summary(Model.Intersectional.QSGE)
fix=model_summary.Intersectional.QSGE$fixed
rand=model_summary.Intersectional.QSGE$random
fixrow=rownames(fix)
for(name in fixrow){
  insert_row<-fix[name,]
  coefficients_table<-rbind(coefficients_table, data.frame(col1=name,col2=insert_row$Estimate,col3=insert_row$`l-95% CI`,col4=insert_row$`u-95% CI`))
}
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_item_id_intercept",col2=rand$item_id$Estimate,col3=rand$item_id$`l-95% CI`,col4=rand$item_id$`u-95% CI`))
coefficients_table<-rbind(coefficients_table, data.frame(col1="sd_conversation_id_intercept",col2=rand$rater_id$Estimate,col3=rand$rater_id$`l-95% CI`,col4=rand$rater_id$`u-95% CI`))

write_csv(coefficients_table,"Table_5:QS_coeffecient.csv")


posterior_samples_Model_Intersectional_AD <- posterior_samples(Model.Intersectional.AD)



# Create a data frame with parameter names, medians, and credible intervals
selected_columns <- select(posterior_samples,
                           starts_with("b_"))

standardized_columns <- mutate(selected_columns, 
                               across(everything(), .fns = ~ . / sd(.)))

reshaped_data <- pivot_longer(standardized_columns,
                              cols = everything(),
                              names_to = "Parameter",
                              values_to = "Value")

parameter_summary_Model_Intersectionl_AD<- reshaped_data %>%
  group_by(Parameter) %>%
  reframe(
    Median = median(Value),
    CI = quantile(Value, c(0.025, 0.975)),
    Direction = ifelse(Median > 0, "Positive", "Negative"),
    Significance = ifelse(abs(Median) > 1.96, "Yes", "No"),  # Using a threshold for significance
    Large = ifelse(abs(Median) > 0.5, "Yes", "No")  # Using a threshold for practical significance
  )

posterior_samples_Model.Intersectional.QSGE <- posterior_samples(Model.Intersectional.QSGE)


selected_columns <- select(posterior_samples,
                           starts_with("b_"))

standardized_columns <- mutate(selected_columns, 
                               across(everything(), .fns = ~ . / sd(.)))

reshaped_data <- pivot_longer(standardized_columns,
                              cols = everything(),
                              names_to = "Parameter",
                              values_to = "Value")

parameter_summary_Model_Intersectionl_QSGE<- reshaped_data %>%
  group_by(Parameter) %>%
  reframe(
    Median = median(Value),
    CI = quantile(Value, c(0.025, 0.975)),
    Direction = ifelse(Median > 0, "Positive", "Negative"),
    Significance = ifelse(abs(Median) > 1.96, "Yes", "No"),  # Using a threshold for significance
    Large = ifelse(abs(Median) > 0.5, "Yes", "No")  # Using a threshold for practical significance
  )
# ############Charts and Graphs##################

# Define reference levels for gender and phase
reference_levels <- data.frame(
  rater_gender = "Man",
  rater_age = "millenial"  # Replace with the mean of your age variable
)

# Create a new data frame for predictions
new_data <- expand.grid(
  race = unique(dices$rater_race),  # Replace with your race/ethnicity variable
  gender = reference_levels$rater_gender,
  age = reference_levels$rater_age
)

# Generate conditional predictions
conditional_preds <- conditional_effects(Model.Intersectional.AD, 
                                         new_data = new_data)

# Create the plot
Q2.5=conditional_preds$rater_race$lower__
Q97.5=conditional_preds$rater_race$upper__
Estimate=conditional_preds$rater_race$estimate_


jpeg("Figure1.jpg", width = 1000, height = "1000")
ggplot(conditional_preds$rater_race, aes(x = conditional_preds$rater_race$rater_race, y = Estimate, ymin = Q2.5, ymax = Q97.5, group =conditional_preds$rater_race$rater_race)) +
  geom_point() +
  geom_errorbar(aes(ymax = Estimate + (Estimate - Q2.5), ymin = Estimate - (Q97.5 - Estimate)),width = 0.15) +
  labs(
    x = "Race/Ethnicity",
    y = "Estimated Likelihood of Unsafe Rating",
    title = "Estimated Likelihood of Unsafe Rating by Race/Ethnicity",
    subtitle = "Controlling for Gender and Age"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
dev.off()

##################################Figure 2##########################

# Define reference levels for gender and phase
reference_levels <- data.frame(
  rater_gender = c("Man","Woman"),
  rater_age = "millenial"  # Replace with the mean of your age variable
)

# Create a new data frame for predictions
new_data <- expand.grid(
  race = unique(dices$rater_race),  # Replace with your race/ethnicity variable
  gender = reference_levels$rater_gender,
  age = reference_levels$rater_age
)

# Generate conditional predictions
conditional_preds <- conditional_effects(Model.Intersectional.AD, 
                                         new_data = new_data)


custom_colors <- c("Woman" = "blue", "Man" = "red")
# Create the plot
Q2.5=conditional_preds$rater_race$lower__
Q97.5=conditional_preds$rater_race$upper__
Estimate=conditional_preds$rater_race$estimate_
race=conditional_preds$rater_race$rater_race
gender=conditional_preds$rater_race$rater_gender

jpeg("Figure2.jpg", width = 1000, height = "1000")
ggplot(conditional_preds$rater_race, aes(x = race, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = gender)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = Estimate - (Estimate - Q2.5), ymax = Estimate + (Q97.5 - Estimate)), 
                position = position_dodge(width = 0.8), width = 0.3) +
  labs(
    x = "Race/Ethnicity",
    y = "Estimated Difference in Safety Risk Reporting (Women vs. Men)",
    title = "Conditional Effects of Gender on Safety Risk Reporting by Race/Ethnicity",
    subtitle = "Controlling for Age and Education (Average Levels)"
  ) +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
dev.off()


##################################Figure 2##########################

# Define reference levels for gender and phase
reference_levels <- data.frame(
  rater_age = c("gen x+","gen z")  # Replace with the mean of your age variable
)

# Create a new data frame for predictions
new_data <- expand.grid(
  race = unique(dices$rater_race),  # Replace with your race/ethnicity variable
  age = reference_levels$rater_age
)

# Generate conditional predictions
conditional_preds <- conditional_effects(Model.Intersectional.AD, 
                                         new_data = new_data)


custom_colors <- c("gen x+" = "blue", "gen z" = "red")
# Create the plot
Q2.5=conditional_preds$rater_race$lower__
Q97.5=conditional_preds$rater_race$upper__
Estimate=conditional_preds$rater_race$estimate_
race=conditional_preds$rater_race$rater_race
age=conditional_preds$rater_race$rater_age

# jpeg("Figure4.jpg", width = 1000, height = "1000")
ggplot(conditional_preds$rater_race, aes(x = race, y = Estimate, ymin = Q2.5, ymax = Q97.5, color = age)) +
  geom_point(position = position_dodge(width = 0.5)) +
  
  labs(
    x = "Race/Ethnicity",
    y = "Estimated Difference in Safety Risk Reporting (Women vs. Men)",
    title = "Conditional Effects of Gender on Safety Risk Reporting by Race/Ethnicity",
    subtitle = "Controlling for Age and Education (Average Levels)"
  ) +
  scale_fill_manual(values = custom_colors) +  # Set custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
# dev.off()
