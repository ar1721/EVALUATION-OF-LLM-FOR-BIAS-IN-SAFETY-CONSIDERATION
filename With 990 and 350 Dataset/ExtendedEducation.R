# setwd("/home/al3170/Bayesian_Multilevel")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.create(Sys.getenv("R_LIBS_USER"), showWarnings = FALSE, recursive = TRUE)
# install.packages("haven")
# install.packages("tidyverse")
# install.packages("brms", type="binary")
# install.packages("lme4", type="binary")
# install.packages("lmerTest", type="binary")
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("ellipsis" ,type="binary")
# install.packages("tidybayes")
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


library(tidybayes)
library(brms) # for the analysis
library(haven) # to load the SPSS .sav file
# library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(ggridges)
library(loo)

# #########################Reading dices dataset#######################
dices1=read.csv("diverse_safety_adversarial_dialog_350.csv")
dices2=read.csv("diverse_safety_adversarial_dialog_990.csv")
colnm<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q_overall")
colnm2<-c("rater_id","rater_gender","rater_race","rater_race_raw","rater_age","phase","rater_education","item_id","degree_of_harm","Q_overall")
colnm3<-c("rater_id","rater_gender","rater_race","rater_raw_race","rater_age","phase","rater_education","item_id","degree_of_harm","Q_overall")

dices1<-dices1[colnm]
dices3<-dices2[colnm2]
colnames(dices3)<-colnm3
dices=rbind(dices1,dices3)


# ###################Turning Q_Overall rating to numeric from character################

dices<-dices %>% mutate(
  
  rater_ethinicity = case_when(
    
    
    rater_raw_race %in% c("White") ~ "White",
    
    rater_raw_race %in% c(
      
      "Asian", "East or South-East Asian") ~ "Asian",
    
    rater_raw_race %in% c("Black or African American") ~ "Black",
    
    rater_raw_race %in% c(
      
      "Indian",
      
      "Indian subcontinent (including Bangladesh, Bhutan, India, Maldives, Nepal, Pakistan, and Sri Lanka)"
      
    ) ~ "Indian Subcontinent",
    
    rater_raw_race %in% c(
      
      "American Indian or Alaska Native",
      
      "LatinX, Latino, Hispanic or Spanish Origin, American Indian or Alaska Native",
      
      "LatinX, Latino, Hispanic or Spanish Origin, Mexican Indigenous",
      
      "Native Hawaiian or other Pacific Islander",
      
      "White, American Indian or Alaska Native"
      
    ) ~ "Indigenous",
    
    rater_raw_race %in% c(
      
      "Latino, Hispanic or Spanish Origin",
      
      "LatinX, Latino, Hispanic or Spanish Origin") ~ "Latin(x)e",
    
    rater_raw_race %in% c(
      
      "Black or African American, East or South-East Asian",
      
      "LatinX, Latino, Hispanic or Spanish Origin, East or South-East Asian",
      
      "White, East or South-East Asian",
      
      "White, LatinX, Latino, Hispanic or Spanish Origin",
      
      "Mixed") ~ "Multiracial",
    
    rater_raw_race %in% c(
      
      "Middle Eastern or North African",
      
      "Other",
      
      "Prefer not to answer",
      
      ""
    ) ~ "Other"
    
  )
  
) %>%
  
  # make white people the reference group
  
  mutate(rater_ethinicity = relevel(factor(rater_ethinicity), "White"))
dices$Q_overall <- factor(dices$Q_overall, levels = c("No", "Unsure", "Yes"), ordered = TRUE)
###################Models###########################

# Education
formula14<- Q_overall ~ rater_education * (rater_ethinicity +phase+rater_gender +rater_age) + (1 | rater_id) + (1 | item_id)

formula15<-Q_overall ~ rater_education * (rater_ethinicity +phase+ rater_gender + degree_of_harm+rater_age) + (1 | rater_id) + (1 | item_id)

formula16 <- Q_overall ~ rater_education *(rater_ethinicity + phase+rater_gender + degree_of_harm+rater_age) + (degree_of_harm | rater_id) + (1 | item_id)

prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,2.5), class="b")
)


Model.intersectional.AD.Education <- brm(
  formula = formula14,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,init=0,
  backend = 'rstan',
  cores = 4
)

save(Model.intersectional.AD.Education,file="ModelIntersectionalADEducation.RData")

Model.intersectional.QS.Education <- brm(
  formula = formula15,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,init=0,
  backend = 'rstan',
  cores = 4
)

save(Model.intersectional.QS.Education,file="ModelIntersectionalQSEducation.RData")

Model.intersectional.QSGE.Education <- brm(
  formula = formula16,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,
  chains = 4,
  seed = 42,init=0,
  backend = 'rstan',
  cores = 4
)

save(Model.intersectional.QSGE.Education,file="ModelIntersectionalQSGEEducation.RData")



########################Summary Of Model#####################


summary_Modelraw.Intersectional.AD <- summary(Model.intersectional.AD.Education)
summary_Modelraw.Intersectional.QS <- summary(Model.intersectional.QS.Education)
summary_Modelraw.Intersectional.QSGE <- summary(Model.intersectional.QSGE.Education)

fixed_Modelraw_Intersectional_AD <- summary_Modelraw.Intersectional.AD$fixed
fixed_Modelraw_Intersectional_QS <- summary_Modelraw.Intersectional.QS$fixed
fixed_Modelraw_Intersectional_QSGE <- summary_Modelraw.Intersectional.QSGE$fixed
random_Modelraw_Intersectional_AD <- summary_Modelraw.Intersectional.AD$random
random_Modelraw_Intersectional_QS <- summary_Modelraw.Intersectional.QS$random
random_Modelraw_Intersectional_QSGE <- summary_Modelraw.Intersectional.QSGE$random


a=fixed_Modelraw_Intersectional_AD[c("Estimate","l-95% CI","u-95% CI")]
names=c("AD","AD LCI","AD UCI")
names(a)<-names

b=fixed_Modelraw_Intersectional_QS[c("Estimate","l-95% CI","u-95% CI")]
names=c("QS","QS LCI","QS UCI")
names(b)<-names

c=fixed_Modelraw_Intersectional_QSGE[c("Estimate","l-95% CI","u-95% CI")]
names=c("QSGE","QSGE LCI","QSGE UCI")
names(c)<-names

temp=merge(a,b, by = 'row.names', all = TRUE)
temp2=merge(a,c, by = 'row.names', all = TRUE)
names=c("Row.names","QSGE","QSGE LCI","QSGE UCI")
temp2=temp2[names]
fixed=merge(temp,temp2, by = 'row.names', all = TRUE)
fixed=fixed[c("Row.names.x","AD","AD LCI","AD UCI","QS","QS LCI","QS UCI","QSGE","QSGE LCI","QSGE UCI")]








ADitem=random_Modelraw_Intersectional_AD$item_id[c("Estimate","l-95% CI","u-95% CI")]
names=c("AD","AD LCI","AD UCI")
names(ADitem)<-names
rn=rownames(ADitem)
temp3=paste(rn[1],"item_id",sep="_")
rownames(ADitem)<-c(temp3)


ADrater=random_Modelraw_Intersectional_AD$rater_id[c("Estimate","l-95% CI","u-95% CI")]
names=c("AD","AD LCI","AD UCI")
names(ADrater)<-names
rn=rownames(ADrater) 
temp3=paste(rn[1],"rater_id",sep="_")
rownames(ADrater)<-c(temp3)



QSitem=random_Modelraw_Intersectional_QS$item_id[c("Estimate","l-95% CI","u-95% CI")]
names=c("QS","QS LCI","QS UCI")
names(QSitem)<-names
rn=rownames(QSitem)
temp3=paste(rn[1],"item_id",sep="_")
rownames(QSitem)<-c(temp3)

QSrater=random_Modelraw_Intersectional_QS$rater_id[c("Estimate","l-95% CI","u-95% CI")]
names=c("QS","QS LCI","QS UCI")
names(QSrater)<-names
rn=rownames(QSrater)
temp3=paste(rn[1],"rater_id",sep="_")
rownames(QSrater)<-c(temp3)



QSGEitem=random_Modelraw_Intersectional_QSGE$item_id[c("Estimate","l-95% CI","u-95% CI")]
names=c("QSGE","QSGE LCI","QSGE UCI")
names(QSGEitem)<-names
rn=rownames(QSGEitem)
temp3=paste(rn[1],"item_id",sep="_")
rownames(QSGEitem)<-c(temp3)

QSGErater=random_Modelraw_Intersectional_QSGE$rater_id[c("Estimate","l-95% CI","u-95% CI")]
names=c("QSGE","QSGE LCI","QSGE UCI")
names(QSGErater)<-names
rn=rownames(QSGErater)
temp3<-c()
for(i in rn){
  temp3=rbind(temp3,paste(i,"rater_id",sep="_"))
}
rownames(QSGErater)<-c(temp3)

temp4<-merge(ADitem,QSitem, by = 'row.names', all = TRUE)
temp5<-merge(ADitem,QSGEitem, by = 'row.names', all = TRUE)
names=c("Row.names","QSGE","QSGE LCI","QSGE UCI")
temp5=temp5[names]
randomitem<-merge(temp4,temp5, by = 'row.names', all = TRUE)
randomitem=randomitem[c("Row.names.x","AD","AD LCI","AD UCI","QS","QS LCI","QS UCI","QSGE","QSGE LCI","QSGE UCI")]



temp4<-merge(ADrater,QSrater, by = 'row.names', all = TRUE)
temp5<-merge(ADrater,QSGErater, by = 'row.names', all = TRUE)
names=c("Row.names","QSGE","QSGE LCI","QSGE UCI")
temp5=temp5[names]
randomrater<-merge(temp4,temp5, by = 'Row.names', all = TRUE)
randomrater=randomrater[c("Row.names","AD","AD LCI","AD UCI","QS","QS LCI","QS UCI","QSGE","QSGE LCI","QSGE UCI")]
names(randomrater)<-c("Row.names.x","AD","AD LCI","AD UCI","QS","QS LCI","QS UCI","QSGE","QSGE LCI","QSGE UCI")
random<-rbind(fixed,randomitem)
randomfixed=rbind(random,randomrater)

print(include.rownames=FALSE,xtable(randomfixed,type="latex"),file="EstimateEducation.txt")
#########################################POSTERIOR SAMPLES#####################################
# Define the parameter of interest (e.g., rater_age)
parameter_dfAD<-data_frame(Parameter=NA,Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(Model.intersectional.AD.Education)
colPosteriorall<-colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior){
  print(i)
  # Calculate the median of the posterior distribution
  median_estimate <- round(median(parameter_samples[[i]]),5)
  
  # Calculate the 95% Bayesian credible interval
  credible_interval <- round(quantile(parameter_samples[[i]], c(0.025, 0.975)),5)
  appended_values <- paste("[",as.character(credible_interval[1]), as.character(credible_interval[2]),"]")
  credible_interval<-appended_values
  # Calculate the probability of direction (96% chance of being positive)
  probability_direction <- round(mean(parameter_samples[[i]] > 0),5)
  
  # Calculate the probability of practical significance (95% chance of being > 0.05)
  probability_practical_significance <- round(mean(parameter_samples[[i]] > 0.05),5)
  
  # Calculate the probability of having a large effect (89% chance of being > 0.30)
  probability_large_effect <- round(mean(parameter_samples[[i]] > 0.30),5)
  
  parameter_dfAD<-rbind(parameter_dfAD,c(i,median_estimate,credible_interval,probability_direction,probability_practical_significance,probability_large_effect))
  
}
parameter_dfAD<-parameter_dfAD[-1,]




parameter_dfQS<-data_frame(Parameter=NA,Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(Model.intersectional.QS.Education)
colPosteriorall<-colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior){
  print(i)
  # Calculate the median of the posterior distribution
  median_estimate <- round(median(parameter_samples[[i]]),5)
  
  # Calculate the 95% Bayesian credible interval
  credible_interval <- round(quantile(parameter_samples[[i]], c(0.025, 0.975)),5)
  appended_values <- paste("[",as.character(credible_interval[1]), as.character(credible_interval[2]),"]")
  credible_interval<-appended_values
  # Calculate the probability of direction (96% chance of being positive)
  probability_direction <- round(mean(parameter_samples[[i]] > 0),5)
  
  # Calculate the probability of practical significance (95% chance of being > 0.05)
  probability_practical_significance <- round(mean(parameter_samples[[i]] > 0.05),5)
  
  # Calculate the probability of having a large effect (89% chance of being > 0.30)
  probability_large_effect <- round(mean(parameter_samples[[i]] > 0.30),5)
  
  parameter_dfQS<-rbind(parameter_dfQS,c(i,median_estimate,credible_interval,probability_direction,probability_practical_significance,probability_large_effect))
  
}
parameter_dfQS<-parameter_dfQS[-1,]




parameter_dfQSGE<-data_frame(Parameter=NA,Median=NA,CI=NA,Direction=NA,Significance=NA,Large=NA)

# Extract posterior samples for the parameter
parameter_samples <- posterior_samples(Model.intersectional.QSGE.Education)
colPosteriorall<-colnames(parameter_samples)
colPosterior <- colPosteriorall[grep("^b_", colPosteriorall)]

for (i in colPosterior){
  print(i)
  # Calculate the median of the posterior distribution
  median_estimate <- round(median(parameter_samples[[i]]),5)
  
  # Calculate the 95% Bayesian credible interval
  credible_interval <- round(quantile(parameter_samples[[i]], c(0.025, 0.975)),5)
  appended_values <- paste("[",as.character(credible_interval[1]), as.character(credible_interval[2]),"]")
  credible_interval<-appended_values
  # Calculate the probability of direction (96% chance of being positive)
  probability_direction <- round(mean(parameter_samples[[i]] > 0),5)
  
  # Calculate the probability of practical significance (95% chance of being > 0.05)
  probability_practical_significance <- round(mean(parameter_samples[[i]] > 0.05),5)
  
  # Calculate the probability of having a large effect (89% chance of being > 0.30)
  probability_large_effect <- round(mean(parameter_samples[[i]] > 0.30),5)
  
  parameter_dfQSGE<-rbind(parameter_dfQSGE,c(i,median_estimate,credible_interval,probability_direction,probability_practical_significance,probability_large_effect))
  
}
parameter_dfQSGE<-parameter_dfQSGE[-1,]



pTable<-data.frame(parameter_dfAD) 
print(xtable(pTable,type="latex"),file="ADEducation.txt")

pTableQS<-data.frame(parameter_dfQS)
print(xtable(pTableQS,type="latex"),file="QSEducation.txt")


pTableQSGE<-data.frame(parameter_dfQSGE) 
print(xtable(pTableQSGE,type="latex"),file="QSGEEducation.txt")
##################################Plots##########################
# plot(mod_plot, plot = FALSE)[[1]] +facet_wrap("rater_ethinicity")


plottingBar <- function(gr1,l,k) { # create a function with the name my_function
  titlex=paste("Probability of \"No\" by rater_education and ",k)
  titlex=paste(titlex,l)
  
  m=ggplot(gr1, aes(x = rater_education, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.5),size=4) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.5)) +
    labs(
      title = titlex ,
      x = "Rater Education",
      y = "Probability Of No rating"
    )+scale_x_discrete(labels = label_wrap(10),"type",
                       expand=c(0,1.5))+
    scale_y_continuous(
      limits =   c(0,100))+
    theme_minimal()+
    theme(axis.text.x = element_text(size=15),
          axis.title.y = element_text(size = 15),
          plot.title = element_text(size = 15),
          legend.text=element_text(size=15),
          legend.title=element_text(size=15)
    )
  return (m)
}
dof=unique(dices$degree_of_harm)
# For Model.Intersection.AD
# Condition 1: rater_education
conditions1 <- expand.grid(phase="Phase1",rater_education = unique(dices$rater_education), rater_age = "gen x+", rater_gender = "Man", rater_ethinicity = "White")

# Condition 2: rater_education, rater-race
conditions2 <- expand.grid(phase="Phase1",rater_education = unique(dices$rater_education), rater_age = "gen x+", rater_gender = "Man", rater_ethinicity = unique(dices$rater_ethinicity))

# Condition 3: rater_education, rater-gender
conditions3 <- expand.grid(phase="Phase1",rater_education = unique(dices$rater_education), rater_ethinicity = "White", rater_age = "gen x+", rater_gender = unique(dices$rater_gender))

# Condition 4: rater_education, rater-age
conditions4 <- expand.grid(phase="Phase1",rater_education = unique(dices$rater_education), rater_age = unique(dices$rater_age), rater_gender = "Man", rater_ethinicity = "White")

# For Model.Intersection.QS
# Condition 5: rater_education
conditions5 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_education = unique(dices$rater_education), rater_age = "gen x+", rater_gender = "Man", rater_ethinicity = "White")

# Condition 6: rater_education, rater-race
conditions6 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_education = unique(dices$rater_education), rater_age = "gen x+", rater_gender = "Man", rater_ethinicity = unique(dices$rater_ethinicity))

# Condition 7: rater_education, rater-gender
conditions7 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_ethinicity = "White", rater_age = "gen x+", rater_gender = unique(dices$rater_gender), rater_education = unique(dices$rater_education))

# Condition 8: rater_education, rater-age
conditions8 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_education = unique(dices$rater_education), rater_age = unique(dices$rater_age), rater_gender = "Man", rater_ethinicity = "White")

# Condition 9: rater_education, degree of harm
conditions9 <- expand.grid(phase="Phase1",rater_education = unique(dices$rater_education), rater_age = "gen x+", degree_of_harm = dof[1:4], rater_gender = "Man", rater_ethinicity = "White")

# For Model.Intersection.QSGE
# Condition 10: rater_education
conditions10 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_education = unique(dices$rater_education), rater_age = "gen x+", rater_gender = "Man", rater_ethinicity = "White")

# Condition 11: rater_education, rater-race
conditions11 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_education = unique(dices$rater_education), rater_age = "gen x+", rater_gender = "Man", rater_ethinicity = unique(dices$rater_ethinicity))

# Condition 12: rater_education, rater-gender
conditions12 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_ethinicity = "White", rater_age = "gen x+", rater_gender = unique(dices$rater_gender), rater_education = unique(dices$rater_education))

# Condition 13: rater_education, rater-age
conditions13 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_education = unique(dices$rater_education), rater_age = unique(dices$rater_age), rater_gender = "Man", rater_ethinicity = "White")

# Condition 14: rater_education, degree of harm

conditions14 <- expand.grid(phase="Phase1",rater_education = unique(dices$rater_education), rater_age = "gen x+", degree_of_harm = dof[1:4], rater_gender = "Man", rater_ethinicity = "White")

conditions15 <- expand.grid(phase=unique(dices$phase),rater_education = unique(dices$rater_education),rater_age="gen x+", rater_ethinicity ="White",rater_gender ="Man")

conditions16 <- expand.grid(phase=unique(dices$phase),rater_education = unique(dices$rater_education),rater_age="gen x+", degree_of_harm="Moderate",rater_ethinicity ="White",rater_gender ="Man")

conditions17 <- expand.grid(phase=unique(dices$phase),rater_education = unique(dices$rater_education),rater_age="gen x+", degree_of_harm="Moderate",rater_ethinicity ="White",rater_gender ="Man")

######################################Model.Intersection.AD################
modelname=" AD Intersection"


# rater_education
conditions <- conditions1
mod_plot <- conditional_effects(Model.intersectional.AD.Education,categorical = TRUE, effect ="rater_education",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <-conditions2
mod_plot <- conditional_effects(Model.intersectional.AD.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_ethinicity")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_ethinicity"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_rater_education_and_rater_ethinicity.jpeg", plot = plotb, width = 16, height = 8) 




# rater_education,rater_gender
conditions <- conditions3
mod_plot <- conditional_effects(Model.intersectional.AD.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_rater_education_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,rater-age
conditions <- conditions4
mod_plot <- conditional_effects(Model.intersectional.AD.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] +facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Education_rater_education_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 






######################################Model.Intersection.QS################
modelname=" QS Intersection"
# rater_education
conditions <- conditions5
mod_plot <- conditional_effects(Model.intersectional.QS.Education,categorical = TRUE, effect ="rater_education",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Education_QS_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- conditions6
mod_plot <- conditional_effects(Model.intersectional.QS.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_ethinicity")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_ethinicity"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QS_rater_education_and_rater_ethinicity.jpeg", plot = plotb, width = 16, height = 8) 




# rater_education,rater_gender
conditions <- conditions7
mod_plot <- conditional_effects(Model.intersectional.QS.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Education_QS_rater_education_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,rater-age
conditions <- conditions8
mod_plot <- conditional_effects(Model.intersectional.QS.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)
m=plot(mod_plot)[[1]] + facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QS_rater_education_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,degree of harm
conditions <- conditions9
mod_plot <- conditional_effects(Model.intersectional.QS.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)


ggsave(filename = "Overall_Education_QS_rater_education_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


######################################Model.Intersection.QSGE################

modelname=" QSGE Intersection"
# rater_education
conditions <- conditions10
mod_plot <- conditional_effects(Model.intersectional.QSGE.Education,categorical = TRUE, effect ="rater_education",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Education_QSGE_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- conditions11
mod_plot <- conditional_effects(Model.intersectional.QSGE.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_ethinicity")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_ethinicity"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QSGE_rater_education_and_rater_ethinicity.jpeg", plot = plotb, width = 16, height = 8) 




# rater_education,rater_gender
conditions <-conditions12
mod_plot <- conditional_effects(Model.intersectional.QSGE.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QSGE_rater_education_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,rater-age
conditions <- conditions13
mod_plot <- conditional_effects(Model.intersectional.QSGE.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QSGE_rater_education_and_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,degree of harm
conditions <- conditions14

mod_plot <- conditional_effects(Model.intersectional.QSGE.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QSGE_rater_education_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


# rater_education,phase
conditions <- conditions15

mod_plot <- conditional_effects(Model.intersectional.AD.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("phase")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="phase"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_AD_rater_education_and_phase.jpeg", plot = plotb, width = 8, height = 6) 

# rater_education,phase
conditions <- conditions16

mod_plot <- conditional_effects(Model.intersectional.QS.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("phase")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="phase"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QS_rater_education_and_phase.jpeg", plot = plotb, width = 8, height = 6) 



# rater_education,phase
conditions <- conditions17

mod_plot <- conditional_effects(Model.intersectional.QSGE.Education,categorical = TRUE, effect ="rater_education" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("phase")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="phase"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Education_QSGE_rater_education_and_phase.jpeg", plot = plotb, width = 8, height = 6) 
