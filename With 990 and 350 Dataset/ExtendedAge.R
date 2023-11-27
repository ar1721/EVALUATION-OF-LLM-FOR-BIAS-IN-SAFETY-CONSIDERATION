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

# Age
formula11<- Q_overall ~ rater_age * (rater_ethinicity+phase +rater_gender +rater_education) + (1 | rater_id) + (1 | item_id)

formula12<-Q_overall ~ rater_age * (rater_ethinicity +phase+ rater_gender + degree_of_harm+rater_education) + (1 | rater_id) + (1 | item_id)

formula13 <- Q_overall ~ rater_age *(rater_ethinicity +phase+ rater_gender + degree_of_harm+rater_education) + (degree_of_harm | rater_id) + (1 | item_id)

prior_thresholds <- c(
  prior(normal(.440,0.5), class=Intercept, coef=1),
  prior(normal(.583,0.5), class=Intercept, coef=2),
  prior(student_t(3,0,2.5), class="b")
)


Model.intersectional.AD.Age <- brm(
  formula = formula11,
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

save(Model.intersectional.AD.Age,file="ModelIntersectionalADAge.RData")

Model.intersectional.QS.Age <- brm(
  formula = formula12,
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

save(Model.intersectional.QS.Age,file="ModelIntersectionalQSAge.RData")

Model.intersectional.QSGE.Age <- brm(
  formula = formula13,
  data = dices,
  family = cumulative("probit"),
  prior = prior_thresholds,
  warmup = 1000,
  iter = 4000,init=0,
  chains = 4,
  seed = 42,
  backend = 'rstan',
  cores = 4
)

save(Model.intersectional.QSGE.Age,file="ModelIntersectionalQSGEAge.RData")





##################################Plots##########################



plottingBar <- function(gr1,l,k) { # create a function with the name my_function
  titlex=paste("Probability of \"No\" by rater_age and ",k)
  titlex=paste(titlex,l)
  
  m=ggplot(gr1, aes(x = rater_age, y = estimate__*100, fill = !! sym(k), colour = !! sym(k))) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(aes(ymin = lower__*100, ymax = upper__*100), width = 0.11, position = position_dodge(width = 0.3)) +
    labs(
      title = titlex ,
      x = "Rater Age",
      y = "Probability Of No rating"
    )+scale_y_continuous(
      limits =   c(0,100))+
    theme_minimal()
  return (m)
}


dof=unique(dices$degree_of_harm)
# For Model.Intersection.AD
# Condition 1: rater_age
conditions1 <- expand.grid(phase="Phase1",rater_age= unique(dices$rater_age), rater_education="College degree or higher", rater_gender = "Man", rater_ethinicity = "White")

# Condition 2: rater_age, rater-race
conditions2 <- expand.grid(phase="Phase1",rater_age= unique(dices$rater_age), rater_education="College degree or higher", rater_gender = "Man", rater_ethinicity = unique(dices$rater_ethinicity))

# Condition 3: rater_age, rater-gender
conditions3 <- expand.grid(phase="Phase1",rater_age= unique(dices$rater_age), rater_ethinicity = "White", rater_education="College degree or higher", rater_gender = unique(dices$rater_gender))

# Condition 4: rater_age, rater-education
conditions4 <- expand.grid(phase="Phase1",rater_age= unique(dices$rater_age),rater_education = unique(dices$rater_education), rater_gender = "Man", rater_ethinicity = "White")

# For Model.Intersection.QS
# Condition 5: rater_age
conditions5 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_age= unique(dices$rater_age), rater_education="College degree or higher", rater_gender = "Man", rater_ethinicity = "White")

# Condition 6: rater_age, rater-race
conditions6 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_age= unique(dices$rater_age), rater_education="College degree or higher", rater_gender = "Man", rater_ethinicity = unique(dices$rater_ethinicity))

# Condition 7: rater_age, rater-gender
conditions7 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_ethinicity = "White", rater_education="College degree or higher", rater_gender = unique(dices$rater_gender), rater_age= unique(dices$rater_age))

# Condition 8: rater_age, rater-education
conditions8 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_age= unique(dices$rater_age), rater_education = unique(dices$rater_education), rater_gender = "Man", rater_ethinicity = "White")

# Condition 9: rater_age, degree of harm
conditions9 <- expand.grid(phase="Phase1",rater_age= unique(dices$rater_age), rater_education="College degree or higher", degree_of_harm =dof[1:4], rater_gender = "Man", rater_ethinicity = "White")

# For Model.Intersection.QSGE
# Condition 10: rater_age
conditions10 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_age= unique(dices$rater_age), rater_education="College degree or higher", rater_gender = "Man", rater_ethinicity = "White")

# Condition 11: rater_age, rater-race
conditions11 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_age= unique(dices$rater_age), rater_education="College degree or higher", rater_gender = "Man", rater_ethinicity = unique(dices$rater_ethinicity))

# Condition 12: rater_age, rater-gender
conditions12 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_ethinicity = "White", rater_education="College degree or higher", rater_gender = unique(dices$rater_gender), rater_age= unique(dices$rater_age))

# Condition 13: rater_age, rater-education
conditions13 <- expand.grid(phase="Phase1",degree_of_harm = "Moderate", rater_age= unique(dices$rater_age), rater_education = unique(dices$rater_education), rater_gender = "Man", rater_ethinicity = "White")

# Condition 14: rater_age, degree of harm
conditions14 <- expand.grid(phase="Phase1",rater_age= unique(dices$rater_age), rater_education="College degree or higher", degree_of_harm =dof[1:4], rater_gender = "Man", rater_ethinicity = "White")

conditions15 <- expand.grid(phase=unique(dices$phase),rater_education = "College degree or higher",rater_age= unique(dices$rater_age), rater_ethinicity ="White",rater_gender ="Man")

conditions16 <- expand.grid(phase=unique(dices$phase),rater_education = "College degree or higher",rater_age= unique(dices$rater_age), degree_of_harm="Moderate",rater_ethinicity ="White",rater_gender ="Man")

conditions17 <- expand.grid(phase=unique(dices$phase),rater_education = "College degree or higher",rater_age= unique(dices$rater_age), degree_of_harm="Moderate",rater_ethinicity ="White",rater_gender ="Man")


######################################Model.Intersection.AD################
modelname=" AD Intersection"


# rater_age
conditions <- conditions1
mod_plot <- conditional_effects(Model.intersectional.AD.Age,categorical = TRUE, effect ="rater_age",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <-conditions2
mod_plot <- conditional_effects(Model.intersectional.AD.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_ethinicity")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_ethinicity"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_rater_age_and_rater_ethinicity.jpeg", plot = plotb, width = 16, height = 8) 




# rater_age,rater_gender
conditions <- conditions3
mod_plot <- conditional_effects(Model.intersectional.AD.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_rater_age_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_age,rater-education
conditions <- conditions4
mod_plot <- conditional_effects(Model.intersectional.AD.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]] +facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Age_rater_age_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 






######################################Model.Intersection.QS################
modelname=" QS Intersection"
# rater_age
conditions <- conditions5
mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_age",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Age_QS_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- conditions6
mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_ethinicity")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_ethinicity"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QS_rater_age_and_rater_ethinicity.jpeg", plot = plotb, width = 16, height = 8) 




# rater_age,rater_gender
conditions <- conditions7
mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Age_QS_rater_age_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_age,rater-age
conditions <- conditions8
mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)
m=plot(mod_plot)[[1]] + facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QS_rater_age_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_age,degree of harm
conditions <- conditions9
mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)


ggsave(filename = "Overall_Age_QS_rater_age_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 


######################################Model.Intersection.QSGE################

modelname=" QSGE Intersection"
# rater_age
conditions <- conditions10
mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_age",conditions = conditions)

m=plot(mod_plot)[[1]]+facet_wrap("rater_age")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_age"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)

ggsave(filename = "Overall_Age_QSGE_rater_age.jpeg", plot = plotb, width = 8, height = 6) 


# rater-gender,rater-race
conditions <- conditions11
mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)

m=plot(mod_plot)[[1]] +facet_wrap("rater_ethinicity")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_ethinicity"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QSGE_rater_age_and_rater_ethinicity.jpeg", plot = plotb, width = 16, height = 8) 




# rater_age,rater_gender
conditions <-conditions12
mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)
str(mod_plot)


m=plot(mod_plot)[[1]] + facet_wrap("rater_gender")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_gender"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QSGE_rater_age_and_rater_gender.jpeg", plot = plotb, width = 8, height = 6) 


# rater_age,rater-education
conditions <- conditions13
mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)
str(mod_plot)

m=plot(mod_plot)[[1]]+facet_wrap("rater_education")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="rater_education"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QSGE_rater_age_and_rater_education.jpeg", plot = plotb, width = 8, height = 6) 


# rater_age,degree of harm
conditions <- conditions14

mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("degree_of_harm")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="degree_of_harm"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QSGE_rater_age_and_degree_of_harm.jpeg", plot = plotb, width = 8, height = 6) 



# rater_education,phase
conditions <- conditions15

mod_plot <- conditional_effects(Model.intersectional.AD.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("phase")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="phase"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_AD_rater_age_and_phase.jpeg", plot = plotb, width = 8, height = 6) 

# rater_education,phase
conditions <- conditions16

mod_plot <- conditional_effects(Model.intersectional.QS.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("phase")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="phase"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QS_rater_age_and_phase.jpeg", plot = plotb, width = 8, height = 6) 



# rater_education,phase
conditions <- conditions17

mod_plot <- conditional_effects(Model.intersectional.QSGE.Age,categorical = TRUE, effect ="rater_age" , conditions = conditions)


m=plot(mod_plot)[[1]] +facet_wrap("phase")

gr=m$data
gr1=gr[gr$effect2__=="No",]
k="phase"

gr1 <- subset(gr1, select = -cond__) 
gr2=distinct(gr1)
plotb=plottingBar(gr2,modelname,k)
ggsave(filename = "Overall_Age_QSGE_rater_age_and_phase.jpeg", plot = plotb, width = 8, height = 6) 
