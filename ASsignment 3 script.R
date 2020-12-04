#Packages needed to run the code

library(psych)
library(dplyr)
library(ggplot2)
library(car)
library(tibble)
library(lme4)
library(tidyverse) 
library(cAIC4) 
library(r2glmm) 
library(lmerTest) 
library(MuMIn) 
library(optimx)




#
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

#Uploading the new data files



data_sample_hospital_1 = read.csv("https://tinyurl.com/ha-dataset3")
data_sample_hospital_2 = read.csv("https://tinyurl.com/ha-dataset4")


data_sample_hospital_1 = data_sample_hospital_1 %>% 
 mutate(data_sample_hospital_1 = factor(c(hospital)))

data_sample_hospital_2 = data_sample_hospital_2 %>% 
  mutate(data_sample_hospital_2 = factor(c(hospital)))

data_sample_hospital_1 = data_sample_hospital_1 %>% 
  slice(-c(182,77))

data_sample_hospital_2 = data_sample_hospital_2 %>% 
  slice(-c(5,87,80))


summary(data_sample_hospital_1)
summary(data_sample_hospital_2)





#Visualization

int_plot = data_sample_hospital_1 %>% 		
  ggplot() +		
  aes(y = pain, x = mindfulness, color = hospital) +		
  geom_point(size = 3) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

int_plot	

int_plot+
  xlim(-1, 10)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept=0)



#Loading the theory based model

mod_theory_based = lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_hospital_1)


#Loading a new model on data_set_3 which accounts for random intercept

mod_pain_random_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1|hospital), data = data_sample_hospital_1)


#Visualization

data_sample_hospital_1 = data_sample_hospital_1 %>%
  mutate(prediction_int = predict(mod_pain_random_int))

data_sample_hospital_1 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='blue', aes(y=prediction_int, x=cortisol_serum))+
  facet_wrap( ~ hospital, ncol = 2)

#Comparison of effect

cAIC(mod_theory_based)
cAIC(mod_pain_random_int)

#confidence intervals

confint(mod_pain_random_int, level = 0.95)

#Standard beta coefficients
stdCoef.merMod(mod_pain_random_int)

#Summary
summary(mod_pain_random_int)


#Marginal and Conditional R2 for fixed, and fixed and  random factors
r.squaredGLMM(mod_pain_random_int)



stdCoef.merMod(mod_pain_random_int)	

#R2 value for the random adjusted model


RSS_random_int = sum((data_sample_hospital_2$pain - predict.merMod(mod_pain_random_int))^2)	

RSS(mod_pain_random_int)

RSS_random_int




mod_mean <- lm(pain ~ 1, data = data_sample_hospital_2)	

TSS_random_int = sum((data_sample_hospital_2$pain - predict(mod_mean))^2)	
TSS_random_int

predict(mod_mean)



R2 = 1-(RSS_random_int/TSS_random_int)
R2

#__________*creating a new model with the most influential predictor*______________


mod_pain_random_int_most_influential = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = data_sample_hospital_1)


#Visualization


data_sample_hospital_1 = data_sample_hospital_1 %>% 
  mutate(predict_rand_int_most_infl = predict(mod_pain_random_int_most_influential))

data_sample_hospital_1 %>%
  ggplot() +
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4) +
  geom_line(color='red', aes(y=predict_rand_int_most_infl, x=cortisol_serum))+
  facet_wrap( ~ hospital, ncol = 2)


	
