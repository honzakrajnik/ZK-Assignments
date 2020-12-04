#This are the packages needed to carry out the analysis

library(psych)
library(dplyr)
library(ggplot2)
library(car)
library(tibble)
library(lm.beta)


#_____________*Custom functions*_______________



#Table with coefficients function that you ahve provided us in the excercises

coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	



#Loading the data sample
data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")


#____________*Visual data exploration of the variables under investigation*_________________

#Descriptives
View(data_sample_1)
summary(data_sample_1)
describe(data_sample_1)



#Slicing the data that were excluded in the first assignment  

data_sample_1 = data_sample_1 %>% 
  slice(-c(93,150))


#Linear model without the backward regression of my colleague and the theory based model

mod_1_colleague <-lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = data_sample_1) 

mod_theory_based <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum  + mindfulness, data = data_sample_1)

#_____________________*Model diagnostics*_____________
#QQ plot
mod_1_colleague %>% 
  plot(which = 2)

#Histogram
residuals_mod_1_colleague = enframe(residuals(mod_1_colleague))
residuals_mod_1_colleague %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

#Skew and Kurtosis
describe(residuals(mod_1_colleague))


#________*Linearity*__________


mod_1_colleague %>% 
  residualPlots()



#________*Homoscedasticity*________

mod_1_colleague %>% 
  plot(which = 3)


mod_1_colleague %>% 
  ncvTest()


#________*No Multicollinearity*___________


mod_1_colleague %>% 	
  vif()	

data_sample_1 %>% 
  select(age, sex, STAI_trait, pain_cat, cortisol_serum, weight, IQ, household_income) %>% 
  pairs.panels(col = "red", lm = T)



#_____________*The backward model and the comparisons*__________________


step(mod_1_colleague, direction = "backward")

mod_1_colleague_backward <- lm(pain ~ age + sex  + pain_cat + mindfulness + cortisol_serum + household_income, data = data_sample_1) 


summary(mod_1_colleague)
summary(mod_1_colleague_backward)
summary(mod_theory_based)

AIC(mod_1_colleague)
AIC(mod_1_colleague_backward)
AIC(mod_theory_based)


#Printing the table for backward model

sm = summary(mod_1_colleague_backward)

sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table = cbind(as.data.frame(round(cbind(coef(mod_1_colleague_backward), confint(mod_1_colleague_backward), c(0, lm.beta(mod_1_colleague_backward)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	

sm_table = coef_table(mod_1_colleague_backward)	
sm_table



#_______________*New Data Set*____________________

data_sample_2 = read.csv("https://tinyurl.com/ha-dataset2")

describe(data_sample_2)



#Comparing the results

RSS1 = sum((data_sample_2$pain - predict(mod_theory_based))^2)	
RSS1	

RSS2 = sum((data_sample_2$pain - predict(mod_1_colleague_backward))^2)
RSS2








  
