#This are the packages needed to carry out the analysis

library(psych)
library(dplyr)
library(ggplot2)
library(car)
library(tibble)
library(lm.beta)


#__________*Custom functions*_____________________

# This is a custom function that you wrote which helps in creating the final table for the regression coefficients.	


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



#The quick look at the data already yielded some results as I have noticed that the minimum value for the "STAI_trait" was 3.90, although the minimum score in that scale is 20.00. As a result of this deiscovery I have decidede to remove this value from any further analysis.

#Using the "slice" function to remove the data with the mistake that was in the data. 

data_sample_2 <- data_sample_1 %>% 
  slice(-150,)


#The "ggplot" function was further used to visually examine any possible mistakes or discrepancies in the data. 
data_sample_2 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram(bins = 30)

data_sample_2 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram(bins = 30)



#_____________*Model Creation*_________________


mod_pain_1 <- lm(pain ~ age + sex, data = data_sample_2)
AIC(mod_pain_1)

mod_pain_2 <- lm(pain ~ age + sex + STAI_trait, data = data_sample_2)
AIC(mod_pain_2)

mod_pain_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat, data = data_sample_2)
AIC(mod_pain_3)

mod_pain_4 <-lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum, data = data_sample_2)
AIC(mod_pain_4)

mod_pain_5 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva, data = data_sample_2)
AIC(mod_pain_5)

mod_pain_6 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_2)
AIC(mod_pain_6)


#______________*Variable assessment*______________

#The Cook's test
mod_pain_1 %>% 	
  plot(which = 5)	

mod_pain_1 %>% 	
  plot(which = 4)	

#The Cook's test has revealed an error in data collection upon further investigation we found out that participant #93 recoreded his age as "444" this is considered as a mistake and as such this data point will be omitted from further investigation. 


#Similarly to the previous example where we removed a data point, I had used the slice "function" to remove the whole row, creating a new updated dataset called data_sample_3
data_sample_3 <- data_sample_2 %>% 
  slice(-93,)

#!At this point all the previous models have to be re run with new "data_sample_3!".
mod_pain_1 <- lm(pain ~ age + sex, data = data_sample_3)

mod_pain_6 <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = data_sample_3)




#_____________________*Model diagnostics___________________

#The following section will assess the asumptions of the linear regression hold true. 

#________*Normality*___________ 

#The assumption of normality is that the errors follow normal distribution, this can be visual checked by a histogram, a  QQ plot, or Skew and Kurtosis

#QQ plot
mod_pain_6 %>% 
  plot(which = 2)

#Histogram
residuals_mod_pain_6 = enframe(residuals(mod_pain_6))
residuals_mod_pain_6 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()

#Skew and Kurtosis
describe(residuals(mod_pain_6))


#________*Linearity*__________


#Using .residualPlots to check for linarity
mod_pain_6 %>% 
  residualPlots()
#Despite the slight curve seen in the "mindfulness" and "pain_cat" value the p-values for all the individual variables are > 0.05 which means that the assumption of linearity was not violated and no changes need to be done to the model


#________*Homoscedasticity*________

mod_pain_6 %>% 
  plot(which = 3)

#Although the assumption of Homoscedasticty seems to be intact upon visual examination I decided to run the ncVTest

mod_pain_6 %>% 
  ncvTest()
#The ncvTest returned values >0.05 meaning that the assumption of Homoscedasticity was not violated


#________*No Multicollinearity*___________


mod_pain_6 %>% 	
  vif()	

#The vif value was above 3 in two variables, cortisol_saliva and cortisol_serum, we can investigate their relation by looking at the correlation matrix of the variables

data_sample_3 %>% 
  select(age, sex, STAI_trait, pain_cat, cortisol_serum, cortisol_saliva, mindfulness) %>% 
  pairs.panels(col = "red", lm = T)

#The visualization of this function has revealed a high correlation between the cortisol_serum and cortisol_saliva variables
#Since it is theorized that cortisol_serum is a better predictor, I have decided to remove the cortisol_saliva variable from the model

mod_pain_6_final <- lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum  + mindfulness, data = data_sample_3)



#_________*Data reporting*_______________________

#Models
summary(mod_pain_6_final)
summary(mod_pain_1)

#AIC's
AIC(mod_pain_1)
AIC(mod_pain_6_final)

#Data report using the function you provided in earlier excercises

#Mod_pain_1

sm = summary(mod_pain_1)

sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table = cbind(as.data.frame(round(cbind(coef(mod_pain_1), confint(mod_pain_1), c(0, lm.beta(mod_pain_1)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table["(Intercept)","Std.Beta"] = "0"	


sm_table = coef_table(mod_pain_1)	
sm_table


#Mod_pain_2

sm_final = summary(mod_pain_6_final)

sm_p_values = as.character(round(sm$coefficients[,4], 3))	
sm_p_values[sm_p_values != "0" & sm_p_values != "1"] = substr(sm_p_values[sm_p_values != "0" & sm_p_values != "1"], 2, nchar(sm_p_values[sm_p_values != "0" & sm_p_values != "1"]))	
sm_p_values[sm_p_values == "0"] = "<.001"	


sm_table_final = cbind(as.data.frame(round(cbind(coef(mod_pain_6_final), confint(mod_pain_6_final), c(0, lm.beta(mod_pain_6_final)$standardized.coefficients[c(2,3)])), 2)), sm_p_values)	
names(sm_table_final) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")	
sm_table_final["(Intercept)","Std.Beta"] = "0"	

sm_table_final = coef_table(mod_pain_6_final)	
sm_table_final

#_____________*Regression equation*_______________
#𝑌 = 𝑏0 + 𝑏1 ∗ X1 + 𝑏2 ∗ X2




#______________*Model Comparison*______________

#Since mod_pain_2 has "nested variables" the F-test can tell us a little more about how significant the difference between the two models is

anova(mod_pain_1, mod_pain_6_final)



