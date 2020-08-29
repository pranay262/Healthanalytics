g <- read.csv(file = "C:/Users/HP/Documents/R/simulated HF mort data.csv", header=TRUE, sep=',')
head(g)
tail(g)
dim(g)
install.packages("survival")
install.packages("ggplot")
install.packages("survminer")
install.packages("ggfortify") 

gender <- as.factor(g[,"gender"]) # R calls categorical variables factors
fu_time <- g[,"fu_time"] # continuous variable (numeric) 
death <- g[,"death"] # binary variable (numeric) 

km_fit <- survfit(Surv(fu_time, death) ~ 1)
plot(km_fit)

summary(km_fit, times = c(1:7,30,60,90*(1:10))) 

survfit(formula = Surv(fu_time, death) ~ 1) 

#survival by gender
km_gender_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_gender_fit)

#With rho = 0, which is the default so we don't need to write this bit, 
#it yields the log-rank or Mantel-Haenszel test
survdiff(Surv(fu_time, death) ~ gender, rho=0) 
#From p value we conclude there is no difference in death based on age.

#Comparing patients aged above 65 and below 65
age_65plus <- ifelse(g[,"age"]>=65,1,0) # dichotomise age
table(age_65plus, exclude = NULL) # inspect the numbers - always a good idea

table(g$age,age_65plus, exclude = NULL) # check - an even better idea...

#Analysis
survdiff(Surv(fu_time, death) ~ age_65plus, rho=0)
#For below 65 obeserved death is below expected deaths 
#and for older than 65 it is viceversa
#This is explained by the low p values that surival times do differ by when you turned 65

cox <- coxph(Surv(fu_time,death) ~ age, data =g)
summary(cox)

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup, data = g) # take variables straight from g
summary(cox)
#unless you tell it otherwise, R will assume that all your variables are continuous. 
#Ethnicity is very obviously not a continuous variable
ethnicgroup <- factor(g[,"ethnicgroup"]) # can also use "as.factor" rather than "factor"
fu_time <- g[,"fu_time"]
death <- g[,"death"]

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup)
summary(cox)

#But all these values are in refrence to ethnicgrp 1 (white people) and we cant see that in table
#Now for the missing ethnic grps we can start by making an "unknown" category for them, which I'll give the value 8 (you could pick anything not already in use)

levels(ethnicgroup)<-c(levels(ethnicgroup),"8") # add level 8 to the factor

ethnicgroup[is.na(ethnicgroup)] <- "8" # Change NA to "8"

cox <- coxph(Surv(fu_time, death) ~ ethnicgroup) 
summary(cox) 

summary(age)

t <- table(gender, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)

round(100*prop.table(t),digits=1) # get %s rounded to 1dp

t <- table(g$copd, exclude=NULL)
addmargins(t) # adds the total (a "sum" column)
round(100*prop.table(t),digits=1) # get %s rounded to 1dp

t <- table(g$prior_dnas, exclude=NULL) 
addmargins(t) # adds the total (a "sum" column) 
round(100*prop.table(t),digits=1) # get %s rounded to 1dp 

t <- table(ethnicgroup, exclude=NULL) 
addmargins(t) # adds the total (a "sum" column) 
round(100*prop.table(t),digits=1) # get %s rounded to 1dp 

cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + g$prior_dnas + ethnicgroup)
summary(cox)

#NON CONVERGENCE
quintile <- as.factor(g[,"quintile"])
cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + quintile + ethnicgroup) 
summary(cox)
#This data set is non converging and we will try to fix it.
table(quintile, exclude=NULL) #Its due to the 4 na values 
t <- table(g$quintile,death) 
t # just the counts 
round(100*prop.table(t,1),digits=1) # row %s 

#1.Now we will use some methods to change it

#Change the reference category
quintile <- relevel(quintile, ref = 2) # quintile 1 as the ref cat again
cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + quintile + ethnicgroup)
summary(cox)
#That didnt work out
#The problem is with quintile zero: look at its huge standard error (1.208e+03) and infinite confidence interval.

#2.Combine categories
#To combine the quintile 0 people with the quintile 5 people, you could make a new variable, which I've called quintile_5groups,
#and just populate its five categories with quintile's values
quintile_5groups <- g[,'quintile'] # best start with the original data set, not from "quintile" 
quintile_5groups[quintile_5groups==0] <- 5 
quintile_5groups <- factor(quintile_5groups) 
table(quintile_5groups, exclude=NULL) 

cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + quintile_5groups + ethnicgroup)
summary(cox)

#This time it's behaved well and has converged. However, was combining the quintile zero people
#with the quintile=5 (most disadvantaged) people a good idea

#3.Drop the quintile zero patients
quintile_5groups <- g[,'quintile'] 
quintile_5groups[quintile_5groups==0] <- NA # set the zeroes to missing 
quintile_5groups <- factor(quintile_5groups) 
table(quintile_5groups, exclude=NULL) 

cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + quintile_5groups + ethnicgroup)
summary(cox)

#4.Drop the offending variable
#This is the simplest and sometimes best option. Just don't put quintile in the list of predictors in the model

cox <- coxph(Surv(fu_time, death) ~ g$age + gender + g$copd + ethnicgroup) 
summary(cox)

#Proportionality assumption
fit <- coxph(Surv(fu_time, death) ~ gender) # fit the desired model
temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # pl

#KM Plot for gender
km_fit <- survfit(Surv(fu_time, death) ~ gender) 
plot(km_fit, xlab = "time", ylab = "Survival probability") # label the axes 

#Deviance residuals
# Generating other diagnostic plots for Cox Proportional Hazards model 
res.cox <- coxph(Surv(fu_time, death) ~ g$age) 
#plots the estimated changes in the regression coefficients on deleting each observation (patient) in turn
ggcoxdiagnostics(res.cox, type = "dfbeta", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 
#Positive values correspond to individuals that "died too soon" compared with expected survival times.
#Negative values correspond to individual that "lived too long" compared with expected survival times.
#Very large or small values are outliers, which are poorly predicted by the model.

res.cox <- coxph(Surv(fu_time, death) ~ g$age) 
ggcoxdiagnostics(res.cox, type = "deviance", 
                 linear.predictions = FALSE, ggtheme = theme_bw()) 

#Martingale residuals 
fit <- coxph(Surv(fu_time, death) ~ g$age + log(g$age) + sqrt(g$age)) 
ggcoxfunctional(fit, data = g) # note we must specify original dataframe 

#testing the proportionality assumption
fit <- coxph(Surv(fu_time, death) ~ g$copd) # fit the desired model
temp <- cox.zph(fit)# apply the cox.zph function to the desired model
print(temp) # display the results
plot(temp) # pl

fit <- coxph(Surv(fu_time, death) ~ gender + tt(gender)) # "tt" is the time-transform function 
summary(fit) 

#Analysing what predictors to include by backwards elimination
# make the other covariates 
ihd <- factor(g[,'ihd']) 
valvular <- factor(g[,'valvular_disease']) 
pvd <- factor(g[,'pvd']) 
stroke <- factor(g[,'stroke']) 
copd<- factor(g[,'copd'])
pneumonia <- factor(g[,'pneumonia']) 
ht <- factor(g[,'hypertension'])
renal <- factor(g[,'renal_disease']) 
ca <- factor(g[,'cancer']) 
mets <- factor(g[,'metastatic_cancer']) 
mental_health <- factor(g[,'mental_health']) 
los <- g[,'los']
prior_dna <- g[,'prior_dnas']

# generate cognitive impairment variable (senility and dementia combined)
cog_imp <- as.factor(ifelse(g$dementia == 1 | g$senile == 1, 1, 0))

# run the full model 
cox <- coxph(Surv(fu_time, death) ~ g$age + gender + ethnicgroup + ihd + 
               
               valvular + pvd + stroke + copd + pneumonia + ht + renal + 
               
               ca + mets + mental_health + cog_imp + los + prior_dna) 

summary(cox) 

#Now by checking values and dropping predictors which have significant p values above 0.05
cox <- coxph(Surv(fu_time, death) ~ g$age + gender + valvular + pneumonia + mets + cog_imp) 
summary(cox) 

#Next, you need to compare the two sets of coefficients using the hazard ratios, 
#i.e. the numbers from the "exp(coef)" bit of the output.
#All other predictors have same value except for metastaic cancer which went from 8.98 to 12.20
#This is a big difference so we have to reduce this change if u think such a chnage is big in this parameter
#here it is fine
table(cog_imp) 
t <- table(cog_imp,death) 
t
round(100*prop.table(t,1),digits=1)

#Testing the proportionality assumption on the remaining variables
fit <- coxph(Surv(fu_time, death) ~ g$age + gender + valvular + pneumonia + 
               
               mets + cog_imp) # test them all in the same model 

temp <- cox.zph(fit)  
print(temp) 
#Nice high p-values, so all's well on that front.