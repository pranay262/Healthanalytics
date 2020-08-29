g <- read.csv(file = "C:/Users/HP/Documents/R/final diabetes data.csv",
              header=TRUE, sep=',')
head(g)
tail(g)
dim(g)
dimnames(g)[[2]]

chol <- g['chol'] # cholesterol is continuous, so it's easy
gender <- as.factor(g[,'gender']) # but gender isn't.
dm <- as.factor(g[,'dm']) # neither is dm

t <- table(gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total 
#and print the results

round(prop.table(t),digits=3) # get proportions rounded to 3dp

round(100*prop.table(t),digits=1) # get %s rounded to 1dp

dm2 <- factor(dm, exclude=NULL) # make new factor from the old one
table(dm2) # display the counts including the missings (NAs)

summary(chol) #for continous variables

height <- g[,'height']
weight <- g[,'weight']
summary(height)
summary(weight)

height.si <- height*0.0254   #to convert from inches to metres
weight.si <- weight*0.453592  #to convert from pounds to kilograms
bmi <- weight.si/height.si^2
summary(bmi)

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 
# check that the bmi_categorized variable has worked  
table(bmi_categorised, exclude = NULL) 

# frequencies of diabetes by BMI category 
dm_by_bmi_category <- table(bmi_categorised, dm2, exclude = NULL) 

# check 
dm_by_bmi_category 

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) #margin=2 will
#column percentages

#Doing same with age
age <- g[,'age']
age_categorised <- ifelse(age<45,"Under 45",
                          ifelse(age >=45 & age <=64, "Between 45 and 64",
                                 ifelse(age >=65 & age <=74, "Between 65 and 74",
                                        ifelse(age > 74, "Above 74", NA))))
#Or another way to do it directly
age_categorised <- ifelse(g$age<45,"Under 45",
                          ifelse(g$age >=45 & g$age <=64, "Between 45 and 64",
                                 ifelse(g$age >=65 & g$age <=74, "Between 65 and 74",
                                        ifelse(g$age > 74, "Above 74", NA))))

table(age_categorised,exclude = NULL)
gend <- factor(gender, exclude=NULL)
age_by_gender <- table(age_categorised,gend,exclude=NULL)
age_by_gender

#Simple logistic regression with only one predictor value
m <- glm(dm ~ gender, family=binomial (link=logit))
summary(m)
#From summary we can see coefficient is 0.086 which is log odds or logs odds ratio
# for male to female
m$coefficients 
exp(m$coefficients)  #By finding exponent we find odds ratio instead of log odds ratio

levels(gender) #Default makes it acc. to alphabetical order
gender <- relevel(gender, ref = "female") # If you want female as reference and odds of female to male
gender <- relevel(gender, ref = "male") #After changing by alp. order to change back to male as ref.
levels(gender) 

#Same with age
m <- glm(dm ~ age, family=binomial (link=logit))
summary(m)

m$coefficients  #Log odds ratio is 0.525 (increase in log odds in  having diabetes for 1 year increase in age)
exp(m$coefficients)  #Odds ratio is 1.05  (increase in odds in  having diabetes for 1 year increase in age)

#But first we have to check if the predictor has a linear relationship with the
#outcome if the predictor is continuous
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) 
dm_by_age
# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 
freq_table
# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 


head(g)
m2 <- glm(dm ~ g$location, family=binomial (link=logit))
summary(m2)

#Table between location and diabetes
dm_by_location_category <- table(g$location, dm)
dm_by_location_category
round(100 * prop.table(dm_by_location_category, margin = 1), digits = 1)

#Does the same job as histogram but gives a curve instead of blocks 
d <- density(age) 
plot(d,main = "") # gives warnings but the "main" argument suppresses the ugly default title 

#TO FIND OUT WHICH PREDICTORS TO USE IN THE MODEL BY CHECKING EACH ONE AND COMPARING WITH OTHERS
#Analysis on cholestrol
summary(chol)
chol.no.na <- chol[is.na(chol)==0]  #To exclude missing value
d <- density(chol.no.na)
plot(d,main = "") 

summary(g$hdl)
hdl.no <- g$hdl[is.na(g$hdl)==0]
hdl_test <- density(hdl.no)
     
#Assessing crude relations between predictors and the outcome
#Gender
# define the gender variable 
gender <- as.factor(g[,"gender"]) 
# cross tabulation 
dm_by_gender <- table(gender, dm) # not including NA values because there aren't that many 
dm_by_gender
# proportion of diabetes status by gender 
dm_by_gender_prop <- prop.table(dm_by_gender, margin = 1) 
dm_by_gender_prop
# calculate the odds of having diabetes by gender 
odds_gender <- dm_by_gender_prop[, "yes"]/dm_by_gender_prop[, "no"] 
# calculate the log odds 
logodds_gender <- log(odds_gender) 
# plot the log odds of having diabetes by gender 
dotchart(logodds_gender)
plot(as.factor(names(logodds_gender)), logodds_gender) 

#Now for age
# define the age variable (continuous) 
age <- age <- g[,"age"] 
# create a cross tabulation of age and diabetes status  
dm_by_age <- table(age, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by age 
dm_by_age_prop <- prop.table(dm_by_age, margin = 1) 
# calculate the odds of having diabetes 
odds_age <- dm_by_age_prop[, "yes"]/dm_by_age_prop[, "no"] 
odds_age
# calculate the log odds 
logodds_age <- log(odds_age) 
# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_age_prop), logodds_age) 

# age grouping converting continuous variable to a categorical (ordinal) one  
age_grouped <- ifelse(age < 45, "under 45", 
                      ifelse(age >= 45 & age < 65, "45 - 64",  
                             ifelse(age >= 65 & age < 75, "65 - 74",  
                                    ifelse(age >= 75, "75 or over", NA)))) 

age_grouped <- factor(age_grouped, levels = c("under 45", "45 - 64", "65 - 74", "75 or over")) 
# create a cross tabulation of age and diabetes status  
dm_by_age_grouped <- table(age_grouped, dm) 
# output the frequencies of diabetes status by age 
age_grouped_prop <- prop.table(dm_by_age_grouped, margin = 1) 
# calculate the odds of having diabetes 
odds_age_grouped <- age_grouped_prop[, "yes"]/age_grouped_prop[, "no"] 
odds_age_grouped
# calculate the log odds 
logodds_age_grouped <- log(odds_age_grouped) 
# plot the age groups found in the sample against the log odds of having diabetes 
dotchart(logodds_age_grouped) 

#Now for cholesterol
# define chol as a continuous variable 
chol <- g[,"chol"] 
# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol <- table(chol, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by cholesterol 
dm_by_chol_prop <- prop.table(dm_by_chol, margin = 1) 
# calculate the odds of having diabetes 
odds_chol <- dm_by_chol_prop[, "yes"]/dm_by_chol_prop[, "no"] 
# calculate the log odds 
logodds_chol <- log(odds_chol) 
# plot the cholesterol found in the sample against the log odds of having diabetes 
plot(rownames(dm_by_chol_prop), logodds_chol, xlim=c(150, 300)) 

#putting chol into categories
# categorizing chol into an ordinal variable 
chol_categorised <- ifelse(chol < 200, "healthy",  
                           ifelse(chol < 240, "borderline high", 
                                  ifelse(chol >= 240, "high", NA))) 
# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
chol_categorised <- factor(chol_categorised, levels = c("healthy", "borderline high", "high")) 
# create a cross tabulation of cholesterol and diabetes status  
dm_by_chol_categorised <- table(chol_categorised, dm) # not including NA values because there aren't that many 
# output the frequencies of diabetes status by cholesterol 
dm_by_chol_categorised_prop <- prop.table(dm_by_chol_categorised, margin = 1) 
# calculate the odds of having diabetes 
odds_chol_categorised <- dm_by_chol_categorised_prop[, "yes"]/dm_by_chol_categorised_prop[, "no"] 
# calculate the log odds 
logodds_chol_categorised <- log(odds_chol_categorised) 
# plot the cholesterol categories found in the sample against the log odds of having diabetes 
dotchart(logodds_chol_categorised)

#For BMI
#bmi 
height <- g[,"height"] 
weight <- g[,"weight"] 
height.si <- height*0.0254 
weight.si <- weight*0.453592 
bmi <- weight.si/height.si^2 


# categorising BMI 

bmi_categorised <- ifelse(bmi < 18.5, "underweight", 
                          ifelse(bmi >= 18.5 & bmi <= 25, "normal", 
                                 ifelse(bmi > 25 & bmi <= 30, "overweight", 
                                        ifelse(bmi > 30, "obese", NA)))) 

# make sure that it is treated as a factor/categorical variable and ordering the levels within the factor for the table 
bmi_categorised <- factor(bmi_categorised, levels = c("underweight", "normal", "overweight","obese")) 

# create a cross tabulation of BMI and diabetes status  
dm_by_bmi_categorised <- table(bmi_categorised, dm) # not including NA values because there aren't that many 

# output the frequencies of diabetes status by BMI 
dm_by_bmi_categorised_prop <- prop.table(dm_by_bmi_categorised, margin = 1) 

# calculate the odds of having diabetes 
odds_bmi_categorised <- dm_by_bmi_categorised_prop[, "yes"]/dm_by_bmi_categorised_prop[, "no"] 

# calculate the log odds 
logodds_bmi_categorised <- log(odds_bmi_categorised) 

# plot the BMI categories found in the sample against the log odds of having diabetes 
dotchart(logodds_bmi_categorised) 


#Correlation to check if both the variables should be included in the model
cor.test(x=chol,y=g$hdl,method='pearson') 

#MULTIPLE REGRESSION MODEL
m <- glm(dm ~ age + gender + bmi, family=binomial (link=logit)) 
summary(m)
exp(confint(m))



#Question
head(g)
insurance=as.factor(g$insurance)
m3 <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 
summary(m3)
m3$coefficients  #Log odds ratio
exp(m3$coefficients)  #Odds ratio



#McFadden's r square
#Normal r2 but adjusted for log reg.
#High values are better same like normal R2
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model) 

# run a null model 
null_model <- glm(dm ~ 1, family=binomial (link=logit)) 

# check 
summary(null_model) 

# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 

# print it 
R2 

#c statistic or Area under ROC curve (x = 0.5 means model is only as good at predicting the outcome as random chance)
#A curve at or close to the black line (y=x) in the diagram would be an example of this.
#c statistic above 0.5 means it is better than random prediction.
install.packages("DescTools") 
require(DescTools) 
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family=binomial (link=logit)) 

# check your model 
summary(full_model) 
# generate the c-statistic 
Cstat(full_model)

# H-L test (p value of above conventional 0.05 indicates model is a good fit)

# install package "ResourceSelection" 
install.packages("ResourceSelection") 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 

full_model$y
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL 
# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 
# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 
# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"]))

#Another way for H test
install.packages("generalhoslem") 
require(generalhoslem) 
# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) 


# the bigger the deviance, the worse the model fits the data
# design your logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial(link = logit)) 

# analyse table of deviance 
anova(full_model, test = "Chisq") 


#Backwards Elimination
dm <- as.factor(g[,"dm"]) 
insurance <- as.factor(g[,"insurance"])# let's say 0=none, 1=gov, 2=private 
fh <- as.factor(g[,"fh"]) # 1=FH, 0=no FH 
smoking <- as.factor(g[,"smoking"]) # 1,2,3 
chol <- g[,'chol'] 
hdl <- g[,'hdl'] 
ratio <- g[,'ratio'] 
location <- as.factor(g[,'location']) 
age <- g[,'age'] 
gender <- as.factor(g[,'gender']) 
frame <- as.factor(g[,'frame']) 
systolic <- g[,'bp.1s'] 
diastolic <- g[,'bp.1d'] 

model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial(link = logit)) 

summary(model) 

anova(model, test = "Chisq") 
#It's clear that neither of the BP variables is significantly associated with the 
#odds of being diagnosed with diabetes in this data set
#because p values are high and dev values didn't decrease much with addition of these parameters
#The more the dev values change the more impact the parameter has on the model
model <- glm(formula = dm ~ age + bmi + chol + hdl, family = binomial(link = logit))
summary(model)
#The coefficients of the other 4 didn't change much which is good
#We can see blood pressure is not significant here by comparing correlation with other variables
cor.test(systolic, hdl) # not significant 
cor.test(systolic, bmi) # significant 
cor.test(systolic, chol) # very significant
cor.test(systolic, age) # extremely significant 


#Now trying out all 11 predictors
model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + insurance + smoking, family = binomial(link = logit)) 
summary(model) 

anova(model, test = "Chisq") 
