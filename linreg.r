COPD <- read.csv('C:/Users/HP/Documents/R/COPD_student_dataset.csv') 
#Calcualting coorelations and accessing distributions
hist(COPD$MWT1Best)
head(COPD)
hist(COPD$MWT1Best, main="Histogram of MWT1Best", xlab="MWT1Best", breaks=12)

#Calculates a subset
subset(COPD, MWT1Best > 650)
subset(COPD, MWT1Best > 600 | MWT1Best < 150)

#Histogram
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1") 

#List of details
list("Summary" = summary(COPD$MWT1Best), 
     "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), 
     "Range" = range(COPD$MWT1Best, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE))

plot(COPD$FEV1, COPD$MWT1Best, xlab = "FEV1", ylab = "MWT1Best")

#Corelation test
cor.test(COPD$FEV1,COPD$MWT1Best,use="complete.obs",method = "pearson")
cor.test(COPD$FEV1,COPD$MWT1Best,use="complete.obs",method = "spearman")

#Linear Regression
MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)
summary(MWT1Best_FEV1)
confint(MWT1Best_FEV1)   #Confidence intervals
plot(MWT1Best_FEV1)

#Linear Regression
MWT1Best_AGE <- lm(MWT1Best~AGE, data=COPD) 
summary(MWT1Best_AGE) 

#Predicted values
predictedVals <- predict(MWT1Best_AGE)
residualVals <- residuals(MWT1Best_AGE)  #Residual values
par(mfrow=c(2,2))   #To plot all 4 graphs together
plot(MWT1Best_AGE)

par(mfrow=c(1,1))   #To plot the graphs one by one together

hist(residualVals, main = "Histogram of residuals", xlab = "Residuals")

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD)
summary(MWT1Best_FEV1_AGE)
confint(MWT1Best_FEV1_AGE)

dim(COPD) 

#Gives type of variable, summary, histogram acc to respective commands
class(COPD$AGE) 
summary(COPD$AGE) 
hist(COPD$AGE) 

class(COPD$CAT) 
summary(COPD$CAT) 
hist(COPD$CAT) 

class(COPD$COPDSEVERITY) 
table(COPD$COPDSEVERITY, exclude = NULL) 

class(COPD$gender) 
COPD$gender <- as.factor(COPD$gender) #To change it as a factor and not as a numeric value
class(COPD$gender)
table(COPD$gender, exclude = NULL) 

class(COPD$MWT1Best)
class(COPD$copd)

COPD$copd <- factor(COPD$copd)  #To change it as a factor and not as a numeric value
class(COPD$copd)
str(COPD$copd)

lr1 <- lm(MWT1Best~copd,data=COPD)
summary(lr1)

#to use the 'severe' group as a reference category for COPD severity (i.e. level 3)
COPD$copd <- relevel(COPD$copd, ref=3) 
lr1 <- lm(MWT1Best~copd,data=COPD)
summary(lr1)

#To check if any one is positive then comorbid is +ve
comorbid <- length(COPD$Diabetes)
comorbid[COPD$Diabetes==1 | COPD$muscular==1 | COPD$hypertension==1 | COPD$AtrialFib==1 | COPD$IHD==1] <- 1
comorbid[is.na(comorbid)] <- 0
comorbid <- factor(comorbid) #comorbid is a factor of the above variables

print(comorbid)
str(comorbid)
comorbid[15]

COPD$comorbid <- comorbid   #To add it to the copd table


head(COPD)

#Linear Regression between copd and smokers
COPD$smokers <- ifelse(COPD$smoking = 2, 1, 0)  #smoking is 2
COPD$smokers <- factor(COPD$smokers)
lr2 <- lm(MWT1Best~smokers,data=COPD)  #Linear Regression
#For finding Mean
list("Summary" = summary(COPD$lr2), 
     "Mean" = mean(COPD$MWT1Best, na.rm=TRUE))

describe(COPD)

CrossTable(COPD$copd)

summary(COPD$MWT1Best)

hist(COPD$CAT)
COPD$CAT[COPD$CAT > 40] <- NA   #There is an unusual data at 180 which could be an error

#For finding a correlation matrix between multiple variables all at once.
my_data <- COPD[,c("AGE","PackHistory","FEV1","FEV1PRED","FVC","CAT","HAD","SGRQ")]
cor_matrix <- cor(my_data)

cor_matrix
round(cor_matrix,2)

#Correlation Plot
pairs(~AGE+PackHistory+FEV1+FEV1PRED+FVC+CAT+HAD+SGRQ,data=COPD)

#Examine associations between categorical variables
CrossTable(COPD$hypertension,COPD$IHD)

#Check which variables actually effect the outcome one by one
lr1 <- lm(MWT1Best~gender,data=COPD)
lr2 <- lm(MWT1Best~FEV1,data=COPD)

#Checking all similarly it is found FEV1 and copd severity were the only variables
# to effect the outcome properly as it had a pretty big R2 value compared to others
#others like age had some significant effect also so we can include that.

#Now for multi regression variable
m1r1= lm(MWT1Best~FEV1+AGE+factor(gender)+factor(COPDSEVERITY)+factor(comorbid),data=COPD)
summary(m1r1)
confint(m1r1)

imcdiag(model.matrix(m1r1)[,-1],m1r1$model[1],method = 'VIF')  #Not working

#R will not be able to create this new variable if the Diabetes and AtrialFib
# variables are saved as factors!
COPD$Diabetes <- c(0,1)[as.integer(COPD$Diabetes)]
COPD$AtrialFib <- c(0,1)[as.integer(COPD$AtrialFib)]

#New variable
DAF <- COPD$Diabetes * COPD$AtrialFib
r1 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(DAF), data=COPD)
summary(r1)

#Another way
r2 <- lm(MWT1Best~factor(Diabetes)+factor(AtrialFib)+factor(Diabetes*AtrialFib), data=COPD) 



#Prediction values
list("Diabetes"= prediction(r2,at=list(Diabetes=c(0,1))),
     "AtrialFib"= prediction(r2,at=list(AtrialFib=c(0,1))),
     "Diabetes*AtrialFib"= prediction(r2,at=list(Diabetes=c(0,1),AtrialFib=c(0,1))))

#Other predictions
r3 <- lm(MWT1Best~factor(Diabetes)+factor(IHD),data=COPD)
summary(r3)

r4 <- lm(MWT1Best~factor(Diabetes)+factor(IHD)+factor(Diabetes*IHD),data=COPD)
summary(r4)

#To change order of the numbers assigned to smokers and ex smokers
COPD$smoking <- relevel(COPD$smoking, ref = 2)

#To change smokers to 0 so it will be 0 and 1 instead of 1 and 2 to avoid confusion.
COPD$smoking[COPD$smoking == 2] <- 0

