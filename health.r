# To check which directory you are working in: 

getwd() 



# To import the data set 

# you need to change the "file" location to where you've stored the data set  

g <- read.csv(file = "C:/Users/HP/Documents/R/Health Analytics/cancer data for MOOC 1.csv", 
              
              header = TRUE, sep = ',')  





# To have a look at the first few rows of our data set: 

head(g) 



# To inspect the `age` variable: 

g$age 



# To display a summary of the ages of our patients: 

summary(g$age) 



# To display a summary of the genders of our patients: 

table(g$gender) 



# To display a summary of the BMI of our patients: 

summary(g$bmi) 



# To display a summary of the smoking status of our patients: 

table(g$smoking) 



# To display a summary of the exercise status of our patients: 

table(g$exercise) 



# To display a summary of the daily fruit consumption of our patients: 

table(g$fruit) 



# To display a summary of the daily vegetable consumption of our patients: 

table(g$veg) 



# To display a summary of the cancer status of our patients: 

table(g$cancer) 



# To create a new variable `fruitveg`, which sums the daily consumption of fruit and veg of each patient: 

g$fruitveg <- g$fruit + g$veg 



# To display a summary of the combined fruit and veg consumption of our patients: 

table(g$fruitveg) 



# To display a histogram of the ages of our patients: 

hist(g$age) 



# To create a new binary variable `five_a_day`, whether the patient eats at least 5 fruit or veg a day: 

g$five_a_day <- ifelse(g$fruitveg >= 5, 1, 0) 



# To summarise the `five_a_day` variable: 

table(g$five_a_day) 


hist(g$five_a_day)

hist(g$fruitveg)


# To display a histogram of the daily fruit and veg consumption of our patients, including a title and proper axes: 

hist(g$fruitveg, xlab = "Portions of fruit and vegetables", 
     
     main = "Daily consumption of fruit and vegetables combined", axes = F) 

axis(side = 1, at = seq(0, 11, 1)) 

axis(side = 2, at = seq(0, 16, 2)) 

#Using ggplot
ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen")

#For black border
ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black")

#Relabeling
ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") +
  
  labs(x = "Portions of fruit and vegetables", y = "Frequency")

#Making x axis display every number
ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") +
  
  labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1))

#For themes
ggplot() + geom_histogram(data = g, aes(x = fruitveg), bins = 10, fill = "darkgreen", col = "black") +
  
  labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  
  scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()




# To create a new binary variable `healthy_BMI`, whether the patient has a healthy BMI or not: 

g$healthy_BMI <- ifelse(g$bmi > 18.5 & g$bmi < 25, 1, 0) 



# To summarise `healthy_BMI`: 

table(g$healthy_BMI) 

#Histogram
hist(g$fruit, xlab = "Portions of fruit",
     
     main = "Daily consumption of fruit", axes = F)

axis(side = 1, at = seq(0, 4, 1))

axis(side = 2, at = seq(0, 24, 4))

#Hist
hist(g$veg, xlab = "Portions of vegetables",
     
     main = "Daily consumption of vegetables", axes = F)

axis(side = 1, at = seq(0, 9, 1))

axis(side = 2, at = seq(0, 18, 2))

#ggplot
ggplot() + geom_histogram(data = g, aes(x = fruit), bins = 5, fill = "darkgreen", col = "black") +
  
  theme_bw() + labs(x = "Portions of fruit", y = "Frequency") +
  
  scale_x_continuous(breaks = seq(from = 0, to = 4, by = 1))

#ggplot
ggplot() + geom_histogram(data = g, aes(x = veg), bins = 10, fill = "darkgreen", col = "black") + 
  theme_bw() + labs(x = "Portions of vegetables", y = "Frequency") + 
  scale_x_continuous(breaks = seq(from = 0, to = 9, by = 1))



# To run a chi-squared test to look for an association between eating five or more fruit and veg a day and cancer: 

chisq.test(x = g$five_a_day, y = g$cancer) 



# To run a (two-tailed) t-test to see whether the mean BMI of those with cancer is different from the mean BMI of those without cancer: 

t.test(g$bmi ~ g$cancer) 



# To run a (two-tailed) t-test to see whether the mean BMI of those with cancer is different from the mean BMI of those without cancer, where the variances are equal: 

t.test(g$bmi ~ g$cancer, var.equal = T) 





# To run a t-test to see whether the mean BMI of all patients is different from 25: 

t.test(g$bmi, mu = 25) 



# To run a chi-squared test to see whether there is an association between eating five or more fruit a day and having cancer: 

chisq.test(x = g$five_a_day, y = g$cancer) 





# To create a new binary variable, whether overweight or not according to their BMI: 

g$overweight <- ifelse(g$bmi >= 25, 1, 0) 



# To summarise the `overweight` variable: 

table(g$overweight) 



# To run a chi-squared test to see whether there is an association between being overweight and cancer: 

chisq.test(x = g$overweight, y = g$cancer) 