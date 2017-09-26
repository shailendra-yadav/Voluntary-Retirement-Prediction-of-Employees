install.packages("plyr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("caTools")
install.packages("plotrix")


library(plyr)
library(dplyr)
library(ggplot2)
library(caTools) # for sample splitting
library(ROCR) # for the ROC/ AUC measure
library(pROC) # for the ROC/ AUC measure
library(rattle) # Visualization of Decision Trees
library(rpart.plot)
library(RColorBrewer)
library(psych) # for point biserial correlation
library(plotrix)

# Load data ---------------------------------------------------------------

# d <- read.csv("your_file_location/Final_Turnover_Data_CSV.csv")

d <- read.csv("/Users/shailendra/Desktop/DS8004/VRP_Project_All/Employee_Turnover.csv")

dim(d) # dataset size
summary(d) # basic summary stats

d <- filter(d, role == "Ind" | role == "Manager" | role == "Director") 
d$role <- factor(d$role) # resetting role so the CEO and VP are listed anymore 
summary(d)

# A Nicer color palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



slices <- table(d$perf)
lbls <- c("Low(1)", "Average(2)", "High(3)")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, col= rainbow(length(lbls)), main="Performacne Rating Distribution")


hist(d$perf, col = cbPalette[6], main = "Distribution of Performance Ratings", 
     xlab = "Performance Rating", ylab = "# Employees" )


agg_perf <- aggregate(vol_leave ~ perf, data = d, mean)

print(agg_perf)



ggplot(agg_perf, aes(x = perf, y = vol_leave)) + geom_bar(stat = "identity", fill = cbPalette[6]) + 
  ggtitle("Voluntary Retirement Ratio Vs.Performance Rating") + 
  labs (y = "Proportion to leave ", x = "Rating in Performance")



agg_gender <- aggregate(vol_leave ~ gender, data = d, mean)
print(agg_gender)


ggplot(agg_gender, aes(x = gender, y = vol_leave)) + geom_bar(stat = "identity", fill = cbPalette[4]) + 
  ggtitle("Voluntary Retirement Ratio by Gender") + 
  labs (y = "Proportion Leaving", x = "Gender")


agg_dept <- aggregate(vol_leave ~ dept, data = d, mean)
print(agg_dept)


ggplot(agg_dept, aes(x = dept, y = vol_leave)) + geom_bar(stat = "identity", fill = cbPalette[4]) + 
  ggtitle("Voluntary Retirement Ratio by Business dept") + 
  labs (y = "Proportion to Leave", x = "Business dept")


agg_as <- aggregate(vol_leave ~ dept + gender, data = d, mean)
print(agg_as)


ind <- order(agg_as$dept, agg_as$gender) #reordering so make comparisons easier

print(agg_as[ind,])


ggplot(agg_as, aes(x = dept, y = vol_leave, fill = gender)) + 
  geom_bar(aes(fill = gender), stat = "identity", position=position_dodge()) + 
  scale_fill_manual(values=cbPalette[3:4]) +
  ggtitle("Voluntary Retirement Ratio by Business dept & gender ") + 
  labs (y = "Proportion to Leave", x = "Business dept")


hist(d$age, breaks = 100, col = cbPalette[4], main = "Distribution of Age", border = F, xlab = "Age")

quantile(d$age)


d$role <- factor(d$role, levels = c("Ind", "Manager", "Director")) # reording levels of role

boxplot(age ~ role, data = d, col= cbPalette[6], main="Employee Age with Role")

d$log_age <- log(d$age)


agg_role <- aggregate(vol_leave ~ role, data = d, mean)
print(agg_role)


ggplot(agg_role, aes(x = role, y = vol_leave)) + 
  geom_bar(stat = "identity", fill = cbPalette[8], width = .7) + 
  ggtitle("Voluntary Retirement Ratio by Role") + 
  labs (y = "Proportion to Leave", x = "Role") 


agg_age <- aggregate(x = d$vol_leave, by = list(cut(d$age, 10)), mean)
print(agg_age)


names(agg_age) <- c("Age", "Probability")

ggplot(agg_age, aes(x = Age, y = Probability)) + 
  geom_bar(stat = "identity", fill = cbPalette[3], width = .7) + 
  ggtitle("Voluntary Retirement Ratio by Age") + 
  labs(y = "Proportion to Leave", x = "Age Cut") 


summary(d$salary)

hist(d$salary, breaks = 50, col = cbPalette[1], main = "Salary Distribution", 
     xlab = "Salary")

quantile(d$salary, probs = seq(0,1,.2))

aggregate(salary ~ role, data = d, mean)

boxplot(salary ~ role, data = d, col = cbPalette[1], main = "Salary by Role")

median_role_salary <- aggregate(salary ~ role, data = d, median) # get the median
print(median_role_salary)

names(median_role_salary)[2] <- "role_median_salary" #rename the median variable

# using merge to add the median salary measure for each person according to role

d <- merge(d, median_role_salary, by = "role")

d$sal_med_diff <- d$salary - d$role_median_salary # creating a difference metric


hist(d$sal_med_diff, breaks = 50, 
     main = "Distribution of Salary Differences \n from Role Median Salary", 
     col = cbPalette[4], xlab = "Difference from Median")


aggregate(sal_med_diff ~ vol_leave, data = d, mean)

biserial(d$sal_med_diff, y = d$vol_leave)


set.seed(42) # setting the random seed for replication
spl <- sample.split(d$vol_leave, 2/3)
train <- d[spl,]
test <- d[!spl,]

# Checking proporion leaving is same for both sets
mean(test$vol_leave)
## [1] 0.3816216
mean(train$vol_leave)
## [1] 0.3814865

m1 <- glm(vol_leave ~ perf + role + log_age + gender + dept + sal_med_diff + gender*dept, data = train, family = 'binomial')
summary(m1) # getting model results

m2 <- glm(vol_leave ~ perf + role + log_age + gender + dept + sal_med_diff, data = train, family = 'binomial')
summary(m2) # getting model results


hist(m2$fitted.values, main = "Distribution of Predicted Probabilities", 
     xlab = "Probability of Leaving", col = cbPalette[4], border = F, breaks = 50)
abline(v = .5, col = "red", lwd = 3)

prop.table(table(m2$fitted.values >= .5))
prop.table(table(m2$fitted.values >= .3))



#=====================Test Data =========================

m2_test <- predict(m2, newdata = test, type = "response")

hist(m2_test, main = "Distribution of Test Set \nPredicted Probabilities", 
     xlab = "Probability of Leaving", col = cbPalette[4], border = F, breaks = 50)
abline(v = .5, col = "red", lwd = 3)


prop.table(table(m2_test >= .5))
prop.table(table(m2_test>= .3))