# Lam Lay
# EXST 7142
# Final Project

bank_original <- read.csv2("bank-full.csv")
summary(bank_original)
str(bank_original)

bank = bank_original

# Output variable
table_y <- table(bank$y)
prop.table(table_y)

library(ggplot2)
library("patchwork")  # Multiple ggplots
library(gmodels)      # to use CrossTable()
library(forcats)      # to reorder bar chart (fct_rev & fct_infreq)
library(dplyr)        
library(caret)
library(InformationValue)   # to use optimalCutoff()
library(ROCR)
library(rpart)
library(rpart.plot) 

# First look at the dataset
summary(bank_original)
str(bank_original)

sapply(bank_original, function(x) sum(is.na(x)))  # No missing Values 
sum(duplicated(bank_original))  # Before all the cleaning, there was no duplicated rows 

prop.table(table(bank$y))

# ==================================================
# Graphical Presentations

# Age
plot1 <- ggplot(bank, aes(x=age, fill=y))+
  geom_bar()+
  ggtitle("Age of clients")+
  xlab("age")+
  guides(fill=guide_legend(title="y")) +
  theme(text = element_text(size = 15))
plot1

# Therefore we divide the continous age variable into 3 categories.  
attach(bank)
bank$ageGroup[age >= 60] <- c("old")
bank$ageGroup[age >= 30 & age < 60] <- c("middle")
bank$ageGroup[age < 30] <- c("young")
detach(bank)
CrossTable(bank$ageGroup, bank$y, prop.c = F, prop.t = F, prop.chisq = F)

# campaign
attach(bank)
bank$campaignGroup[campaign >= 1 & campaign <= 10] <- c("between 0 and 10")
bank$campaignGroup[campaign > 10 & campaign <= 20] <- c("between 11 and 20")
bank$campaignGroup[campaign > 20 & campaign <= 30] <- c("between 21 and 30")
bank$campaignGroup[campaign > 30 & campaign <= 40] <- c("between 31 and 40")
bank$campaignGroup[campaign > 40 & campaign <= 50] <- c("between 41 and 50")
detach(bank)

plot2 <- ggplot(bank, aes(x=campaign, fill=y))+
  geom_bar()+
  ggtitle("Number of Contact during Campaign")+
  xlab("campaign")+
  xlim(c(min=1,max=30)) +
  guides(fill=guide_legend(title="y")) +
  theme(text = element_text(size = 15))
plot2 

plot1 + plot2 + plot_layout(guides = "collect")

# Job
CrossTable(bank$job, bank$y, prop.c = F, prop.t = F, prop.chisq = F)

ggplot(data = bank_original, aes(x = fct_rev(fct_infreq(job)), fill=y) ) + geom_bar() + coord_flip() +
  xlab("job") + ggtitle("Job Distribution of Customers") + 
  theme(text = element_text(size = 20))

# Histograms 1
p1 <- ggplot(data = bank_original, aes(x=marital, fill=y) ) + geom_bar() +
  xlab("marital") + ggtitle("Marital Status") + 
  theme(text = element_text(size = 12))
p1

p2 <- ggplot(data = bank_original, aes(x=education, fill=y) ) + geom_bar() +
  xlab("education") + ggtitle("Education Status") + 
  theme(text = element_text(size = 12))
p2

p3 <- ggplot(data = bank_original, aes(x=default, fill=y) ) + geom_bar() +
  xlab("default") + ggtitle("Do clients have credit in default?") + 
  theme(text = element_text(size = 12))
p3

p4 <- ggplot(data = bank_original, aes(x=loan, fill=y) ) + geom_bar() +
  xlab("loan") + ggtitle("Do clients have personal loan?") + 
  theme(text = element_text(size = 12))
p4

p1 + p2 + p3 + p4 + plot_layout(guides = "collect")

# Histograms 2
p5 <- ggplot(data = bank_original, aes(x=housing, fill=y) ) + geom_bar() +
  xlab("housing") + ggtitle("Do clients have housing loan?") + 
  theme(text = element_text(size = 12))
p5

p6 <- ggplot(data = bank_original, aes(x=contact, fill=y) ) + geom_bar() +
  xlab("contact") + ggtitle("Contact communication types") + 
  theme(text = element_text(size = 12))
p6

p7 <- ggplot(data = bank_original, aes(x=month, fill=y) ) + geom_bar() +
  xlab("month") + ggtitle("Last contact month of year") + 
  theme(text = element_text(size = 12))
p7

p8 <- ggplot(data = bank_original, aes(x=day, fill=y) ) + geom_bar() +
  xlab("day") + ggtitle("Last contact day of the month") + 
  theme(text = element_text(size = 12))
p8

p5 + p6 + p7 + p8 + plot_layout(guides = "collect")

# last contact duration, in seconds
plot(bank$duration)

# pdays (999 means client was not previously contacted)
attach(bank)
bank$pdaysGroup[pdays == 999] <- 0
bank$pdaysGroup[pdays != 999] <- 1
detach(bank)

nrow(bank[bank$pdays == '999',])   # there are no rows with pdays = 999

# previous: number of contacts performed before this campaign and for this client (numeric)
table(bank$previous)

attach(bank)
bank$previousGroup[previous >= 2] <- c("2 or more")
bank$previousGroup[previous == 1] <- c("1")
bank$previousGroup[previous == 0] <- c("0")
detach(bank)

# balance
ggplot(bank, aes(x = 1:nrow(bank), y = balance)) +  # Apply nrow function
  geom_point() + theme(text = element_text(size = 20))  

attach(bank)
bank$balanceGroup[balance < 0] <- c("< 0")
bank$balanceGroup[balance >= 0 & balance <= 25000] <- c("between 0 and 25000")
bank$balanceGroup[balance > 25000] <- c("> 25000")
detach(bank)

# Histograms 3
p9 <- ggplot(data = bank, aes(x=pdaysGroup, fill=y) ) + geom_bar() +
  xlab("pdays") + ggtitle("Days passed after the last contact") + 
  theme(text = element_text(size = 12))
p9

p10 <- ggplot(data = bank, aes(x=previousGroup, fill=y) ) + geom_bar() +
  xlab("previous") + ggtitle("Number of contacts before this campaign") + 
  theme(text = element_text(size = 12))
p10

p11 <- ggplot(data = bank, aes(x=poutcome, fill=y) ) + geom_bar() +
  xlab("poutcome") + ggtitle("Outcome of the previous campaign") + 
  theme(text = element_text(size = 12))
p11

p12 <- ggplot(data = bank, aes(x=balanceGroup, fill=y) ) + geom_bar() +
  xlab("balance") + ggtitle("Has balance loan?") + 
  theme(text = element_text(size = 12))
p12

p9 + p10 + p11 + p12 + plot_layout(guides = "collect")

# There are no Social and economic context attributes in this dataset
# Instead we have variables "balance" and "campaign" 

# Age Distribution vs Marital Status  OPTIONAL
p3 <- ggplot(bank, aes(x=age, fill=marital)) + 
  geom_histogram(binwidth = 2, alpha=0.7) +
  facet_grid(cols = vars(y)) +
  expand_limits(x=c(0,100)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  theme(text = element_text(size = 15)) +
  ggtitle("Age Distribution by Marital Status")
p3

# ======================================================
# Data Cleaning
# Remove the observations with unknown jobs
bank = bank %>% 
  filter(job != "unknown")

# Remove the observations with unknown education
bank = bank %>% 
  filter(education != "unknown")

# Remove the observations with unknown contact
bank = bank %>% 
  filter(contact != "unknown")

# There are too many "unknown" values in poutcome (75% of the sample size) => I won't remove them

# ========================================================
# Variable Selection

# Information Values
library("Information")
bank$y <- ifelse(bank$y == 'yes', 1, 0)
bank2 <- select(bank, -c("pdaysGroup","ageGroup","previousGroup",
                         "balanceGroup","campaignGroup"))
IV <- create_infotables(data = bank2, y = "y", bins = 5, parallel = FALSE)
IV$Summary

# Identify binary variables
is.binary <- function(v) {
  x <- unique(v)
  length(x) - sum(is.na(x)) == 2L
}
vapply(bank2, is.binary, logical(1))

# Variables with IV’s lower than 0.02 have no predictive power.
# So let's try to remove these variables (default, day) later

# =============================================================
# Check for missing values and duplicated rows again

bank3 <- select(bank2, -c("duration"))   # Remove col duration as suggested in Kaggle

# After removing variable duration, certain rows became identical hence need to be removed.
sum(duplicated(bank3))                   # Check for Duplicate Rows
dup_rows = subset(bank3,duplicated(bank3))
bank3 = bank3[-c(269,959,3062,8511,9127,9180,9201,9384,9410,9522,9539,10004,10479,10800),]
bank2 = bank2[-c(269,959,3062,8511,9127,9180,9201,9384,9410,9522,9539,10004,10479,10800),]
bank = bank[-c(269,959,3062,8511,9127,9180,9201,9384,9410,9522,9539,10004,10479,10800),]

# Verify no missing values
sapply(bank, function(x) sum(is.na(x)))      
summary(bank)

# Check the proportion of the target data
prop.table(table(bank$y))

# check data types
str(bank)
sapply(bank,class)

# Save
#write.csv(bank3,"bank_cleaned.csv", row.names = FALSE)
#write.csv(bank2,"bank_cleaned_with_Duration.csv", row.names = FALSE)

# ================================================
# Detect unusual data points from full model

bank = read.csv('bank_cleaned_with_Duration.csv')
str(bank)

# change the quantitative variables into numeric (do the same for bank2)
bank$age <- as.numeric(bank$age)
bank$campaign <- as.numeric(bank$campaign)
bank$pdays <- as.numeric(bank$pdays)
bank$previous <- as.numeric(bank$previous)
bank$balance <- as.numeric(bank$balance)
bank$day <- as.numeric(bank$day)
bank$y <- as.numeric(bank$y)
bank$pdaysGroup <- as.numeric(bank$pdaysGroup)

# Change chr variables into factor. A factor is how R deals categorical variables.
bank$job = factor(bank$job)
bank$marital = factor(bank$marital )
bank$education = factor(bank$education)
bank$default = factor(bank$default)
bank$housing = factor(bank$housing)
bank$loan = factor(bank$loan)
bank$contact = factor(bank$contact)
bank$month = factor(bank$month)
bank$poutcome = factor(bank$poutcome)

str(bank)

data = subset(bank, select = -c(ageGroup,pdaysGroup,previousGroup,balanceGroup,campaignGroup))

# Scale the numerical variables
data[c("age","balance","duration", "campaign","day", "pdays")] = scale(data[c("age","balance","duration", "campaign","day", "pdays")])

# Logistic Regression model on entire dataset
model.full = glm(y ~ ., family = binomial, data)
summary(model.full)   # Variables age, default, pdays, previous are not significant

data_noDuration = subset(data, select = -c(duration))
model.full2 = glm(y ~ ., family = binomial, data_noDuration)
summary(model.full2)  # Variables default, day, pdays, previous are not significant
                         
# Diagnostic Plot
par(mfrow = c(2,2))
plot(model.full)
plot(model.full2)

library(car)
#dev.off()
outlierTest(model.full)                             # Outlier = 11037 and 10997
plot(model.full, which = 4, cook.levels = 4/249)    # Influential point = 15853

outlierTest(model.full2)                            # Outlier = 5025
plot(model.full2, which = 4, cook.levels = 4/249)   # Influential point = 15853

# =================================================
# Remove insignificant variables
data = subset(data, select = -c(default,day,pdays,previous))
str(data)

# Split the dataset
samp = createDataPartition(data$month, p = 0.70, list = F)
train_set = data[samp,]
test_set = data[-samp,]

train_set2 = subset(train_set, select = -c(duration))
test_set2 = subset(test_set, select = -c(duration))

target = test_set$y
str(train_set)            # We should now only have numerical and factor variables 

# ==================================================
# Logistic Regression
train.fit1 = glm(y ~ ., family = binomial, data = train_set)
predicted1 = predict(train.fit1, type='response', newdata = test_set)   

train.fit2 = glm(y ~ ., family = binomial, data = train_set2)
predicted2 = predict(train.fit2, type='response', newdata = test_set2)

# find optimal cutoff probability to use to maximize accuracy
optimal1 <- optimalCutoff(target, predicted1)[1]
optimal2 <- optimalCutoff(target, predicted2)[1]

#create confusion matrix
eval1 = confusionMatrix(target, predicted1, threshold = optimal1)
eval2 = confusionMatrix(target, predicted2, threshold = optimal2)
eval1
eval2

# =================================================================
# Performance check with ROC

# Select model
check_predicted = predicted2
target = test_set$y
pred = prediction(check_predicted, target)

# Create an ROC curve
roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2, main="ROC Curve of Logistic Regression Model (without duration)", 
     cex.lab=1.5, cex.axis=3, cex.main=1.5)
abline(a = 0, b = 1) 

# AUC
auc = performance(pred, measure = "auc")
print(auc@y.values)

# ===========================================================
# 10-fold CV for logistic regresson models 
# produced exactly the same confusion matrices without 10-fold

library(boot)
set.seed(123)
cv_errors = data.frame(delta1 = 0, delta2 = 0)

for (i in 1:10){
  model_GLR = glm(y ~ ., family = binomial, data = train_set)
  cv_errors[i, ] = cv.glm(train_set, model_GLR, K=10)$delta
}
cv_errors                 # Each time you rerun the for loop, cv_errors will change

# delta1 = standard k-fold CV estimate
# delta2 = bias-corrected version

which(cv_errors[1] == min(cv_errors[1]))    

target = test_set$y
predicted_cv = predict(model_GLR, type='response', newdata = test_set)

optimal_cv <- optimalCutoff(target, predicted_cv)[1]                        
eval.cv = confusionMatrix(target, predicted_cv, threshold = optimal_cv)
eval.cv

# ==============================================================
# Decisiotn tree

set.seed(0)
tree1 = rpart(y ~ ., data = train_set, method = "class")
tree2 = rpart(y ~ ., data = train_set2, method = "class")

# tree1 = rpart(y ~ ., data = train_set, control = rpart.control(xval=100), method = "class")
# tree2 = rpart(y ~ ., data = train_set2, control = rpart.control(xval=100), method = "class")
# The 10-fold CV in this case doesn't improve the model at all. 
# We obtain the same confusion matrix whether we use CV or not. 

summary(tree1)
target = test_set$y

# predict test data by probability
pred.tree1 = predict(tree1, newdata = test_set, type = 'prob')
pred.tree2 = predict(tree2, newdata = test_set2, type = 'prob')

# find the threshold for prediction optimization with predicted probabilities
predictions <- data.frame(y = target, pred = NA)
predictions$pred <- pred.tree1[,2]
plot_pred_type_distribution(predictions,0.25)

# Create confusion matrix
pdata1 <- as.data.frame(pred.tree1)
head(pdata1)
pdata1$custom_pred <- ifelse(pdata1$`1` > 0.25, 1, 0)
table(pdata1$custom_pred, target)

pdata2 <- as.data.frame(pred.tree2)
head(pdata2)
pdata2$custom_pred <- ifelse(pdata2$`1` > 0.25, 1, 0)
table(pdata2$custom_pred, target)

# CV errors (handout page 112)
tree1$cptable
opt <- which.min(tree1$cptable[,"xerror"])
cp <- tree1$cptable[opt,"CP"]

tree2$cptable
opt <- which.min(tree2$cptable[,"xerror"])
cp <- tree2$cptable[opt,"CP"]

# With duration variable, there are 4 leaves
# Without duration variable, there are only 2 leaves 

# plot decision tree
prp(tree1, type = 5, extra = 106, fallen.leaves = TRUE, main="Decision Tree")
rpart.rules(select_model, extra = 4, cover = TRUE)

prp(tree2, type = 5, extra = 106, fallen.leaves = TRUE, main="Decision Tree")
rpart.rules(select_model, extra = 4, cover = TRUE)



