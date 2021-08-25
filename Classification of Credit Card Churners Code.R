## ***Library***
library(leaps)
library(MASS)
library(corrplot)
library(dplyr)
library(tidyverse)
library(caret)
library(car)
library(magrittr)
library(purrr)
library(kableExtra)
library(randomForest)

## ***Load Data***
data <-read.csv("finaldata1.csv")
data <- subset(data,select=-c(RowNumber, Surname, CustomerId)) # useless variables (counts, ids and names)
all(!is.na(data)) # check if any missing values include / it shows no missing values here
all(!is.null(data)) # Also shows no blank here. 

## ***Prepossessing***

### ***Type Changes***

#######################################################################################################
#### In order to see the obvious difference, we converted the CreditScore (numeric) into category. ####
#######################################################################################################
data$CreditScore[data$CreditScore>=300 & data$CreditScore<=579] <- "low"
data$CreditScore[data$CreditScore>=580 & data$CreditScore<=669] <- "med"
data$CreditScore[data$CreditScore>=670 & data$CreditScore<=850] <- "good"

#######################################################################################################
########## Based on the reality, we only considered whether the accounts are balance or not. ##########
#######################################################################################################
data$Balance[data$Balance == 0] <- "0"
data$Balance[data$Balance > 0] <- "1"

#######################################################################################################
## Since the variables below actually shows true or false, we converted them into categories first. ###
#######################################################################################################
data$HasCrCard[data$HasCrCard == 1] <- "1"
data$HasCrCard[data$HasCrCard == 0] <- "0"
data$IsActiveMember[data$IsActiveMember == 1] <- "1"
data$IsActiveMember[data$IsActiveMember == 0] <- "0"

### ***Cross Validation***
set.seed(214890)
Index <- createDataPartition(data$Exited, p = 0.7, list = FALSE)
train <- data[Index,]
test <- data[-Index,]

### ***Feature Selection: categorical variables***

######## separate the categorical and the numeric variables ########
cat_pre <- train[, sapply(train, mode) == "character"]

######################  Pearson’s chi-squared ######################
################## Assumption: linear correlation ##################
####################################################################
prob <- 0.05
store <- rep(NA, ncol(cat_pre))
index <- rep(NA, ncol(cat_pre))
for(i in seq_len(ncol(cat_pre))){
  store[i] <- chisq.test(table(train[,11], cat_pre[,i]))$p.value
  if(store[i] < prob){
    index[i] <- TRUE
  }else{
    index[i] <- FALSE
  }
}
index # Only one of them are significant, which is "HasCrCard". 

###################### Table for basic information ######################
table1 <- data.frame("Category.pred" = colnames(cat_pre),"P-value" = store, "Reject" = index)
kable(table1, booktabs = T) %>% kable_styling(latex_options = "striped")

### ***Feature Selection: numeric variables***

######## separate the categorical and the numeric variables ########
num_pre <- train[, sapply(train, mode) == "numeric" & colnames(train) != "Exited"]

############################ Correlations ############################
################## significance test & collinearity ##################
######################################################################

################### check collinearity ###############################
cor(num_pre) # It shows there is no collinearity between these variables. 

######################### significance tests #########################
store2 <- rep(NA, ncol(num_pre))
index2 <- rep(NA, ncol(num_pre))
for(i in seq_len(ncol(num_pre))){
  store2[i] <- summary(aov(train[,11]~num_pre[,i]))[[1]][["Pr(>F)"]][[1]]
  if(store2[i] < 0.05) {
    index2[i] <- TRUE
  }else{
    index2[i] <- FALSE
  }
}
index2 # Two of them are not significant, which are "Tenure" and "EstimatedSalary".

###################### Table for basic information ######################
table2 <- data.frame("Numeric.pred" = colnames(num_pre),"P-value" = store2, "Reject" = index2)
kable(table2, booktabs = T) %>% kable_styling(latex_options = "striped")

### ***Rearrange Potential Predictors***
delate <- c("HasCrCard", "Tenure", "EstimatedSalary")
newtrain <- train[, -which(colnames(train) %in% delate)]
dim(newtrain)
head(newtrain)

### ***Another Feature Selection: Bestsubset method (double checking)***

###################### Bestsubset with three methods ######################
bestsub <- regsubsets(train$Exited~., data = train, nvmax = 11,
                      method = "exhaustive", really.big = FALSE)

forward_sel <- regsubsets(train$Exited~., data = train, nvmax = 11,
                          intercept = TRUE, method = "forward",
                          really.big = FALSE)

backward_sel <- regsubsets(train$Exited~., data = train, nvmax = 11,
                           intercept = TRUE, method = "backward",
                           really.big = FALSE)

###################### Checking the results ######################
res.sum <- summary(bestsub)
res.sum1 <- summary(forward_sel)
res.sum2 <- summary(backward_sel)
df_bestsubset <- cbind(data.frame("method" = c("exhaustive","forward","backward"),
                                  rbind(data.frame(Adj.R2 = which.max(res.sum$adjr2),
                                                   CP = which.min(res.sum$cp),
                                                   BIC = which.min(res.sum$bic)),
                                        data.frame(Adj.R2 = which.max(res.sum1$adjr2),
                                                   CP = which.min(res.sum1$cp),
                                                   BIC = which.min(res.sum1$bic)),
                                        data.frame(Adj.R2 = which.max(res.sum2$adjr2),
                                                   CP = which.min(res.sum2$cp),
                                                   BIC = which.min(res.sum2$bic)))))
# we get same results of adj R2, CP, BIC from 3 selections
# variables CreditScore, Geography, Gender, Age, Balance, NumOfProducts, IsActiveMember, Exited need to keep
# same reslut like what we did with chisq.test and anova-f-test

###################### Table for basic information ######################
kable(df_bestsubset, booktabs = T) %>% kable_styling(latex_options = "striped")

### **Classification**

# since the bestsubset shows the same results, we just used the newtrain. 

# convert the outcome from numeric variable into category first
newtrain$Exited[newtrain$Exited == 1] <- "exit" 
newtrain$Exited[newtrain$Exited == 0] <- "not"
test$Exited[test$Exited==0] <- "not"
test$Exited[test$Exited==1] <- "exit"

set.seed(9097412)
train_control <- trainControl(method="cv", number = 5, classProbs = TRUE, savePredictions = TRUE)

######################## LR method ########################
LRfit <- train(Exited~., data = newtrain, trControl = train_control, method="glm", family = "binomial")
predLR <- predict(LRfit, newdata = test)
confusionMatrix(data = predLR, reference = as.factor(test$Exited))
summary(LRfit)
LRfit
######################## LDA method ########################
LDAfit <- train(Exited~., data = newtrain,
                method = "lda",preProc = c("center", "scale"), trControl = train_control)
predLDA <- predict(LDAfit, newdata = test) 
confusionMatrix(data = predLDA, reference = as.factor(test$Exited))
LDAfit
######################## QDA method ########################
QDAfit <- train(Exited~., data = newtrain,
                method = "qda",preProc = c("center", "scale"), trControl = train_control)
predQDA <- predict(QDAfit, newdata = test)
confusionMatrix(data = predQDA, reference = as.factor(test$Exited))
QDAfit
######################## KNN method ########################
KNNfit <- train(Exited~., data = newtrain,
                method = 'knn',preProc = c("center", "scale"),trControl = train_control,
                tuneGrid = expand.grid(k = seq(1, 50, by = 5)))
predKNN <- predict(KNNfit, newdata = test)
confusionMatrix(data = predKNN, reference = as.factor(test$Exited))
KNNfit #knn performs best

###################### Table for basic information ######################
table3 <- data.frame("method" = c("LR","LDA","QDA","KNN"),
                     "Accuracy" = c(0.8102859, 0.8089994, 0.8325749, 0.8398571), 
                     "kappa" = c(0.2372941, 0.2493197, 0.3948674, 0.3965521))
kable(table3, booktabs = T) %>% kable_styling(latex_options = "striped")

library(MLeval)
res <- evalm(list(LRfit,LDAfit,QDAfit,KNNfit), gnames = c('LR','LDA','QDA','KNN'))
res$roc

### Add interaction term to the final model KNN
KNNfit.it <- train(Exited~. + CreditScore * Gender, data = newtrain,
                   method = 'knn',preProc = c("center", "scale"),trControl = train_control,
                   tuneGrid = expand.grid(k = seq(1, 50, by = 5)))
predKNN.it <- predict(KNNfit.it, newdata = test)
confusionMatrix(data = predKNN.it, reference = as.factor(test$Exited))
KNNfit.it

glm <- glm(Exited ~ . + CreditScore*Gender, data = newtrain, family = "binomial")
summary(glm)

interaction.plot(x.factor = newtrain$CreditScore,    # variable to plot on x-axis
                 trace.factor = newtrain$Gender, # variable to specify "traces"
                 response = newtrain$Exited,    # variable to plot on y-axis
                 type = "l", 
                 ylim = c(0, 1),
                 ylab = "Exited (0 is no and 1 is yes)",
                 xlab = "Credit Score",
                 col = c("blue4", "red4"),
                 lty = 1,  # line type
                 lwd = 2,  # line width
                 trace.label = "Gender",  # label for legend
                 xpd = FALSE,
                 main = "Interaction plot of Credit Score and Gender as Exited predictors")