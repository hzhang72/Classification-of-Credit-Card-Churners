# Classification-of-Credit-Card-Churners

Situation: 
Credit card churning is a method of gaming bonus incentives offered by banks and credit card companies through the practice of repeatedly applying and closing credit card accounts and only meeting the minimum spending requirements to earn the bonus incentives. These incentives include cashback, airline miles, airline companion passes, hotel loyalty points, and hotel-free night certificates.  Credit card churners cost banks and companies millions of dollars in profit. As a result, many companies have created systems to detect credit card churners and blacklist them.

Task:
The goal of this study is to identify important factors that can accurately classify potential credit card churners.

Action:

1. Data Preprocessing 
1)	Utilizing Chi Squared Tests for categorical variables, ANOVA F-test for numeric variables, checks for multicollinearity by using vif() function in the R car package.
2)	Using the Best Subset Method by regsubsets() function in the R leaps package, we identified the best 7 predictor variables. 

2. Statistical Model 
1)	We used the rfcv() function in the R randomForest library to perform 5-fold cross-validation for feature selection.
2)	We used K-nearest neighbor (KNN), Linear Discriminant Analysis (LDA), Quadratic Discriminant Analysis (QDA), and Multinomial Logistic Regression (LR) classification methods to classify each observation in the test dataset. After many trials and based on the ROC curve, we consistently found that KNN was the best classification method using our model. 

Result:
Using the KNN model, we achieved a prediction accuracy of 0.84. The classifier made a total of 3000 predictions. Out of those cases, the classifier predicted 340 people will exit, and 2660 will not. In reality, 599 people exit, and 2401 did not. The Misclassification Rate is 15% in this prediction.
In conclusion, we found that women are more likely to be credit card churners than men and middle-aged customers are more likely to be credit card churners than younger and older aged customers.
