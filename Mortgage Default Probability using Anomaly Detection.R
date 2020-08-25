
#LOAD LIBRARIES AND IMPORT LOAN DEFAULT DATA

# install.packages('glmnet')
# install.packages('ROCR')
# install.packages("gains")
# install.packages('ggthemes')
# install.packages('gridExtra')
library(tidyverse)
library(ggplot2)
library(glmnet)
library(ROCR)
library(gains)
library(ggthemes)
library(gridExtra)
library(dplyr)

setwd('C:/myfiles/Anomaly/assignment2')
loandata=read.csv('XYZloan_default_selected_vars.csv')

str(loandata)
ncol(loandata) #number of columns
nrow(loandata) #number of rows

#Storing loandata in a Dataframe
df <- data.frame(loandata)
str(df)
ncol(df)

#INTITAL ANALYSIS AND CLEANSING OF DATASET 

#Finding columns with negative numbers
x <- stack(df); x$sign <- sign(x$values); table(x[-1])

#Finding columns with more than 99% values missing
which(colMeans(is.na(df)) > 0.98) #7 out of 89 columns have 99% values missing


#Replace Negative with 0 for numeric columns and removing columns with more than 99% values missing

cleanFun <- function(q){
  
  # set negative values as 0
  q[q < 0] <- 0
  
  # select numeric columns
  num_cols <- names(q)[sapply(q, is.numeric)]
  
  # get name of columns with 99% or more NA values
  col_to_remove <- names(q)[colMeans(is.na(q[num_cols]))>=0.98]
  
  # drop those columns
  return (q[setdiff(colnames(q),col_to_remove)])
}

df <- cleanFun(df)

ncol(df)  #number of columns after dropping variables with > 99% values missing: 82

#Removing Timestamp column and columns with > 99% blank values

df <- df[ , -which(names(df) %in% c("AP005","TD061","TD062"))]
str(df)

ncol(df)  #check the final number of columns before splitting the data: 79

#********************************Split data into train and test sets********************************
set.seed(10000)
split = sample(1:nrow(df),size = 0.7*nrow(df))
train = df[split,]
test = df[-split,]
nrow(train) #56000
nrow(test) #24000
nrow(df) #80000

#************************Dealing with missing values in Train and Test data************************

sum(!!colSums(is.na(train))) #Number of Columns in Train data containing at least one NA

#Using function to list the Names of Columns in Train data containing at least one NA
contains_any_na = sapply(train, function(x) any(is.na(x)))
names(train)[contains_any_na] 

#Using function to replace NAs in Train data with the median of the respective columns
for(i in 11:(ncol(train)-1)){
  train[is.na(train[,i]), i] <- median(train[,i], na.rm = TRUE)
}


sum(!!colSums(is.na(test))) #Number of Columns in Test data containing at least one NA

#Using function to list the Names of Columns in Test data containing at least one NA
contains_any_na = sapply(test, function(x) any(is.na(x)))
names(test)[contains_any_na] 

#Using function to replace NAs in Test data with the median of the respective columns
for(i in 11:(ncol(test)-1)){
  test[is.na(test[,i]), i] <- median(test[,i], na.rm = TRUE)
}

str(train)
str(test)

#*****************Initial Exploratory Data Analysis********************

#Examine correlations between loan_default and some primary variables of interest in Train data (e.g. 
cor(train$loan_default, train$AP001) #Age: -0.026
cor(train$loan_default, train$AP003) #Education: -0.089
cor(train$loan_default, train$AP004) #Loan term: 0.09 
cor(train$loan_default, train$AP007) #Applying City: 0.04
cor(train$loan_default, train$CR009) #Total loan amount: -0.02
cor(train$loan_default, train$CR017) #Debit card score_total: -0.05
cor(train$loan_default, train$CR018) #Debit card score_utility: -0.038

#We observe that, of all the variables of interest, Education (AP003) and Loan term (AP004) have the highest 
#correlations, negative and positive, respectively with Loan default and we may expect these attributes to be part of our final model.
#Total loan amount (CR009) has the least correlation with loan default.

#********************************Exploratory Data Analysis of selective variables*************************************

#Function to compute the frequency of variable and store it in a dataframe

getfreq <- function(data, col_name){
  data %>%
    group_by_(col_name) %>%
    summarise(X_counts = n(),
              Y_counts = sum(loan_default)) %>%
    mutate(   Y_by_X_pcnt = Y_counts/X_counts,
              X_pcnt = X_counts/sum(X_counts))
}

#Function to generate one-way frequency bar chart

one_two_way_plot <-function(df1,var){
  
  theme_set(theme_bw())
  
  # Draw plot
  p1 <- ggplot(df1, aes(x=get(var), y=X_pcnt, fill=get(var))) + 
    geom_bar(stat="identity", width=0.8) + 
    labs(title="One-way frequency barchart", 
         subtitle=var, 
         caption="source: data") + 
    coord_cartesian(ylim=c(0,1)) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  
  # Draw plot
  p2 <- ggplot(df1, aes(x=get(var), y=Y_by_X_pcnt, fill=get(var))) + 
    geom_bar(stat="identity", width=0.8) + 
    labs(title="% of Y by X", 
         subtitle=var, 
         caption="source: data") + 
    coord_cartesian(ylim=c(0,1)) +
    theme(axis.text.x = element_text(angle=65, vjust=0.6))
  
  require(gridExtra)
  grid.arrange(p1, p2, nrow=2,heights=c(1, 1))
}

# ****************1. Analysis of AP003 (Education)****************

df3 <- getfreq(train,"AP003")
df3
one_two_way_plot(df3,"AP003")

#Our inital correlation analysis implied that education had one of the strongest correlation with Loan default 
#and the plot reflects the same. Higher the education lower is the risk of default.

# ****************2. Analysis of AP004 (Loan term)****************

df4 <- getfreq(train,"AP004")
df4
one_two_way_plot(df4,"AP004")

#Also reflective of our initial correlation analysis, loan term being a variable with a very high positive correlation
#with Loan default, the plot shows the highest incidents of loan term are concentrated highly on the longest loan terms

# ****************3. Analysis of PA023 (Days b/w application & 1st collection call)****************

df23 <- getfreq(train,"PA023")
df23
one_two_way_plot(df23,"PA023")

#The plot shows some peak values on the higher side of the number of days between loan application and first 
#collection call. The first collection call is usually made when the loanee has prolonged his loan payment for a long time.
#Such loanees could fall under higher probability of defaulters

# ****************4. Analysis of AP008 (Level IP City)****************

df8 <- getfreq(train,"AP008")
df8
one_two_way_plot(df8,"AP008")

#The variable related to city was chosen to see if it had some relation to the loan default. By looking at the plot 
#IP City variable does not seem to have major relationship with loan default though it shows some variation. So, we 
#would leave to the variable selction methods to decide if it needs to be part of the final model.

# ****************5. Analysis of AP006 (OS type)****************

df6 <- getfreq(train,"AP006")
df6
one_two_way_plot(df6,"AP006")

#A specific OS type most defaulters use may hint at specific segments of people hailing from the same geography, culture,
#business, income type, etc. Interestingly, the plot suggests that people using h5 might have higher chances of being
#potential defaulters

# ****************6. Analysis of CR018 (Score debit card utility amt)****************

df18 <- getfreq(train,"CR018")
df18
one_two_way_plot(df18,"CR018")

#Utility debit card score is expected to be a very important attribute influencing the loan default as it takes into account
#factors similar to a FICO score which is an established variable in determining mortgage defaults in the industry. We do 
#see some variations in the plot which can be further analyzed by statistical feature selection methods.

# ****************7. Analysis of CR017 (Score debit card total amt)****************

df17 <- getfreq(train,"CR017")
df17
one_two_way_plot(df17,"CR017")

#Total debit card score is also expected to be a very important attribute influencing the loan default as it takes into account
#factors similar to a FICO score which is an established variable in determining mortgage defaults in the industry. We do 
#see some variations in the plot which can be further analyzed by statistical feature selection methods.

# ****************8. Analysis of AP007 (Level Application City)****************

df7 <- getfreq(train,"AP007")
df7
one_two_way_plot(df7,"AP007")

#The variable Application city was chosen to see if it had some relation to the loan default based on the city loan is 
#applied. By looking at the plot App City variable does not seem to have major relationship with loan default though it
#shows some variation. So, we would leave to the variable selction methods to decide if it needs to be part of the final model.

# ****************9. Analysis of PA022 (Days b/w application & High risk 1st collection call)****************

df22 <- getfreq(train,"PA022")
df22
one_two_way_plot(df22,"PA022")

#The High risk collection call is usually made when the loanee has prolonged his loan payment for a long time. Such 
#loanees could fall under higher probability of defaulters. The plot shows some significant peaks on the higher side of the 
#number of days between loan application and high risk collection call as expected. This could possibly be a significant variable.

# ****************10. Analysis of AP001 (Age)****************

df01 <- getfreq(train,"AP001")
df01
one_two_way_plot(df01,"AP001")

#There is a general stigma that people on the higher side of age may not have the capacity to repay loans. However, our 
#initial correlation analysis showed that age was not among the variables with high correlation with loan defaults. The plot
#shows a slightly high frequency at the late 20s number but we will need further feature selection analysis to check if Age
#considered to be a significant variable.

# ****************11. Analysis of CD160 (Count of Service calls_last 2 months)****************

df160 <- getfreq(train,"CD160")
df160
one_two_way_plot(df160,"CD160")

#The count of service calls in the last 2 months was chosen to see if there are any significant relation with the 
#loan default and the plot seems to convey a mixed result.

# ****************12. Analysis of CD162 (Count of Service calls_last 3 months)****************

df162 <- getfreq(train,"CD162")
df162
one_two_way_plot(df162,"CD162")

# ****************13. Analysis of CD164 (Count of Service outbound calls_last 3 months)****************

df164 <- getfreq(train,"CD164")
df164
one_two_way_plot(df164,"CD164")

#The count of service calls and service outbound calls in the last 3 months seem to show mixed results similar to 
#that in the last 2 months except for a few outlying values

# ****************14. Analysis of TD006 (Count of Small loan queries_last 1 month)****************

df006 <- getfreq(train,"TD006")
df006
one_two_way_plot(df006,"TD006")

# ****************15. Analysis of TD010 (Count of Small loan queries_last 3 months)****************

df010 <- getfreq(train,"TD010")
df010
one_two_way_plot(df010,"TD010")

#The last 2 variables, TD006 and TD010 were analyzed to see if the number of small loan queries in the last 1-3 months is 
#realted to the default trend as people may tend to query on more number of smaller loans to pay the interests for 
#their existing loans. The plot seems to suggest that % of defaults per count of small loan queries in the last 1-3
#months seems to be high at the higher range.


#***************************FEATURE SELECTION*****************

#Forward Stepwise variable selection
start_mod = lm(loan_default~1,data=train)
empty_mod = lm(loan_default~1,data=train)
full_mod = lm(loan_default~.,data=train)
forwardStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='forward')
summary(forwardStepwise)

#Backward Stepwise variable selection

start_mod = lm(loan_default~.,data=train)
empty_mod = lm(loan_default~1,data=train)
full_mod = lm(loan_default~.,data=train)
backwardStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='backward')
summary(backwardStepwise)

#Hybrid Stepwise regression

start_mod = lm(loan_default~1,data=train)
empty_mod = lm(loan_default~1,data=train)
full_mod = lm(loan_default~.,data=train)
hybridStepwise = step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='both')
summary(hybridStepwise)


hybridStepwisecoeff <- as.data.frame(summary(hybridStepwise)$coefficients)
backwardStepwisecoeff <- as.data.frame(summary(backwardStepwise)$coefficients)
forwardStepwisecoeff <- as.data.frame(summary(forwardStepwise)$coefficients)

#LASSO REGRESSION

x = model.matrix(loan_default~.,data=train)
y = train$loan_default


lassoModel = glmnet(x,y, alpha=1) #default for alpha is 1 which corresponds to Lasso
lassoModel
plot(lassoModel,xvar='lambda',label=T) #Coefficients vs Log Lambda
plot(lassoModel,xvar='dev',label=T) #Coefficients vs fraction deviation

cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso) #Mean-Squared Error vs log(Lambda)
coef(cv.lasso)

#Combining the results of Feature selection models of Forward, Backward and Hybrid Stepwise regression and Lasso regression, we perform
#Logistic regression on the 31 highly statistically significant variables as follows:


#LOGISTIC REGRESSION

#We use Logistic regression to create our final model as it is the most widespread technique, combining a high predictive capability with 
#accuracy percentages not statistically significant, different from other more recent techniques

logismodel = glm(loan_default~AP003+AP004+MB005+CR015+PA023+AP002+CD113+PA029+TD001+AP008+CD117+TD005+AP006+CD170+TD010+CR018+CD137+CD132+CD008+CD152+CR017+TD013+CR017+CD106+CD107+AP007+TD009+TD014+CR004+PA022+PA028+CD123,data=train,family='binomial')
summary(logismodel)
#Assess Performance on train sample
#Use predict function and construct classification table
pred = predict(logismodel,type='response')
ggplot(data=data.frame(pred),aes(x=pred))+
  geom_histogram(fill='steelblue3')

table(as.numeric(pred>0.5)) # how many predictions for loan default using a 0.5 cutoff
ct = table(train$loan_default,pred>0.5);ct # classification table
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy  #0.8054286

#We are interested in the Specificity which provides the ability to correctly identify the true defaulters of the lot

specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity #0.9901722
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity #0.04311607

#How much better is the accuracy than just going with majority class
t = table(train$loan_default)
baseline = max(t[1],t[2])/nrow(train); baseline #0.8049286

#Assess Performance (on test sample)
baseline = table(test$loan_default)[1]/nrow(test); baseline #0.8098333 

#compute prediction quality on the test sample using logismodel
pred_test = predict(logismodel,newdata=test,type='response')
ggplot(data=data.frame(pred_test),aes(x=pred_test))+
  geom_histogram(fill='steelblue3')
ct = table(test$loan_default,pred_test>0.5); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy #0.8086667
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity #0.9881663
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity #0.04425942



ROCRpred = prediction(pred_test,test$loan_default)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

## construct ROC plot
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,xlab="False Positive Rate",ylab="True Positive Rate") # basic plot with labeled axes

#For a perfect classifier the ROC curve will go straight up the Y axis and then along the X axis. In our case, it is
#somewhere in between indicating that the model has an intermediate performance power

test <- cbind(test, pred_test)
str(test)

#Gains table

#In the Gains table, Test data set is scored and sorted from high to low probability of default and put into equal segments 

gain.table <- with(test, gains(actual=loan_default, predicted=pred_test, optimal=TRUE, percents=TRUE))
gain.table

#Conclusion
#We proposed a regression logistic model to predict loan default as it is the most widespread technique, combining a 
#high predictive capability with accuracy percentages not statistically significant, different from other more recent techniques
#Education(AP003) and Loan Term (AP004) turned out to be the most predictive factors of default based on our intial 
#exploratory data analysis as well as the multiple variable selection methods and lasso regression, but other additional siginificant factors were 
# were also added to improve the accuracy of the model. The accuracy of our final logistic model was approximately 80%.
#Though Accuracy is a good metric for balanced classes, we further evaluated the model using Gains table.
#The gains table being model-agnostic, it can be used to compare the power of different models. 
#The Lift is the measure of effectiveness of our predictive model calculated as the ratio between the results obtained with and
#without our predictive model. Our current model has a Cumulative Lift of 2.09 which is a good index.