
##################### REGRESSION ANALYSIS #############################

## Regression (literally means relationship)is a statistical method 
## for computing relationships between a response variable & 
## an independent variable.
## we model the data according to the structure of the variable we want
## to predict & compute reln. witht he help of several indep. var's

## Since,
## Correlation doesn't necesarily mean causation
## We calculate regression coeff. & interpret those coeff. to see
## the reln. between the variables.



############################# ASSUMPTIONS #############################

## A MODEL IS BOUND TO FOLLOW THESE ASSUMP.
## IF NOT WE CANNOT FIR THE REGRN.

## Regrn Model must be Linear-In-Parameters (Beta's with power == 1)

## Indept. Var = Non Stochastic in nature.....KNOWN IN ADVANCE

## Mean of Error Terms  == 0

## Variance of Error Term == Constant (Sigma ^ 2)
## HOMOSCEDASTICITY (Homo = equal , scedastic = variance)
## No funnel shape


## The Error Term follows Ui ~ ND(0 , Sig ^ 2)

## Covariance B/W Indept. & Error Term == 0 (Uncorrelated)

## For (2) different Indept var's the corresp. Error Terms are Uncorrelated
## Assump of No - AUTO CORRELATION

## No EXACT LINEAR RELN. B/W two Indept. Var's
## Assump of No - MULTICOLLINEARITY



####################### BANKING CASE STUDY #######################

# Business Problem : : To predict the banking loan default
# Business Solution: : To build Logistic Regrn model

# The data consists of 1000 loan applications
# The default var. is binary & or target var., 
# Wherein, 1 = Default & 0 = No-Default

# There are == (70 %) Defaulters
# Out of all == (30 %) Non - Defaulters

## WE're going to use a logistic regrn. to predict the bank loan default 
## in this case study  : : 

getwd()

# Loading the dataset
Loan_Data <- read.csv("E:/R Programming/Data Analysis using R CASE STUDIES/Dataset/Chapter_02_loan_default_dataset.csv", 
                      header = T, sep = ",")

# Converting the table form to a data frame
Loan_Data_DF <- data.frame(Loan_Data)

# Performing EDA

library(dplyr)

# Both of the fn's below help us see the structure of the data
glimpse(Loan_Data_DF)
str(Loan_Data_DF)

# Dim helps us see the dimensions (Rows * Columns) of a dataset
dim(Loan_Data_DF)

# For all of the variable names
names(Loan_Data_DF)
## The first variable is of our concern

# For first & last 3 values across all variables
head(Loan_Data_DF, n = 3)
tail(Loan_Data_DF, n = 3)

# To see descriptive stats for Age
summary(Loan_Data_DF$Age)

# Wanna know if there are any missing values in the dataset?
sum(is.na(Loan_Data_DF))
##### THERE ARE NO N/A VALUES PRESENT IN OUR DATASET

# Had any N/A values present in the data, we would had to impute them
# With No missing values present, we can proceed with EDA

# To see the corrl. among var.'s

options(scipen = 999)
# This helps expand miniscule figures & removes (e) from a number..

attach(Loan_Data_DF)
cor <- cor.test(Default, Term, method = "pearson")
cor
## It suggests that Corrl != 0 & = +0.34, so there is a reln. between them
## There's a (+ve) correlation bw them
## AS Term increases, the default is also going to increase
detach(Loan_Data_DF)

###### SEEING PATTERNS IN GRAPHS
library(ggplot2)
glimpse(Loan_Data_DF)
names(Loan_Data_DF)

Amount_CR <- ggplot(Loan_Data_DF, aes(Amount, Credit_score)) + 
                      geom_point(aes(colour = Default))
Amount_CR
## It shows a scatter plot of Amount & Cr.Score with 
## colour coding of Default
## Inferences :
## Almost all people with a score of < 600 appear to be 
## defaulting on a loan.


Amount_Age <- ggplot(Loan_Data_DF, aes(Age, Amount)) + 
                     geom_histogram(stat = "identity" , fill = "red", 
                                    binwidth = 0.1)
Amount_Age
## It shows a histogram between Age & Amount of Loan Applied.
## Inferences : People in the age range [25 - 35]  apply
## for Loan Amounts > $50,000



######## BUILDING THE LOGISTIC MODEL 

# USING GLM
Full_Model <- glm(Default ~ ., data = Loan_Data_DF, 
                  family = binomial(link = logit))

summary(Full_Model)

## SINCE OUR HYPOTHESIS : 
## H0 : Desired Variable has no sigf. impact on Default ,    against
## Ha : Desired Variable has sigf. impact on Default

## We know that for all p-values <= Alpha (0.05), WE REJECT THE H0
## We know that for all p-values > Alpha (0.05), WE ACCEPT THE H0

## Our results show that, ONLY
## Checking_Amount,  TERM, CREDIT_SCORE,  Saving_Amount,  & AGE
## HAVE SIGF. IMPACT ON THE TARGET VAR. == DEFAULT


## SO, WE HAVE TO RUN THE MODEL AGAIN BY REMOVING THE INSIGF. VAR'S
REDUCED_MODEL_A <- glm(Default ~ Checking_amount + Term +
                                 Credit_score + Saving_amount + Age, 
                       data = Loan_Data_DF, family = binomial(link = logit))

summary(REDUCED_MODEL_A)
## WE CAN SEE THAT OUR MODEL HAS ALL THE APPROPRAITE PREDICTOR VAR'S
## WHICH HAVE A SIGF. IMPACT ON THE TARGET VARIABLE
## ALL OF THE VAR'S ARE STATISTICALLY SIGF. AS ALL HAVE P-VALUE <= 0.05

## FOR INFERENCES ::

## CHECKING AMOUNT:
## WE SAY, FOR A ONE-UNIT INCREASE IN THE CHECKING AMOUNT, THE LOG ODDS
## OF DEFAULTING VS NON-DEFAULTING DECREASE BY (-0.004).
## MOREOVER, IT SHOWS THAT THERE'S A (-VE) RELN. BETWEEN PREDICTOR & RESP. VAR.
## IF CHECKING AMOUNT IS HIGH, THE PROBL. OF BANK DEFAULT DECREASES & VICE-VERSA

## TERM:
## WE SAY, FOR A ONE-UNIT INCREASE IN THE TERM, THE LOG ODDS
## OF DEFAULTING VS NON-DEFAULTING INCREASE BY (+0.174).
## MOREOVER, IT SHOWS THAT THERE'S A (+VE) RELN. BETWEEN PREDICTOR & RESP. VAR.
## IF THE TERM INCREASES, THE PROBL. OF BANK DEFAULT ALSO INCREASES & VICE-VERSA

## SIMILARILY FOR THE OTHER VAR'S


####### FOR VALIDATION PURPOSES & TO CHECK THE PREDICTIVE ACCRUACY
####### WE NOW SPLIT THE DATA INTO TRAINING & TEST DATASETS
####### WE HAVE TAKEN THE SPLITTING RATIO = TRAINING (70 %)
#######                                     TEST     (30 %)

?floor
# This fn. is useful in rounding the figures to the nearest largest integer
# So, a figure = 75.84 becomes figure = 76

Rounding_Obs <- floor(0.7*nrow(Loan_Data_DF))
Rounding_Obs
## Since, there are 1000 obs. we have split the data in 70 : 30 ratio
## So, 70 % i.e 700 obs. go to the TRAINING MODEL OF THE DATASET

?seq_len
## It shows the sequence of the length of obs. in a data

set.seed(2)
Loan_Train_Index <- sample(seq_len(nrow(Loan_Data_DF)), size = Rounding_Obs)

Loan_Test_Index <- -Loan_Train_Index

######## Getting the no. of obs
Loan_Train_Data <- Loan_Data_DF[Loan_Train_Index,]
Loan_Test_Data <- Loan_Data_DF[-Loan_Train_Index,]


####### Building Logistic Model using Train Data
set.seed(2)
Train_Model <- glm(Default ~ ., data = Loan_Train_Data,
                   family = binomial(link = logit))
summary(Train_Model)

## AGAIN REPEATING THE PROCESS OF ELIMINATING INSIGF. VAR'S
## & RERUNNING THE MODEL WITH REDUCED VAR'S, WE GET

Reduced_Train_Model <- glm(Default ~  Checking_amount + Term + 
                                      Credit_score + Saving_amount + Age, 
                           data = Loan_Train_Data,
                           family = binomial(link = logit))
summary(Reduced_Train_Model)

######### AGAIN , 
## WE CAN SEE THAT OUR MODEL HAS ALL THE APPROPRAITE PREDICTOR VAR'S
## WHICH HAVE A SIGF. IMPACT ON THE TARGET VARIABLE
## ALL OF THE VAR'S ARE STATISTICALLY SIGF. AS ALL HAVE P-VALUE <= 0.05

## FOR INFERENCES ::

## CHECKING AMOUNT:
## WE SAY, FOR A ONE-UNIT INCREASE IN THE CHECKING AMOUNT, THE LOG ODDS
## OF DEFAULTING VS NON-DEFAULTING DECREASE BY (-0.004).
## MOREOVER, IT SHOWS THAT THERE'S A (-VE) RELN. BETWEEN PREDICTOR & RESP. VAR.
## IF CHECKING AMOUNT IS HIGH, THE PROBL. OF BANK DEFAULT DECREASES & VICE-VERSA

## TERM:
## WE SAY, FOR A ONE-UNIT INCREASE IN THE TERM, THE LOG ODDS
## OF DEFAULTING VS NON-DEFAULTING INCREASE BY (+0.147).
## MOREOVER, IT SHOWS THAT THERE'S A (+VE) RELN. BETWEEN PREDICTOR & RESP. VAR.
## IF THE TERM INCREASES, THE PROBL. OF BANK DEFAULT ALSO INCREASES & VICE-VERSA

## SIMILARILY FOR THE OTHER VAR'S


####### NOW , 
####### FOR CHECKING WHETHER THERE"S MULTICOLLINEARITY PRESENT IN INDEPT 
####### VARIABLES OF THE TRAIN DATA
####### WE HAVE A SPECIAL TEST CALLED == V.I.F (Variance Inflation Factor)

install.packages("car")
library(car)

vif(Reduced_Train_Model)
## For var's with VIF > 5 , we say that they show multicollinearity patterns
## Since, no var's in our model have a VIF > 5 , all of em are good to go.
############THERE"S NO MUTICOLLINEARITY IN OUR DATA.


########## USING THE PREDICT FN.
########## WE are going to set a cut-off value for the probl. & Binary Value
########## For all Binary Values == 1 & Probl > 70 % 
########## we say that there's a higher chance of DEFAULTS ON A LOAN

Default_Probl <- predict(Reduced_Train_Model, Loan_Test_Data,
                         type = "response")

########## Now, setting a fn. to generate 1 / 0 according to threshold

Default_Pred <- factor(Default_Probl > 0.7, levels = c(TRUE, FALSE),
                       labels = c("1 - Default", "0 - No-Default"))
View(Default_Pred)

Default_Results <- ifelse(Default_Probl > 0.70, "Default", "No-Default")

########## FOR VALIDATING THE PREDICTIVE VALUES,
########## WE USE CONFUSION MATRIX & ROC CURVES
########## This is done to see how accurately the model is predicted

######### CONFUSION MATRIX

Confusion_Matrix <- table(Default_Pred, Loan_Test_Data$Default, 
                      dnn = c("Predicted", "Actual"))
Confusion_Matrix

## DEFAULT    = +ve class
## No-DEFAULT = -ve class

## ACTUAL ( 1 ) = +ve class
## ACTUAL ( 0 ) = -ve class

## INTERPRETATION : :

############### TRUE NEGATIVE ###############
## When the ACTUAL claim was that person is Not-Defaulting ( 0 )
## & the PREDICTED claim was that person is Not-Defaulting.
## So, for both of these claims belong to the -ve class
## We say, that its a    "TRUE NEGATIVE"    case..
## The value corresponding to T.N case 
## in our Confusion Matrix == 193. 

############### FALSE NEGATIVE ###############
## When the ACTUAL claim was that person is Defaulting ( 1 )
## & the PREDICTED claim was that person is Not-Defaulting.
## So, the ACTUAL claim belongs to ( +ve ) class
## However, PREDICTED claim belongs to the -ve class
## We say, that its a    "FALSE NEGATIVE"    case..
## The value corresponding to F.N case 
## in our Confusion Matrix == 19.
############### THIS IS TYPE - II ERROR ###############


############### FALSE POSITIVE ###############
## When the ACTUAL claim was that person is Not-Defaulting ( 0 )
## & the PREDICTED claim was that person is Defaulting.
## So, the ACTUAL claim belongs to ( -ve ) class
## However, PREDICTED claim belongs to the +ve class
## We say, that its a    "FALSE POSITIVE"    case..
## The value corresponding to F.P case 
## in our Confusion Matrix == 3.
############### THIS IS TYPE - I ERROR ###############


############### TRUE POSITIVE ###############
## When the ACTUAL claim was that person is Defaulting ( 1 )
## & the PREDICTED claim was that person is Defaulting.
## So, both of these claims belong to the +ve class
## We say, that its a    "TRUE POSITIVE"    case..
## The value corresponding to T.P case 
## in our Confusion Matrix == 85. 


######## CONFUSION MATRIX statistics can be calculated using:

################# CODE IS TAKEN FROM R-In-ACTION BOOK ###################

Confusion_Matrix_Perf <- function(table, n=2){
  if(!all(dim(table) == c(2,2)))
    stop("Must be a 2 x 2 table")
  tn = table[2,1]
  fp = table[1,1]
  fn = table[2,2]
  tp = table[1,2]
  sensitivity = tp/(tp+fn)
  false_positive_rate = fp/(fp+tn)
  specificity = tn/(tn+fp)
  Precision = tp/(tp+fp)
  Inaccuracy = tn/(tn+fn)
  Misclassification = (fp+fn)/(tp+tn+fp+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  result <- paste("Sensitivity (TPR) = ", round(sensitivity, n) ,
                  "\nFalsy_Rate (FPR) = ", round(false_positive_rate, n),
                  "\nSpecificity = ", round(specificity, n),
                  "\nPrecision = ", round(Precision, n),
                  "\nInaccuracy = ", round(Inaccuracy, n),
                  "\nMiclassification_Rate = ", round(Misclassification, n),
                  "\nAccuracy = ", round(hitrate, n), "\n", sep="")
  cat(result)
}

Confusion_Matrix_Perf(Confusion_Matrix)


##################################
class(Default_Pred)
table(Default_Pred)
unique(Default_Pred)

Loan_Test_Data$Default <- as.factor(Loan_Test_Data$Default)
View(Loan_Test_Data$Default)

class(Loan_Test_Data$Default)
table(Loan_Test_Data$Default)
unique(Loan_Test_Data$Default)



########### VISUALIZING USING ROC & AUC
library(ROCR)
library(caret)
# COMPUTING AUC with our models
Probl <- predict(Reduced_Train_Model, newdata = Loan_Test_Data, type = "response")
Pred <- prediction(Probl, Loan_Test_Data$Default)

Pre_AUC <- performance(Pred, measure = "tpr", x.measure = "fpr")
plot(Pre_AUC, col = "red")
## SINCE THE LINE IS CLOSE TO 1.0, we say the model is fit accurately
## Because the curve approaching toward 1 represents the 
## best performance of the model.

AUC <- performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC

## Area under Curve (AUC) values > 70% are considered 
## the model with high predictive accuracy. 

## In our case study of logistic regression model,
## Area under curve (AUC) value  = 98 %, which is considered 
## an absolute accurate model.


########################## THANK YOU ####################################