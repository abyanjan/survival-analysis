# 1. Import the libraries and Data ----

library(tidyverse)
library(dplyr)
library(car)
library(caret)
library(corrplot)
library(survival)
library(OneR)
library(survminer)

# Import Data 

LoanData <- read_csv("Data/LoanData (2).zip")


# 2. Data Preparation ----

#selecting only the required columns

survdata <- LoanData[c(1,10,12,13,16,19,21,23,24,25,27,28,29,32,34,36,49,65,68,79,85)]


# calculating the time period of survival for each loans

# Changing the Status to binary : Default loans and repaid loans to 1 and current loans to 0.

# time period for the defaulted loans

time_1 <- survdata %>%
  filter(!is.na(DefaultDate)) %>%
  mutate (Time = as.numeric( DefaultDate - LoanDate),
          Time = round(Time/(365.25/12)),    
          Status = 1,
          State="Default") 


# time period for paid up loans  

time_2 <- survdata %>%
  filter(Status == 'Repaid' & is.na(DefaultDate)) %>%
  mutate (Time = as.numeric( ContractEndDate - LoanDate),
          Time = round(Time/(365.25/12)),
          Status = 0,
          State="Repaid")


# time period for the ongoing loans with status as current and no default dates
time_3 <- survdata %>%
  filter(Status == 'Current' & is.na(DefaultDate)) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate),
         Time = round(Time/(365.25/12)),
         Status =0, 
         State="Current")


# time period for the ongoing loans with status as late and no default date
time_4 <- survdata %>%
  filter(Status == 'Late' & is.na(DefaultDate)) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate),
         Time = round(Time/(365.25/12)),
         Status= 0,
         State="Current") 


# combining the data sets having the survival times  

survival_data <- bind_rows(time_1,time_2,time_3,time_4)

# Shuffle the data

survival_data <- survival_data[sample(1:nrow(survival_data)),]

summary(survival_data)

# Removing observations with  0 values in EmploymentStatus variable and replacing -1 with 1 in 
# EmploymentStatus,UseofLoan and MaritalStatus

survival_data <- survival_data %>% 
  filter(EmploymentStatus != 0) %>% 
  mutate(EmploymentStatus = if_else(EmploymentStatus == -1, 1L, EmploymentStatus),
         UseOfLoan = if_else(UseOfLoan == -1, 1L, UseOfLoan),
         MaritalStatus = if_else(MaritalStatus == -1, 1L, MaritalStatus))

# Binnig the Income to categorica variable

survival_data$IncomeTotal <- bin(survival_data$IncomeTotal, nbins = 3, 
                                 labels = c('low','middle','high'),
                                 method = 'content')

# checking for the missing values

colSums(is.na(survival_data))

# Replacing NAs in Rating as Missing

survival_data$Rating <- ifelse(is.na(survival_data$Rating), 'Missing', survival_data$Rating)

# Changing categorical variables to factors

survival_data <- survival_data %>% 
  mutate_at(vars(NewCreditCustomer, VerificationType, Gender, Country, UseOfLoan, MaritalStatus,
                 EmploymentStatus, IncomeTotal, Rating, State), as.factor)


# checking NAs
colSums(is.na(survival_data))

# Removing observations with NAs in Time variable
survival_data <- survival_data[!is.na(survival_data$Time),]


# 3. Kaplan-Meier test ----

# Kaplan-Meier overall survival curve

Kmcurve <- survfit(Surv(Time,Status)~ 1, data =survival_data)
print(Kmcurve)
summary(Kmcurve)

# kaplan-meier survival curve plot

ggsurvplot(Kmcurve,
           pval = TRUE, conf.int = TRUE, conf.int.style = 'step',
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           tables.height = 0.20,
           title = "Kaplan Meier Survival Curve",
           ggtheme = theme_survminer(),
           palette = "#FIE345")



# cumulative kaplan-meier survivla curve

ggsurvplot(Kmcurve,
           pval = TRUE, conf.int = TRUE, conf.int.style = 'step',
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           fun='cumhaz',
           title = "Cumulative Survival Curve",
           ggtheme = theme_survminer(),
           palette = "#FIE345")


# 4. Exponential survival model ----

# changing survival time of 0 to 1

survival_data$Time <- ifelse(survival_data$Time == 0,1, survival_data$Time)

# Partition data into train and test set in the ratio 80:20

set.seed(123)

index <- createDataPartition(survival_data$Status, p = 0.8, list = FALSE)

train_data <- survival_data[index, ]
test_data <- survival_data[-index, ]


# Train the exponential model on trai data

exp_model <-survreg(Surv(Time, Status)~ NewCreditCustomer+VerificationType+Age+Gender+Country+
                      AppliedAmount+Interest+LoanDuration+UseOfLoan+MaritalStatus+EmploymentStatus+
                      IncomeTotal+Rating,
                    dist = "exponential", data = train_data)
summary(exp_model)


# Predict hazard rate on train data

train_data$hazard  <-  1/predict(exp_model,newdata = train_data, type='response')

# predict hazard rates on test data

test_data$hazard  <- 1/predict(exp_model,newdata = test_data, type='response')



# 5. Evaluate the model with Prediction accuracy and Auc ----

# Define function to prepare the data for the evaluation

get_evaluation_data <- function(data){
  
  # filter data with missing values in LossGivenDefault
  data <- data[which(!is.na(data$LossGivenDefault)),]
  
  # Prepare empty data frame to store the data
  evaluation_data <- data.frame(Period = numeric(0), Actual = numeric(0),
                                Prediction = numeric(0))
  
  # generate required data                             
  for (i in 1:nrow(data)){
    
    #generate hazard probabilities
    hazard_prob <- rep(data$hazard[i],data$Time[i])
    
    #generate time periods
    t <- seq(1, data$Time[i], by=1)
    
    #generate survival probabilities
    surv_prob <- exp(- hazard_prob * t)
    
    # generate actual status
    
    if (data$Status[i] == 1) {
      actual_status <- c(rep(1, (data$Time[i]- 1)), 0)
      
    } else {
      actual_status <- rep(1, data$Time[i])
    }
    
    # create temporary data frame to store the generated data
    df <- data.frame(Period = t, Actual = actual_status, Prediction = surv_prob)
    
    evaluation_data <- rbind(evaluation_data, df)
  }
  return(evaluation_data)
}

# get train and test evaluation data for prediction accuracy and auc

# for train data

folds_train <- split(train_data, floor(seq(1,20, length.out = nrow(train_data))))

train_evaluation_data <- map_df(.x = folds_train, .f = get_evaluation_data)

# for test data

folds_test <- split(test_data, floor(seq(1,5, length.out = nrow(test_data))))

test_evaluation_data <- map_df(.x = folds_test, .f = get_evaluation_data)

# calculate mean square prediction error for test set

train_evaluation_data$id <- 'train'
test_evaluation_data $id <- 'test'

mse_test <- train_evaluation_data %>% 
  bind_rows(test_evaluation_data) %>% 
  mutate(squared_error = (Actual - Prediction)^2) %>% 
  group_by(id) %>% 
  summarise(mse = mean(squared_error)) 
 


# calculate AUC for test set
library(pROC)

auc_test <- roc(response = test_evaluation_data$Actual, predictor = test_evaluation_data$Prediction)
auc_test
plot(auc_test, legacy.axes = TRUE, print.thres=TRUE, auc.polygon =TRUE,auc.polygon.col = 'lightblue')


# 6. Profit error calculation ----

# function to generate Interest rates,loss given default and observed profit for calculating profit error

get_profit_data <- function(data){
  
  # filter data with missing values in LossGivenDefault
  data <- data[which(!is.na(data$LossGivenDefault)),]
  
  # convert annual Interest rates to monthly interest rate and express in decimal
  data$Interest = (data$Interest / 12) / 100
  
  # Prepare empty data frame to store the data
  profit_data <- data.frame(observed_profit = numeric(0),I = numeric(0), D = numeric(0))
  
  # generate required data                             
  for (i in 1:nrow(data)){
    
    # generate Interest/ Observed-Profits
    
    if (data$Status[i] == 1) {
      observed_profit <- c(rep(data$Interest[i], (data$Time[i]- 1)), - (data$LossGivenDefault[i]))
      
    } else {
      observed_profit <- rep(data$Interest[i], data$Time[i])
    }
    
    # generate interest rates
    
    I <- rep(data$Interest[i], data$Time[i])
    
    # generate Loss given defaults
    
    D <- - rep(data$LossGivenDefault[i],data$Time[i])
    
    
    # create temporary data frame to store the generated data
    df <- data.frame(observed_profit = observed_profit, I = I, D = D)
    
    profit_data <- rbind(profit_data, df)
  }
  return(profit_data)
}


# profit evaluation data for train set

train_profit_evaluation <- map_df(.x = folds_train, .f = get_profit_data)

# combine train_profit_evaluation  with train_evaluation data column wise

train_profit_evaluation <- bind_cols(train_evaluation_data, train_profit_evaluation)

#  profit evaluation data for test set

test_profit_evaluation <- map_df(.x = folds_test, .f = get_profit_data)

# combine train_profit_evaluation  with train_evaluation data column wise

test_profit_evaluation <- bind_cols(test_evaluation_data, test_profit_evaluation)
  

# profit error calculation

profit_error <- train_profit_evaluation %>% 
  
  #row bind train and test profit evaluation data
  bind_rows(test_profit_evaluation) %>% 
  
  # calculate the expected profit
  mutate(expected_profit = (Prediction * I) + ((1-Prediction) * D)) %>% 
  
  # square error of profit 
  mutate(squared_error = (expected_profit - observed_profit)^2) %>% 
  
  # mean square error of profit for train and test set
  group_by(id) %>% 
  summarise(mse_profit = mean(squared_error))
  



