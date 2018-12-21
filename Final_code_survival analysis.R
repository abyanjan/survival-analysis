
# 1. Import Libraries ----

library(tidyverse)
library(dplyr)
library(lubridate)
library(car)
library(caret)
library(corrplot)
library(survival)
library(OneR)
library(survminer)

# Import Data 

LoanData <- read_csv("c:/Users/abyanjan/Downloads/LoanData (2).zip")


# 2. Data Preparation ----

#selecting only the required columns

survdata <- LoanData[c(1,2,10,12,13,16,19,21,23,24,25,27,28,29,32,34,36,49,65,68,79,85)]


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


# Remove loans before 2013

survival_data <- survival_data %>% 
  mutate(Year = year(LoanDate)) %>% 
  filter(Year > 2012 ) 

summary(survival_data)

# Removing observations with  0 values in EmploymentStatus variable and replacing -1 with 1 in 
# EmploymentStatus,UseofLoan and MaritalStatus

survival_data <- survival_data %>% 
  filter(EmploymentStatus != 0) %>% 
  mutate(EmploymentStatus = if_else(EmploymentStatus == -1, 1L, EmploymentStatus),
         UseOfLoan = if_else(UseOfLoan == -1, 1L, UseOfLoan),
         MaritalStatus = if_else(MaritalStatus == -1, 1L, MaritalStatus))

# checking for the missing values

colSums(is.na(survival_data))

# Remove NAs in Rating, Time and LossGivenDefault

survival_data <- survival_data %>% 
  drop_na(c(Rating,Time, LossGivenDefault))



# Binnig the Income to categorica variable

survival_data$IncomeTotal <- bin(survival_data$IncomeTotal, nbins = 3, 
                                 labels = c('low','middle','high'),
                                 method = 'content')


# Changing categorical variables to factors

survival_data <- survival_data %>% 
  mutate_at(vars(NewCreditCustomer, VerificationType, Gender, Country, UseOfLoan, MaritalStatus,
                 EmploymentStatus, Rating, State,Year), as.factor)


# 3. Kaplan-Meier test ----

# Kaplan-Meier overall survival curve

Kmcurve <- survfit(Surv(Time,Status)~ Rating, data = survival_data)
print(Kmcurve)
summary(Kmcurve)

# kaplan-meier survival curve plot

ggsurvplot(Kmcurve,
           pval = F, conf.int = TRUE, conf.int.style = 'step',
           risk.table = FALSE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           title = "Kaplan Meier Survival Curve",
           ggtheme = theme_minimal(),
           palette = "#GHI343",
           legend = 'bottom')



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
  
  # Prepare empty data frame to store the data
  evaluation_data <- data.frame(LoanId = character(0),Period = numeric(0), Actual = numeric(0),
                                Hazard = numeric(0),I = numeric(0), D = numeric(0), 
                                observed_profit = numeric(0))
  
  # convert annual Interest rates to monthly interest rate and express in decimal
  data$Interest = (data$Interest / 12) / 100
  
  # generate required data                             
  for (i in 1:nrow(data)){
  
    #generate time periods
    t <- seq(1, data$Time[i], by=1)
    
    #generate hazard probabilities
    Hazard <- rep(data$hazard[i],data$Time[i])
    
    # generate interest rates
    I <- rep(data$Interest[i], data$Time[i])
    
    # generate Loss given defaults
    D <- - rep(data$LossGivenDefault[i],data$Time[i])
    
    # generate actual status and observed profit
    
    if (data$Status[i] == 1) {
      actual_status <- c(rep(0, (data$Time[i]- 1)), 1)
      observed_profit <- c(rep(data$Interest[i], (data$Time[i]- 1)), - (data$LossGivenDefault[i]))
      
    } else {
      actual_status <- rep(0, data$Time[i])
      observed_profit <- rep(data$Interest[i], data$Time[i])
    }
    
    # create temporary data frame to store the generated data
    df <- data.frame(Period = t, Actual = actual_status, Hazard = Hazard, 
                     I = I, D = D,observed_profit = observed_profit)
    
    # add loan id
    df$LoanId = data$LoanId[i]
    
    evaluation_data <- rbind(evaluation_data, df)
  }
  return(evaluation_data)
}

# get train and test evaluation data for prediction accuracy and auc

# for train data
set.seed(234)

folds_train <- split(train_data, floor(seq(1,30, length.out = nrow(train_data))))

train_evaluation_data <- map_df(.x = folds_train, .f = get_evaluation_data)

# for test data
set.seed(345)

folds_test <- split(test_data, floor(seq(1,10, length.out = nrow(test_data))))

test_evaluation_data <- map_df(.x = folds_test, .f = get_evaluation_data)


# calculate mean square prediction error for test set

# assign ids to group by trai and test set

train_evaluation_data$id <- 'train'
test_evaluation_data $id <- 'test'

mse_evaluation <- train_evaluation_data %>% 
  
  # rowbind train and test evaluation data
  bind_rows(test_evaluation_data) %>% 
  
  # calculate  squared error of default prediction
  mutate(squared_error = (Actual - Hazard)^2) %>%
  
  # group by train and test data
  group_by(id) %>% 
  summarise(mse = mean(squared_error)) 



# calculate AUC for train and test set
library(pROC)

#train set
auc_train <- roc(response = train_evaluation_data$Actual, predictor = train_evaluation_data$Hazard)
auc_train
plot(auc_train, legacy.axes = TRUE, print.thres=TRUE, auc.polygon =TRUE,auc.polygon.col = 'lightblue')


auc_test <- roc(response = test_evaluation_data$Actual, predictor = test_evaluation_data$Hazard)
auc_test
plot(auc_test, legacy.axes = TRUE, print.thres=TRUE, auc.polygon =TRUE,auc.polygon.col = 'lightblue')


# 6. profit error calculation ----

profit_error <- train_evaluation_data %>% 
  
  #row bind train and test profit evaluation data
  bind_rows(test_evaluation_data) %>% 
  
  # calculate the expected profit
  mutate(expected_profit = ((1 - Hazard) * I)  + (Hazard * D)) %>% 
  
  #annual profit
  mutate(expected_profit_yearly = ((expected_profit+1)^12 - 1) * 100)


# square error of profit 
profit_error %>% 
  mutate(squared_error = (observed_profit - expected_profit)^2) %>% 
  
  # mean square error of profit for train and test set
  group_by(id) %>% 
  summarise(mse_profit = mean(squared_error))



# 7. profit estimation on test set

test_data <- test_data %>% 
  mutate (h = hazard,
          D = - LossGivenDefault,
          I = (Interest/12)/100) %>%
  mutate(profit = (h*(1+I)*(1+D)+(1-h)*(1+I))-1 ,
         expected_profit = ((profit+1)^12 - 1) * 100)


# histogram of the expected yearly profit and interest on test set
profit_estimation <- test_data %>% 
  select(Interest,hazard,Interest, expected_profit) %>% 
  rename(Predicted_expected_profit = expected_profit) %>% 
  gather(key = Type, value = Profit, -hazard) %>% 
  mutate(Type = as.factor(Type) %>% fct_relevel("Interest"))

library(tidyquant)
ggplot(profit_estimation, aes(x = Profit, col = 'red1'))+
  geom_histogram(fill = 'royalblue1',bins = 50, show.legend = F)+
  scale_x_continuous( breaks = seq(-100,200, by = 20))+ 
  facet_wrap(~Type, scales = 'free_x')+
  labs(title = 'Comparison of Predicted Expected profit and Interest rates on loans',
       x = 'Return in percentage')+theme_tq()
  

 # Residual plot of predicted profit
 profit_error %>% 
  # residual of profit 
  mutate(residual = (observed_profit - expected_profit)) %>% 
  ggplot(aes(x = expected_profit, y= residual))+geom_point() +
   geom_hline(yintercept =0, color = 'red')
  
 
# plot of interest and hazard
 
 test_data %>% 
  ggplot(aes( x = h , y = I))+
  geom_point(color='royalblue1', shape=21)+
  # add 0% profit line
  geom_line(aes(x = (I)/-(-0.9)*(1+I), y = I), col = 'black', size = 1)+
  # add 20 % profit line
  geom_line(aes(x = (I-20/12/100)/-(-0.9)*(1+I), y = I), col = 'green', size= 1)+
  #add -20% profit line
  geom_line(aes(x = (I+20/12/100)/-(-0.9)*(1+I), y = I), col = 'red', size=1)+
  labs(x ="hazard rates",
       y = "interest rates")
      

 # Distribution of Expected profit in the Ratings groups 
test_data %>% 
  ggplot(aes(x = fct_relevel(Rating, "AA"), y = expected_profit))+
  geom_boxplot(show.legend = FALSE)+
  scale_y_continuous(breaks = seq(-80, 100, by =15))+
  labs( x = 'Rating',
        y = 'Expected_Profit (%)')+
  theme_tq()
  
 

# 8. loan selection ----
   
# Selection of loans based on Ratings
 
selection_rating <- data.frame(LoanId = character(0),Rating = factor(), expected_profit = numeric(0),
                               selection = character(0))  


for (i in seq(0.1,1, by =0.1)){
  
  select_data <- test_data %>%
    mutate(Rating = fct_relevel(Rating,"AA")) %>% 
    arrange(Rating) %>% 
    select(LoanId,Rating, expected_profit) %>% 
    slice(1 : (i *nrow(test_data))) %>% 
    mutate(selection = paste0('top_', i*100 ))
  
  selection_rating <- bind_rows(selection_rating,select_data)
  
}
 
# join with test_evaluation data to get actual profits by selection _rating

selection_rating <- selection_rating %>% 
  left_join(test_evaluation_data, by = "LoanId") %>% 
  # summarise the mean profits in each quartile
  group_by(selection) %>% 
  summarise(Avg_Actual_profit = mean(observed_profit),
            Avg_Expected_profit = mean(expected_profit)) %>% 
  # convert avg_actual_profit from monthly to yearly
  mutate(Avg_Actual_profit = ((Avg_Actual_profit+1)^12 - 1) * 100)




# Selection of loans based on hazard rates

selection_hazard <- data.frame(LoanId = character(0),hazard = numeric(0),
                               expected_profit = numeric(0),selection = character(0))  

for (i in seq(0.1,1, by =0.1)){
  
  select_data <- test_data %>%
    arrange(hazard) %>% 
    select(LoanId,hazard,expected_profit) %>% 
    slice(1 : (i *nrow(test_data))) %>% 
    mutate(selection = paste0('top_', i*100 ))
  
  selection_hazard <- bind_rows(selection_hazard,select_data)
  
}
 
# join with test_evaluation data to get actual profits by selection_hazard

selection_hazard <- selection_hazard %>% 
  left_join(test_evaluation_data, by = "LoanId") %>% 
  # summarise the mean profits in each quartile
  group_by(selection) %>% 
  summarise(Avg_Actual_profit = mean(observed_profit),
            Avg_Expected_profit = mean(expected_profit)) %>% 
  # convert avg_actual_profit from monthly to yearly
  mutate(Avg_Actual_profit = ((Avg_Actual_profit+1)^12 - 1) * 100)


# Selection of loans based on expected profit

selection_profit <- data.frame(LoanId = character(0),Rating = factor(), 
                               expected_profit = numeric(0),selection = character(0))  

for (i in seq(0.1,1, by =0.1)){
  
  select_data <- test_data %>%
    arrange(desc(expected_profit)) %>% 
    select(LoanId,Rating, expected_profit) %>% 
    slice(1 : (i *nrow(test_data))) %>% 
    mutate(selection = paste0('top_', i*100 ))
  
  selection_profit<- bind_rows(selection_profit,select_data)
  
}


# join with test_evaluation data to get actual profits by selection_profits

selection_profit <- selection_profit %>% 
  left_join(test_evaluation_data, by = "LoanId") %>% 
  # summarise the mean profits in each quartile
  group_by(selection) %>% 
  summarise(Avg_Actual_profit = mean(observed_profit),
            Avg_Expected_profit = mean(expected_profit)) %>% 
  # convert avg_actual_profit from monthly to yearly
  mutate(Avg_Actual_profit = ((Avg_Actual_profit+1)^12 - 1) * 100)



# column bind selcetion_rating, selection_hazard and selection_profit

selection_combine <- bind_cols(selection_rating, selection_hazard, selection_profit) %>% 
  # select the required columns
  select(-selection1,-selection2) %>% 
  #rename columns
  rename(Actual_Return_Rating = Avg_Actual_profit,
         Actual_Return_Hazard = Avg_Actual_profit1,
         Actual_Return_Profits = Avg_Actual_profit2,
         Estimated_Return_Rating = Avg_Expected_profit,
         Estimated_Return_Hazard = Avg_Expected_profit1,
         Estimated_Return_Profits = Avg_Expected_profit2) %>% 
  gather(key = selection_type, value = Avg_return, - selection)

# plot the avg return based on selection criteria

selection_combine %>% 
  mutate(selection = as.factor(selection),
         selection = fct_relevel(selection, "top_100", after = 9)) %>% 
  mutate(selection_type = as.factor(selection_type),
         selection_type = fct_relevel(selection_type, "Actual_Return_Rating",
                                      "Actual_Return_Hazard","Actual_Return_Profits","Estimated_Return_Rating",
                                      "Estimated_Return_Hazard","Estimated_Return_Profits")) %>% 
  
  ggplot(aes(x = Avg_return, y = selection))+
  geom_point(size = 2)+
  scale_x_continuous(breaks = seq(0,40, by= 5))+
  geom_segment(aes(xend = 0, yend = selection))+
  facet_wrap(~ selection_type)+
  geom_label(aes(label = round(Avg_return,2),color = selection_type),
             size = 3,show.legend = F)+
  scale_color_brewer(type='div', palette = 'Dark2')+
  #scale_color_manual(values = c( "black","blue","brown3"))+
  theme_tq()+
  labs(title = "Average Predicted Expected Profit based on Different selection Criteria",
       x = "Aveage Profit (%)",
       y = "")+
  theme(title = element_text(size = 10))


