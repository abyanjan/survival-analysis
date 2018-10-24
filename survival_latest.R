library(readr)
library(dplyr)
library(lubridate)

Loan_data <- read_csv("C:/Users/abyanjan/Downloads/LoanData (2).zip")
repayment_data <- read_csv("C:/Users/abyanjan/Downloads/RepaymentsData (3).zip")


#selecting columns from Loan_data
Loan_data <- Loan_data[,c(1,2,10,12,13,16,19,21,23,24,25,27,28,29,32,34,36,49,68,79,85,93,94,97,98)]
Loan_data <-  rename( Loan_data,loan_id = LoanId)

#converting dates from characters to date format
#Loan_data$ReportAsOfEOD <- dmy(Loan_data$ReportAsOfEOD)

#Loan_data <- Loan_data%>%  mutate_at(vars(contains("Date")),dmy)  


#seperate late, repaid and current loans

##repaid loans
repaid_loans <- filter(Loan_data, Status == "Repaid")

#calculate survival time for repaid loans
anyNA(repaid_loans$ContractEndDate)

#replace missing contract end date with maturity date
repaid_loans$ContractEndDate[which(is.na(repaid_loans$ContractEndDate))] <- repaid_loans$MaturityDate_Last[which(is.na(repaid_loans$ContractEndDate))]

#survival time
repaid_loans <- repaid_loans %>%
  mutate(Time = as.numeric( ContractEndDate - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12)))


##current loans
current_loans <- filter(Loan_data,Status == "Current")

#calculate survival time for repaid loans

current_loans <- current_loans %>%
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12)))


##late loans
late_loans <- filter(Loan_data,Status == "Late")

#late loans could be default or current 
#late loans that do not have default dates are current loans

late_loans_current <- filter(late_loans, is.na(DefaultDate)== T)

#late loans that have default date could be default or current based on their repayment data

late_loans_default <- anti_join(late_loans,late_loans_current,by="loan_id")

#finding status of late loans that have default date
#left join late loans with repayment data

late_loans_repay <- left_join(late_loans_default,repayment_data, by="loan_id")
#late_loans_repay <- mutate(late_loans_repay,Date=dmy(Date))


#grouping late_loans_repay by loan_id
late_loans_repay$loan_id <- factor(late_loans_repay$loan_id)

late_repay_group <- late_loans_repay %>% 
  group_by(loan_id) %>% 
  summarise(latest_payment = max(Date),
            ReportAsOfEOD = max(ReportAsOfEOD.x),
            DefaultDate = max(DefaultDate),
            LoanDate =max(LoanDate)) %>% 
  mutate(compare_date = latest_payment+days(60))



#identifying late loans as default or current based on their latest payment
late_repay_group <- mutate(late_repay_group,Status=ifelse(compare_date > ReportAsOfEOD,"Current","Default"))

#converting loan_id as character
late_repay_group$loan_id <- as.character(late_repay_group$loan_id)

#combining late_repay group back to late_loans_default to get new status
late_loans_default <- left_join(late_loans_default,select(late_repay_group,loan_id,Status),by="loan_id") %>% 
  mutate(Status.x=NULL) %>% 
  rename(Status = Status.y)

#adding data frame late_loans_default with current status to late_loans_current
late_loans_current <- rbind(late_loans_current,filter(late_loans_default,Status=="Current"))

#adding survival time for late-current loans
late_loans_current <- late_loans_current %>% 
  mutate(Status = "Current") %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12)))

#taking only loans with default status from late_loans_default
late_loans_default <- filter(late_loans_default,Status!="Current")

#adding survival time for late-default loans
late_loans_default <- late_loans_default %>% 
  mutate(Status = "Default") %>% 
  mutate(Time = as.numeric( DefaultDate - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12)))


#finally add all the loan data sets to get a single loan data
survival_data <- rbind(repaid_loans,current_loans,late_loans_current,late_loans_default)

summary(survival_data)


write.csv(survival_data,"survival_data.csv",row.names = F)





#checking whether the default date matches the actual default(2 consecutive missed payments)

library(tidyverse)

Loan_data <- read_csv("C:/Users/abyanjan/Downloads/LoanData (2).zip")
repayment_data <- read_csv("C:/Users/abyanjan/Downloads/RepaymentsData (3).zip")

#data of default loans only
default_data <- Loan_data %>% 
  select(LoanId,DefaultDate) %>% 
  filter(!is.na(DefaultDate))

#loan ids of default loans
default_id <- default_data$LoanId

#repayment data for the default loans
repayment_default <- repayment_data %>% 
  filter(loan_id %in% default_id) %>% 
  select(loan_id,Date) %>% 
  rename("LoanId" = loan_id) 
  

length(unique(repayment_default$LoanId))


#join default_data and repayment_data
join_data <- default_data %>% 
  left_join(repayment_default,by="LoanId")

# arranging join data by loanid and date
join_data <- join_data %>% 
  arrange(LoanId,Date) %>% 
  select(LoanId,Date,DefaultDate) %>% 
#remove NAs from Date 
  filter(!is.na(Date))


#creating a column to calculate the diff in days between last payment before default and default date
join_data <- join_data %>% 
  mutate(diff_def_pay = DefaultDate - Date) 

#filtering out the data for payment dates passed default date
join_data <- join_data %>% 
 filter(!(Date >= DefaultDate))


#find the closet value to 60 days difference in default date and last payment before default date for each loan id

closest_day <-join_data %>% 
  group_by(LoanId) %>% 
  summarize(nearest_day = min(abs(diff_def_pay))) %>% 
  mutate(nearest_day =as.numeric(nearest_day))

#histogram of the days between default date and the earliest date before default data

hist(closest_day$nearest_day,breaks = 100, xlab = "Days",main = "Days between the default date and last payment 
     date  before default date", col="lightblue")
boxplot(closest_day$nearest_day, main="Days between the default date and payment 
     date just before default data", col= "yellow" )
summary(closest_day$nearest_day)


#ggplot(closest_day, aes(nearest_day))+geom_histogram(binwidth = 20)


df1 <- filter(closest_day, nearest_day < 10 )


df<- filter(repayment_default, LoanId %in% df1$LoanId) %>% 
  arrange(LoanId,Date) %>% 
  left_join(Loan_data[,c("LoanId","DefaultDate","Status")],by="LoanId") %>% 
  left_join(closest_day,by="LoanId")



d <- join_data %>% 
  filter(LoanId =="932B0F92-8B44-499F-A056-00C6D6D1E312") %>% 
  mutate(Date2 = c(Date[2:length(Date)],ymd("20181004")),
         diff =Date2-Date) %>% 
  select(-diff_def_pay)


#schedule data

schedule_data <- read_csv("C:/Users/abyanjan/Downloads/LoanSchedules (1).zip")

schedule_data <- schedule_data %>% 
  filter(loan_id %in% join_data$LoanId,
         Date <= ReportAsOfEOD) %>% 
  arrange(loan_id,Date) %>% 
  rename("LoanId"=loan_id)
  
Repaid <- Loan_data %>% 
  filter(Status=="Repaid") %>% 
  select(LoanId,DefaultDate, Status) %>% 
  filter(is.na(DefaultDate)) %>% 
  rename("loan_id"=LoanId) %>% 
  left_join(repayment_data,by="loan_id") %>% 
  arrange(loan_id,Date) %>% 
  select(loan_id,Date)

Ex <- Repaid %>% 
  filter(loan_id == "5D91E0CC-61ED-40BD-B855-A2DF017E619A")%>% 
  mutate(Date2 = c(Date[2:length(Date)],ymd("20181004")),
         diff =Date2-Date)





## not correct ###

#left join repayment_data and loan_data
new_data <- left_join(Loan_data,repayment_data,by="loan_id")
 
#arranging the columns
new_data <- new_data %>% select(1:3,7,9,10,11,everything())



#converting dates from characters to date format
new_data$ReportAsOfEOD.y <- dmy(new_data$ReportAsOfEOD.y)

new_data <- new_data%>%  mutate_at(vars(contains("Date")),dmy)  



#separating data with different status

#repaid loans
repaid_loans <- new_data %>% filter(Status == "Repaid")

#replacing missing contract end dates with maturity date
repaid_loans$ContractEndDate[which(is.na(repaid_loans$ContractEndDate))] <- repaid_loans$MaturityDate_Last[which(is.na(repaid_loans$ContractEndDate))]

#survival time for repaid loans
repaid_loans <- repaid_loans %>%
  mutate(Time = as.numeric( ContractEndDate - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12)))



#current loans
current_loans <-  new_data %>% filter(Status == "Current")

#survival time for current loans
current_loans <-current_loans %>%  
  mutate(Time = as.numeric( ReportAsOfEOD.y - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12)))


#late loans
late_loans <- new_data %>% filter(Status == "Late")
late_loans <- arrange(late_loans,loan_id,Date)

#late loans with no default date
late_loans_current <- filter(late_loans,is.na(DefaultDate) == T)

#survival time for late loans with no default dates
late_loans_current <- late_loans_current %>% 
  mutate(Time = as.numeric( ReportAsOfEOD.y - LoanDate)) %>% 
  mutate(Time= round(Time/(365.25/12))) %>%
  mutate(Status = "Current")

#late loans with default dates
late_loans_default <- filter(late_loans,is.na(DefaultDate) == F)


#calculating the latest payment date

#converting loan_id to factor for groupby analysis
late_loans_default$loan_id <- factor(late_loans_default$loan_id)

summary_data <- late_loans_default %>% 
  group_by(loan_id) %>% 
  summarise(latest_payment = max(Date),
            ReportAsOfEOD = max(ReportAsOfEOD.y),
            DefaultDate = max(DefaultDate),
            LoanDate =max(LoanDate)) %>% 
  mutate(compare_date = latest_payment+days(90))

summary_data$loan_id <- as.character(summary_data$loan_id)

#Status for the late default loans
summary_data <- mutate(summary_data, Status = ifelse (compare_date > ReportAsOfEOD,"Current","Default"))

#seperating default and current loans from the summary data
summary_data_current <- filter(summary_data, Status=="Current")
summary_data_default <- filter(summary_data, Status=="Default")

#survival times for late default and current loans

#for current loans
summary_data_current  <- summary_data_current %>%
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate))%>% 
  mutate(Time= round(Time/(365.25/12)))

#for default loans
summary_data_default  <- summary_data_default %>%
  mutate(Time = as.numeric( DefaultDate - LoanDate))%>% 
  mutate(Time= round(Time/(365.25/12)))

#combine two summary data of late default loans to a single data
summary_late_default <- rbind(summary_data_current,summary_data_default)


#converting loan_id back to character
late_loans_default$loan_id <- as.character(late_loans_default$loan_id)

#Adding new status and survival time to late_loans_default
late_loans_default<- select(late_loans_default, -Status)
late_loans_default <- left_join(late_loans_default,summary_late_default[,c("loan_id","Status","Time")],
                                by="loan_id")

#combining late_loans_current and late_loans_default to form new late_loans data
late_loans <- rbind(late_loans_current,late_loans_default)

#combining repaid, late and current loans data to get payment data
payment_data <- rbind(repaid_loans,current_loans,late_loans)

 
#getting status and time only from payment data for each loan id
payment_data$loan_id <- factor(payment_data$loan_id)

pay_sub_data <- payment_data %>% group_by(loan_id) %>% 
  summarise(Status=unique(Status), Time=max(Time))

pay_sub_data$loan_id <- as.character(pay_sub_data$loan_id)

#joining with Loan_data to get new status and time
survival_data <- left_join(pay_sub_data,Loan_data,by="loan_id")

#removing status.y and rename status.x to status
survival_data <- survival_data %>% select(-Status.y) %>% rename(Status =Status.x)


#save data
write.csv(survival_data,"survival_data.csv",row.names = F)
write.csv(payment_data,"payment_data.csv",row.names = F)





#auc calculation
library(tidyverse)
library(dplyr)
library(car)
library(caret)
library(corrplot)
library(survival)
library(OneR)


#Import data
LoanData <- read_csv("C:/Users/abyanjan/Downloads/LoanData (2).zip")

#Data preprocessing
#selecting only the required columns

survdata <- LoanData[c(1,2,10,12,13,16,19,21,23,24,25,27,28,29,32,34,36,49,65,68,79,85,93,94)]


#calculating the time period of survival for each loans

#Changing the Status to binary : Default loans and repaid loans to 1 and current loans to 0.

# time period for the defaulted loans

time_1 <- survdata %>%
  filter(!is.na(DefaultDate)) %>%
  mutate (Time = as.numeric( DefaultDate - LoanDate),
          Time = round(Time/(365.25/12)) - 2,    # substracting 2 to make true survival period
          Status = 1,
          State="Default") 


# time period for paid up loans  

time_2 <- survdata %>%
  filter(Status == 'Repaid' & is.na(DefaultDate)) %>%
  mutate (Time = as.numeric( ContractEndDate - LoanDate),
          Time = round(Time/(365.25/12)),
          Status = 0,
          State="Repaid")



#time period for the ongoing loans with status as current and no default dates
time_3 <- survdata %>%
  filter(Status == 'Current' & is.na(DefaultDate)) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate),
         Time = round(Time/(365.25/12)),
         Status =0, 
         State="Current")


#time period for the ongoing loans with status as late and no default date
time_4 <- survdata %>%
  filter(Status == 'Late' & is.na(DefaultDate)) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate),
         Time = round(Time/(365.25/12)),
         Status= 0,
         State="Current") 


#combining the data sets having the survival times  removing unnecessary column

survival_data <- bind_rows(time_1,time_2,time_3,time_4)


summary(survival_data)

#removing observations with  0 values in EmploymentStatus variable and replacing -1 with 1 in 
#EmploymentStatus,UseofLoan and MaritalStatus

survival_data <- survival_data %>% 
  filter(EmploymentStatus != 0) %>% 
  mutate(EmploymentStatus = if_else(EmploymentStatus == -1, 1L, EmploymentStatus),
         UseOfLoan = if_else(UseOfLoan == -1, 1L, UseOfLoan),
         MaritalStatus = if_else(MaritalStatus == -1, 1L, MaritalStatus))

summary(survival_data)
str(survival_data)


#Binnig the Income to categorica variable

survival_data$IncomeTotal <- bin(survival_data$IncomeTotal, nbins = 3, 
                                 labels = c('low','middle','high'),
                                 method = 'content')

colSums(is.na(survival_data))

#Filling NAs in Rating as Missing

survival_data$Rating <- ifelse(is.na(survival_data$Rating)== T, 'Missing',survival_data$Rating)

#Changing categorical variables to factors
survival_data$VerificationType <- factor(survival_data$VerificationType,
                                         levels = c('1','2','3','4'),
                                         labels = c('Income unverified','cross-referenced by phone',
                                                    'Income verified','Income and expenses verified'))
survival_data$Gender <- factor(survival_data$Gender,
                               levels = c('0','1','2'),
                               labels = c('Male','Female','Undefined'))

survival_data$Country <- factor(survival_data$Country,
                                levels = c('EE','ES','FI','SK'),
                                labels = c('Estonia','Spain','Finland','Slovakia'))


survival_data$UseOfLoan <- factor(survival_data$UseOfLoan,
                                  levels = c('0','1','2','3','4','5','6','7','8'),
                                  labels = c('Loan consolidation','Real estate','Home improvement',
                                             'Business','Education','Travel','Vehicle','Other','Health'))


survival_data$MaritalStatus <- factor(survival_data$MaritalStatus,
                                      levels = c('1','2','3','4','5'),
                                      labels = c('Married','Cohabitant','Single','Divorced','Widow'))


survival_data$EmploymentStatus <- factor(survival_data$EmploymentStatus,
                                         levels = c('1','2','3','4','5','6'),
                                         labels = c(' Unemployed', 'Partially employed','Fully employed','Self-employed',
                                                    'Entrepreneur','Retiree'))
survival_data$NewCreditCustomer <- factor(survival_data$NewCreditCustomer,
                                          levels = c('False','True'),
                                          labels = c('No','Yes'))


survival_data$Rating <- as.factor(survival_data$Rating)

levels(survival_data$Rating)

#checking NAs
colSums(is.na(survival_data))

#Removing observations with NAs in Time variable
survival_data <- survival_data[!is.na(survival_data$Time),]

colSums(is.na(survival_data))


#Kaplan-Meier overall survival curve

Kmcurve <- survfit(Surv(Time,Status)~ 1, data =survival_data)
print(Kmcurve)
summary(Kmcurve)$table


library(survminer)
ggsurvplot(Kmcurve,
           pval = TRUE, conf.int = TRUE, conf.int.style = 'step',
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           title = "Kaplan Meier Survival Curve",
           ggtheme = theme_bw(),
           color = "darkblue")

#cumulative survivla curve
ggsurvplot(Kmcurve,
           pval = TRUE, conf.int = TRUE, conf.int.style = 'step',
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           fun='cumhaz',
           title = "Cumulative Survival Curve",
           ggtheme = theme_bw(),
           color = "darkblue")

#Cox model

res.cox <- coxph(Surv(Time, Status) ~ NewCreditCustomer+VerificationType+Age+Gender+Country+
                   AppliedAmount+Interest+LoanDuration+UseOfLoan+MaritalStatus+EmploymentStatus+
                   IncomeTotal+Rating,
                 data = survival_data)


summary(res.cox)

#plot the cox survivall curve
ggsurvplot(survfit(res.cox), data=train,color = "red",
           ggtheme = theme_minimal())

#cumulative hazard curve from cox model
ggsurvplot(survfit(res.cox), data = train,
           pval = TRUE, conf.int = TRUE, conf.int.style = 'step',
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           fun='cumhaz',
           title = "Cumulative Survival Curve",
           ggtheme = theme_bw(),
           color = "darkblue")



#exponential model 

#changing survival time of 0 to 1

survival_data$Time <- ifelse(survival_data$Time == 0,1, survival_data$Time)


exp_model <-survreg(Surv(Time, Status)~ NewCreditCustomer+VerificationType+Age+Gender+Country+
                      AppliedAmount+Interest+LoanDuration+UseOfLoan+MaritalStatus+EmploymentStatus+
                      IncomeTotal+Rating,
                    dist = "exponential", data = survival_data)
summary(exp_model)

#function to get hazard rates, survival rates and return

Return <- function(model,data) {
  
  data <- data[which(!is.na(data$LossGivenDefault)),]
  h <- 1/predict(model, newdata = data, type='response')
  survival_prob <- exp(-h)
  D <- -(data$LossGivenDefault)
  I <- (data$Interest/12)/100
  
  profit <- (h*(1+I)*(1+D)+(1-h)*(1+I))-1
  profit_yearly <- (profit)*12*100
  
  return(data.frame(hazard = h, survival=survival_prob,return=profit_yearly)) 
  
}

#Return on the full data

full_return <-Return(model= exp_model,data = survival_data)
hist(full_return$return,breaks = 50,col="lightgreen")


#exponential model with train and test data

#Divide train and test sets

train_index <- createDataPartition(survival_data$Status, p=0.8, list = FALSE)

train_data <- survival_data[train_index,]
test_data <- survival_data[-train_index,]

#exponential model for train data

exp_train <- survreg(Surv(Time, Status)~ NewCreditCustomer+VerificationType+Age+Gender+Country+
                       AppliedAmount+Interest+LoanDuration+UseOfLoan+MaritalStatus+EmploymentStatus+
                       IncomeTotal+Rating,
                     dist = "exponential", data = train_data)
summary(exp_train)


#calculating hazard rates on train data

train_return <- Return(exp_train,train_data)
hist(train_return$return,breaks = 50, col="lightgreen")

#calculate return for test data
test_return <- Return(exp_train,test_data)

hist(test_return$return, breaks = 50, col="lightgreen")



library(lubridate)

#mean return in each year for test data

return_each_year <- test_data %>% 
  filter(!is.na(LossGivenDefault)) %>% 
  mutate(Return = test_return$return,
         Year = year(LoanDate)) %>% 
  group_by(Year) %>% 
  summarize(mean_return = mean(Return))

#plot of mean return each year
ggplot(return_each_year,aes(x=Year,y=mean_return))+geom_col(width= 0.5,fill="green",alpha=0.4)+
  scale_x_continuous(limits = range(2008:2019),breaks=c(2009:2018))+
  scale_y_continuous(labels = function(x) paste(x,"%"))+
  geom_text(aes(label= paste(round(mean_return,2),"%")),size=2.5,vjust= 0.1)+
  coord_flip ()+ 
  theme_minimal()


#AUC calculation
test_auc_data <- test_data[which(!is.na(test_data$LossGivenDefault)),]
test_auc_data$survival <- test_return$survival
test_auc_data$hazard <- test_return$hazard


#define function to get data for auc calculation

get_auc_data <- function(data){
  
  auc_data <- data.frame(Hazard = numeric(0),Survival = numeric(0),Actual=numeric(0))
  
  for (i in 1:nrow(data)){
    
    hazard_prob <- rep(data$hazard[i],data$Time[i])
    surv_prob <- rep(data$survival[i],data$Time[i])
    
    if (data$State == "Default") {
      actual_status <- c(rep(0,(data$Time[i]-1)),1)
    } else {
      actual_status <- rep(0,(data$Time[i]))
    }
    
    df <- data.frame(Hazard = hazard_prob, Survival = surv_prob, Actual = actual_status)
    
    auc_data <- rbind(auc_data,df)
  }
  return(auc_data)
}

auc_data <- get_auc_data(test_auc_data)

#auc
library(pROC)

auc <- roc(response = auc_data$Actual, predictor = auc_data$Survival)
auc
plot(auc)




