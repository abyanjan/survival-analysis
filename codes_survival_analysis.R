
#Importing libraries

library(tidyverse)
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
  filter(is.na(DefaultDate) == FALSE ) %>%
  mutate (Time = as.numeric( DefaultDate - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% # substracting 3 to make true survival period
  mutate(Status = 1,
         State="Default" )
  

# time period for paid up loans  
time_2 <- survdata %>% 
  filter(Status == 'Repaid' & is.na(DefaultDate)== T) %>% 
  mutate(Time = as.numeric( ContractEndDate - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% 
  mutate(Status = 0,
         State="Repaid")

#time period for the ongoing loans with status as current and no default dates
time_3 <- survdata %>%
  filter(Status =='Current'  & is.na(DefaultDate)== T) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% 
  mutate(Status =0, 
        State="Current")


#time period for the ongoing loans with status as late and no default date
time_4 <- survdata %>%
  filter(Status == 'Late' & is.na(DefaultDate)== T) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% 
  mutate(Status= 0,
         State="Current")


#combining the data sets having the survival times  removing unnecessary columns

survival_data <- bind_rows(time_1,time_2,time_3,time_4)


summary(survival_data)

#removing observations with  0 values in EmploymentStatus variable and replacing -1 with 1
survival_data <- survival_data %>% 
  filter(EmploymentStatus != 0) 
  
survival_data$EmploymentStatus <-  recode(survival_data$EmploymentStatus,'-1 = 1')

# replacing values -1 in UseofLoan by 1
survival_data$UseOfLoan <-  recode(survival_data$UseOfLoan,'-1 = 1')

# replacing values -1 in MaritalStatus by 1
survival_data$MaritalStatus <-  recode(survival_data$MaritalStatus,'-1 = 1')

summary(survival_data)
str(survival_data)


#Binnig the Income to categorica variable
library(OneR)

survival_data$IncomeTotal <- bin(survival_data$IncomeTotal, nbins = 3, 
                                 labels =    c('low','middle','high'),
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
survival_data <- survival_data[is.na(survival_data$Time)==FALSE, ]

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

library(plyr)
#changing survival time of 0 to 1

survival_data$Time <- mapvalues(survival_data$Time, from= 0, to=1)


exp_model <-survreg(Surv(Time, Status)~ NewCreditCustomer+VerificationType+Age+Gender+Country+
                      AppliedAmount+Interest+LoanDuration+UseOfLoan+MaritalStatus+EmploymentStatus+
                      IncomeTotal+Rating,
                    dist = "exponential", data = survival_data)
summary(exp_model)


#calculating hazard rates

hazard_rates <- 1/predict(exp_model, newdata = survival_data, type='response')

survival_prob <- exp(-hazard_rates)

Interest <- (survival_data$Interest/12)/100

#taking hazard and Interest for only non-missing value of loss given default

h <- hazard_rates[which(!is.na(survival_data$LossGivenDefault))]
I <- Interest[which(!is.na(survival_data$LossGivenDefault))]
D <- -(survival_data$LossGivenDefault[which(!is.na(survival_data$LossGivenDefault))])


Return_i <- (h*(1+I)*(1+D)+(1-h)*(1+I))-1

#yearly return 
Return_year <-(Return_i)*12*100

hist(Return_year, breaks = 50, col="lightgreen")





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




