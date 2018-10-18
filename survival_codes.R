



library(readr)
library(tidyverse)
library(car)
library(caret)
library(corrplot)
library(survival)
library(OneR)

#Import data
LoanData <- read_csv("C:/Users/abyanjan/Downloads/LoanData.zip")

#Data preprocessing
#selecting only the required columns

survdata <- LoanData[c(1,2,10,12,13,16,19,21,23,24,25,27,28,29,32,34,36,49,68,79,85,93,94)]

#calculate time period required to recover the investment
#survdata$recover_time <- round(survdata$AppliedAmount/survdata$MonthlyPayment)

#changing the status(loans having default date as default:1 and rest as non default:0)
#survdata$Status <- ifelse(is.na (survdata$DefaultDate) == TRUE, 0,1)
#prop.table(table(survdata$Status))


#calculating the time period of survival for each loans

#Changing the Status to binary : Default loans and repaid loans to 1 and current loans to 0.

# time period for the defaulted loans

time_1 <- survdata %>%
  filter(is.na(DefaultDate) == FALSE ) %>%
  mutate (Time = as.numeric( DefaultDate - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% # substracting 3 to make true survival period
  mutate(Status = 1) %>% 
  mutate(State = "Default")

summary(time_1$Time)
#time_1$LoanId[time_1$Time == 61]

# time period for paid up loans  
time_2 <- survdata %>% 
  filter(Status == 'Repaid' & is.na(DefaultDate)== T) %>% 
  mutate(Time = as.numeric( ContractEndDate - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% 
  mutate(Status = 0) %>% 
  mutate(State = "Repaid")

#time period for the ongoing loans with status as current and no default dates
time_3 <- survdata %>%
  filter(Status =='Current'  & is.na(DefaultDate)== T) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% 
  mutate(Status =0) %>% 
  mutate(State = "Current")


#time period for the ongoing loans with status as late and no default date
time_4 <- survdata %>%
  filter(Status == 'Late' & is.na(DefaultDate)== T) %>% 
  mutate(Time = as.numeric( ReportAsOfEOD - LoanDate)) %>% 
  mutate(Time = round(Time/(365.25/12))) %>% 
  mutate(Status= 0) %>% 
  mutate(State = "Current")


#combining the data sets having the survival times  removing unnecessary columns

survival_data <- bind_rows(time_1,time_2,time_3,time_4) 


#removing unnecessary columns
survival_data <-   select(survival_data, -c(1,4,5,6,19,22,23))

summary(survival_data)


#removing observations with  0 values in EmploymentStatus variable and replacing -1 with 1
survival_data <- survival_data %>% 
  filter(EmploymentStatus != 0) 
 
survival_data$EmploymentStatus <-  recode(survival_data$EmploymentStatus,'-1 = 1')

summary(survival_data)

# replacing values -1 in UseofLoan by 1
survival_data$UseOfLoan <-  recode(survival_data$UseOfLoan,'-1 = 1')

# replacing values -1 in MaritalStatus by 1
survival_data$MaritalStatus <-  recode(survival_data$MaritalStatus,'-1 = 1')


summary(survival_data)
str(survival_data)

#boxplot of Income
boxplot(survival_data$IncomeTotal)

#plot between age and income
ggplot(survival_data, aes(Age, IncomeTotal))+geom_point()

#Binnig the Income to categorica variable
library(OneR)

survival_data$IncomeTotal <- bin(survival_data$IncomeTotal, nbins = 3, labels = c('low','middle','high'),
                                 method = 'content')

summary(survival_data$IncomeTotal)

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

summary(survival_data)
colSums(is.na(survival_data))

#Removing observations with NAs in Time variable
survival_data <- survival_data[is.na(survival_data$Time)==FALSE, ]

colSums(is.na(survival_data))

# calculate monthly payments using Amortization formula

r <- (survival_data$Interest/12)/100
n <- survival_data$LoanDuration
A <- survival_data$AppliedAmount

survival_data$monthly_pay <-  A*((r*(1+r)^n)/((1+r)^n-1))
survival_data$diff_monthlypay <- survival_data$MonthlyPayment - monthly_pay

#checking for near zero variance

nz <- nearZeroVar(survival_data)
nz

#Checking for coliniarity
corelation <- cor(survival_data[,c(3,6,7,8,13)])
corrplot(corelation, method=c('circle'))


#Survival Analysis

#Data Partition

set.seed(1234)
split <-  createDataPartition(survival_data$Status,p=0.7,list=F)
train <- survival_data[split,]
test <- survival_data[-split,]

#Kaplan-Meier overall survival curve

Kmcurve <- survfit(Surv(Time,Status)~ 1, data = train)
print(Kmcurve)
summary(Kmcurve)
summary(Kmcurve)$table

quantile(Kmcurve, probs = c(0.05,0.25,0.5,0.75,0.95))
print(Kmcurve, print.rmean=TRUE)

survival:::survmean(Kmcurve, rmean=60)
#plot survival curve

library(survminer)
ggsurvplot(Kmcurve,
           pval = TRUE, conf.int = TRUE, conf.int.style = 'step',
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv",# Specify median survival
           title = "Kaplan Meier Survival Curve",
           ggtheme = theme_bw(),
           color = "darkblue")



#kaplan Meier survival curve by new credit customer
fit1 <- survfit(Surv(Time,Status)~ NewCreditCustomer, data = train)
print(fit1)
ggsurvplot(fit1,
           conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           title = "Kaplan Meir survial curve for NewCreditCustomer",
           ggtheme = theme_bw()) # Change ggplot2 theme


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
                 data = train)


summary(res.cox)

#plot the cox survivall curve
ggsurvplot(survfit(res.cox), data=train,color = "red",
           ggtheme = theme_minimal())


#plot cumaulative hazard for cox model

ggsurvplot(survfit(res.cox), data=train,palette  = "red",fun = "cumhaz",
           ggtheme = theme_minimal())

#plot the hazard ratios
ggforest(res.cox)


#cumulative hazard ratios
str(survfit(res.cox))
basehaz(res.cox)

# testing the proportional hazard assumptions
cox_test <- cox.zph(res.cox)

#plot schoenfeld residuals

ggcoxzph(cox_test, var = 'NewCreditCustomerYes')


#Exponential surival 
train <- train %>% filter(Time >0)
test <- test %>% filter(Time>0)

exp_model_1 <- survreg(Surv(Time, Status)~1,dist = "exponential" ,data = train)
summary(exp_model_1)

exp_model <- survreg(Surv(Time, Status)~NewCreditCustomer+VerificationType+Age+Gender+Country+
                       AppliedAmount+Interest+LoanDuration+UseOfLoan+MaritalStatus+EmploymentStatus+
                       IncomeTotal+Rating, dist = "exponential",
                     data = train)

summary(exp_model)

attributes(exp_model)
exp_model$coefficients

pred_exp <- predict(exp_model, newdata = test, type = "term")
pred_exp_sum <- apply(pred_exp,1,sum)
base_hazard <- exp(- 4.379462e+00)
hazard_rates <- base_hazard* exp(pred_exp_sum)
survival_prob <- exp(-hazard_rates)

#life time value calculation
library(pec)
t<- 1:83
surv_prob <-predictSurvProb(res.cox,test,t)

M <- test$MonthlyPayment
r <- test$Interest

#function to calulate CLV
clv_data <- data.frame(surv_prob)
clv_data$r <- (test$Interest/12)/100
clv_data$M <-test$MonthlyPayment
clv_data$n <- 83

CLV <- vector("numeric",nrow(clv_data))

for (i in 1:nrow(clv_data)){
  b<-0
  for (j in 1:clv_data$n[i]){
  a <- clv_data[i,j]/((1+clv_data$r[i])^j)
  b <- b+a 
  }
  CLV[i]<- b
}

CLV_return <- clv_data$M*CLV

#comaprison between applied amount and clv
comparison_data <- data.frame(Applied_amount=test$AppliedAmount,CLV_return =CLV_return, 
                              profit= CLV_return-test$AppliedAmount)



survival_data <- survival_data[sample(nrow(survival_data)),]
library(ggplot2)
ggplot(arrange(survival_data,desc(Time)), aes(x=1:nrow(survival_data),y= Time))+
  geom_col(aes(color=State, fill=State))+coord_flip()
ggplot(survival_data, aes(x=LoanId,y= Time))+geom_col(aes(color=State))+coord_flip()

pic <- ggplot(survival_data, aes(x=1:nrow(survival_data),y= Time))+geom_col(aes(color=State, fill=State))+coord_flip()
pic
ggsave("survival plot", pic, device = "jpeg")
