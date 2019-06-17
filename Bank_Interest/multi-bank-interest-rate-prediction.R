# Import training and testing files

ld_train = read.csv(file.choose(),header = T)

ld_test = read.csv(file.choose(),header = T)

## Import dplyr package #
# When working with data you must:
#   
#   Figure out what you want to do.
# 
# Describe those tasks in the form of a computer program.
# 
# Execute the program.
# 
# The dplyr package makes these steps fast and easy:
#   
#   By constraining your options, it helps you think about your data manipulation challenges.
# 
# It provides simple “verbs”, functions that correspond to the most common data manipulation tasks, to help you translate your thoughts into code.
# 
# It uses efficient backends, so you spend less time waiting for the computer.

library(dplyr)

glimpse(ld_train)
glimpse(ld_test)

# Combine Train and Test files for data pre-processing

ld_test$Interest.Rate=NA
ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)

# Start data preperation
    
    ## It is essential that there is only numeric data for linear regression ##

ld_all = ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )

glimpse(ld_all)

# Drop amount funded by investors as this information will not be available at the decision point

ld_all = ld_all %>% select(-Amount.Funded.By.Investors)

ld_all = ld_all %>% 
  mutate(f1 = as.numeric(substr(FICO.Range, 1, 3)),
         f2 = as.numeric(substr(FICO.Range, 5, 7)),
         fico = 0.5 * (f1 + f2)
        ) %>%
  select(-FICO.Range, -f1, -f2) 

glimpse(ld_all)

ld_all = ld_all %>% mutate(
  el = ifelse(substr(Employment.Length, 1, 2) == "10", 10, Employment.Length),
  el = ifelse(substr(Employment.Length, 1, 1) == "<", 0, el),
  el = gsub("years", "", el),
  el = gsub("year", "", el),
  el = as.numeric(el)
) %>% select(-Employment.Length)

glimpse(ld_all)

## Function to create dummy variables

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

# Taking a look at loan purpose variable

table(ld_all$Loan.Purpose)

round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm=T))

ld_all = ld_all %>% mutate(
  #lp_10 = as.numeric(Loan.Purpose == 'educational'),
  lp_11 = as.numeric(Loan.Purpose %in% c("major_purchase", "medical", "car","educational")),
  lp_12 = as.numeric(Loan.Purpose %in% c(
    "vacation", "wedding", "home_improvement"
  )),
  lp_13 = as.numeric(Loan.Purpose %in% c("other", "small_business", "credit_card")),
  lp_14 = as.numeric(Loan.Purpose %in% c("debt_consolidation", "house", "moving"))
) %>%
  select(-Loan.Purpose)

for(col in c("Loan.Length", "State", "Home.Ownership")) {
  ld_all = CreateDummies(ld_all, col, 100)
}

# Check for missing values in data

lapply(ld_all,function(x) sum(is.na(x)))

ld_all=ld_all[!(is.na(ld_all$ID)),]

# Impute the missing values

for(col in names(ld_all)) {
  if (sum(is.na(ld_all[, col])) > 0 & !(col %in% c("ID", "data", "Interest.Rate"))) {
    ld_all[is.na(ld_all[, col]), col] = mean(ld_all[ld_all$data == "train", col], na.rm =T)
  }
}

## Data preperation done ##

# Split the data

ld_train=ld_all %>% filter(data=='train') %>% select(-data) 
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)


set.seed(2) 
s=sample(1:nrow(ld_train),0.7*nrow(ld_train)) 
ld_train1=ld_train[s,] 
ld_train2=ld_train[-s,]

glimpse(ld_train1)

## Build the model ##

fit=lm(Interest.Rate~. -ID,data=ld_train1)

library(car) 
vif(fit)

sort(vif(fit),decreasing = T)[1:3]

fit=lm(Interest.Rate~. -ID - lp_14,data=ld_train) 
sort(vif(fit),decreasing = T)[1:3]

fit=step(fit)

summary(fit)

formula(fit)

fit = lm(
  Interest.Rate ~ Amount.Requested + Open.CREDIT.Lines + Inquiries.in.the.Last.6.Months + fico + Loan.Length_36months + State_TX + Home.Ownership_MORTGAGE,
  data = ld_train1
)

summary(fit)

library(ggplot2) 
ld_train1 %>%
  mutate(pred_IR=predict(fit,newdata=ld_train1)) %>% 
  ggplot(aes(x=Interest.Rate,y=pred_IR))+geom_point(alpha=0.6)

model_string=paste(fit$coefficients,names(fit$coefficients),sep="*",collapse = " + ") 
model_eq=strwrap(sub("\\*\\(Intercept\\)","",gsub("+ -","- ",model_string,fixed=TRUE))) 
model_eq

plot(fit,which=1)


plot(fit,which=2)

df=data.frame(res=fit$residual) 
ggplot(df,aes(x=res))+geom_density(color="red")+
  stat_function(fun=dnorm ,args = list(mean=mean(df$res),sd=sd(df$res)),color="green")

shapiro.test(fit$residuals)

plot(fit,which=3)

plot(fit,which=4)

rmse = mean((ld_train2$Interest.Rate - predict(fit, newdata = ld_train2)) **
  2) %>%
  sqrt()
rmse


## Final Model ##

fit.final=fit=lm(Interest.Rate ~ .-ID, data=ld_train)
fit.final=step(fit.final)

summary(fit.final)

pred.IR=predict(fit.final,newdata=ld_test)

write.csv(pred.IR,"C:/Users/saiva/Desktop/Major Project/Bank_Interest/final_r1.csv",row.names = F)










