
loan<-read.csv(file='~/Desktop/loan/loan.csv', header=TRUE)
dim(loan)
colnames(loan)

install.packages("dplyr")
library(dplyr)
library(ggthemes)
library(ggplot2)

# feature selection 
loan_df<-loan %>%
  select(loan_status, loan_amnt, int_rate, grade, emp_length, 
         home_ownership, annual_inc, term, addr_state, issue_d)

# missing values 
sapply(loan, function(x) sum(is.na(x)))

# remove 4 rows with missing annual income, 
# remove 49 rows where home ownership is 'NONE' or 'ANY' 
# remove rows where emp_length is 'n/a'.
loan_df<-loan_df %>%
  filter(!is.na(annual_inc),
         !(home_ownership %in% c('NONE','ANY')),
         emp_length!='n/a')

# loan_status
loan_status<-loan_df %>%
  count(loan_status) 
ggplot(data = loan_status, aes(x=reorder(loan_status, -desc(n)), y=n, fill=n))+
  geom_col()+labs(x='Loan Status', y='Count')+coord_flip()

loan_df %>%
  group_by(loan_status) %>%
  summarise(count=n(), percentage=count/nrow(loan_df)*100) %>%
  knitr::kable()

Defaulted<-c("Charged_Off","Default",
             "Does not meet the credit policy. Status:Charged Off", 
             "In Grace Period","Late (16-30 days)","Late (31-120 days)")
Paid<-c("Fully Paid")
loan_df1<-na.omit(loan_df$loan_status)
loan_1<-loan_df %>%
  mutate(identification = ifelse(loan_status %in% Defaulted, 0,
         ifelse(loan_status %in% Paid, 1, "Other")))
unique(loan_1$identification)
loan_1_1<-loan_1 %>%
  select(-loan_status) %>%
  filter(identification %in% c(0,1))

iden<-loan_1_1 %>%
  group_by(identification) %>%
  summarise(count=n(), proportion=count/nrow(loan_1_1)) 


par(mfrow=c(1,2))

bar<-c("0(32407)","1(986651)")
barplot(iden$count, main = "Barplot of Loan Status", col=c("pink", "lightblue"),
        xlab = "Loan Status", ylab = "Count", 
        ylim = c(0,1000000), names.arg = c('0','1'))
legend('topleft', bar, cex=0.6, fill = c("pink", "lightblue"))

x=c(32407,986651)
labels=c('0','1')
percent<-round(100*x/sum(x),2)
percent <-paste(percent, "%", sep = "") 
pie(x,labels=percent, main = 'Pie Chart of Loan Status', col = c("pink", "lightblue"), 
    radius = 0.9)
legend("topleft", labels, cex=0.6, fill = c("pink", "lightblue"))


# int & grade
ggplot(loan_1_1, aes(x=grade, y=int_rate, fill=grade))+geom_boxplot()+
  theme_igray()+
  labs(y='Interest Rate', x='Grade', 
       title = 'Boxplot of Interest Rates in Different Grades')+
  theme(plot.title=element_text(hjust=0.5))


# map - loan amount 
library(choroplethrMaps)
library(choroplethr)
data("state.regions")
state_count<-loan_1_1 %>%
  group_by(addr_state) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))
state<-cbind(state.regions, state_count)
state_map<-data.frame(region=state$region, value=state$value)
state_choropleth(state_map, title = "Count by State", legend = "Total Amount")
  



install.packages("gmodels")
install.packages("lubridate")
install.packages("plyr")
install.packages("caTools")
install.packages("e1071")
install.packages("ROCR")
install.packages("caret")
install.packages("ROSE")
library(gmodels)
library(lubridate)
library(plyr)
library(caTools)
library(e1071)
library(ROCR)
library(caret)
library(ROSE)
#------model building---------------------
loan.model<-subset(loan_1_1, select = c(1:10))
anyNA(loan.model)   # no missing value
dim(loan.model)     # 9 features + 1 response, 1019058 obs

loan.model<-subset(loan_1_1, select=c(1:4, 5, 5, 5, 5, 6, 7, 10))
loan.model$loan_amnt<-as.numeric(loan.model$loan_amnt)
loan.model$int_rate<-as.numeric(loan.model$int_rate)
loan.model$grade=c("A"=1, "B"=2, "C"=3, "D"=4, "E"=5, "F"=6, "G"=7)[as.numeric(loan.model$grade)]
loan.model$emp_length=c("< 1 year"=0, "1 year"=1, "2 years"=2, "3 years"=3, "4 years"=4, 
                        "5 years"=5, "6 years"=6, "7 years"=7, "8 years"=8, "9 years"=9, 
                        "10+ years"=10)[as.numeric(loan.model$emp_length)]
loan.model<-rename(loan.model,c(home_ownership="home_ownership_mortgage", 
                                home_ownership.1="home_ownership_rent", 
                                home_ownership.2="home_ownership_own", 
                                home_ownership.3="home_ownership_other"))
loan.model$home_ownership_mortgage=c("MORTGAGE"="1", "RENT"="0", "OWN"="0", "OTHER"="0")[as.character(loan.model$home_ownership_mortgage)]
loan.model$home_ownership_rent=c("MORTGAGE"="0", "RENT"="1", "OWN"="0", "OTHER"="0")[as.character(loan.model$home_ownership_rent)]
loan.model$home_ownership_own=c("MORTGAGE"="0", "RENT"="0", "OWN"="1", "OTHER"="0")[as.character(loan.model$home_ownership_own)]
loan.model$home_ownership_other=c("MORTGAGE"="0", "RENT"="0", "OWN"="0", "OTHER"="1")[as.character(loan.model$home_ownership_other)]
loan.model$home_ownership_mortgage<-as.numeric(loan.model$home_ownership_mortgage)
loan.model$home_ownership_rent<-as.numeric(loan.model$home_ownership_rent)
loan.model$home_ownership_own<-as.numeric(loan.model$home_ownership_own)
loan.model$home_ownership_other<-as.numeric(loan.model$home_ownership_other)
loan.model$annual_inc<-as.numeric(loan.model$annual_inc)
loan.model$term=c("36 months"=36, "60 months"=60)[as.numeric(loan.model$term)]
loan.model$identification<-as.numeric(loan.model$identification)


# Data Normalization 
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
loan_norm<-as.data.frame(lapply(loan.model, normalize))


# check multicollinearity 
library(GGally)
ggcorr(loan_norm, hjust=0.9, label = T, layout.exp = 1)

library(magrittr)
library(dplyr)
loan_norm_model<-loan_norm %>%
  select(loan_amnt, int_rate, emp_length, home_ownership_mortgage, home_ownership_own,
         home_ownership_other, annual_inc, term, identification)

as.character(loan_norm_model$identification)
table(loan_norm_model$identification)

#split dataset into training and testing set
set.seed(123)  # make results reproduciable 
index<-sample(nrow(loan_norm_model), nrow(loan_norm_model)*0.7)
train.df<-loan_norm_model[index,]
test.df<-loan_norm_model[-index,]
train.df$identification<-factor(train.df$identification)
test.df$identification<-factor(test.df$identification)


library(ROSE)
loan.oversample<-ovun.sample(identification~., data = train.df, method = "over", N=800000,seed = 13)$data
table(loan.oversample$identification)


# Logistic Regression Model 
lr.model<-glm(formula = identification~.,family = "binomial", data = loan.oversample)
summary(lr.model)
library(lattice)
library(ggplot2)
library(caret)
lr<-train(identification~., data = loan.oversample, method = "glm")
summary(lr)
test.pred<-predict(lr,test.df)
library(dplyr)
pred_label<-as.factor(if_else(test.pred<0.5,0,1))
confusionMatrix(predict(lr, test.df), test.df$identification)


# ROC 
library(ROCR)
lr.prediction<-prediction(predict(lr, newdata = test.df, type = "prob")[,"1"], test.df$identification)
performance(lr.prediction, measure = "auc")@y.values
perf<-performance(prediction.obj = lr.prediction, measure = "tpr","fpr")
plot(perf, colorize=TRUE, main=paste("AUC:", performance(lr.prediction, measure = "auc")@y.values))



# Decision Trees
library(rpart)
tune<-data.frame(0.001)
colnames(tune)<-"cp"
tr_control<-trainControl(method = "cv", number=10, verboseIter = TRUE)
loan.rpart.oversampled<-train(identification~., data = loan.oversample, method="rpart",
                              trControl=tr_control, tuneGrid=tune, 
                              control=rpart.control(minsplit = 10, minbucket=3))
library(lattice)
library(ggplot2)
library(caret)
confusionMatrix(predict(loan.rpart.oversampled, test.df), test.df$identification)
loan.rpart.pred<-prediction(predict(loan.rpart.oversampled, newdata=test.df,
                                    type="prob")[,"1"], test.df$identification)
performance(loan.rpart.pred, measure="auc")@y.values



# KNN 
train_label<-loan.oversample$identification
test_label<-test.df$identification

library(magrittr)
library(dplyr)
train_knn<-loan.oversample %>%
  select_if(is.numeric) %>%
  scale

test.knn<-test.df %>%
  select_if(is.numeric) %>%
  scale(center = attr(train_knn, "scaled:center"),
        scale = attr(train_knn, "scaled:scale"))

# determine k value 
sqrt(nrow(train_knn))
library(class)
model_knn<-knn(train=train_knn, test=test.knn, cl=train_label, k=)



# svm
run_svm <- function(data = loan.oversample, cost = 1, kernel = "linear", newdata = test.df, degree = 3, gamma = NA){
  if(is.na(gamma)){
    model <- svm(identification ~ ., data = loan.oversample, cost = cost, kernel = kernel, degree = degree)
  } else {
    model <- svm(identification ~ ., data = loan.oversample, cost = cost, kernel = kernel, degree = degree, gamma = gamma)
  }
  predictions <- as.ordered(predict(model, newdata, type = "response"))
  print(table(newdata[["identification"]], predictions))
  print(paste("Misclassification Rate =", 
              mean(newdata[["identification"]] != predictions)))
  print(auc(roc(newdata[["identification"]], predictions)))
}

# Varying cost values
costs <- c(0.1, 0.5, 1, 2, 3)
# Training Set
for (i in costs){
  print(paste("Predicting Training Set with Cost =", i))
  run_svm(cost = i, newdata = loan.oversample)
}
# Test Set
for (i in costs){
  print(paste("Predicting Test Set with Cost =", i))
  run_svm(cost = i)
}
# Varying Polynomial Kernels across Degrees
degrees <- c(2, 3, 4)
# Training Set
for (i in degrees){
  print(paste("Predicting Training Set with Degree =", i))
  run_svm(kernel = "polynomial", newdata = loan.oversample, degree = i)
}
## Test Set
for (i in degrees){
  print(paste("Predicting Test Set with Degree =", i))
  run_svm(kernel = "polynomial", degree = i)
}
# Varying Radial Kernel across Gamma values
gammas <- c(0.1, 1, 3)
## Training Set
for (i in gammas){
  print(paste("Predicting Training Set with Gamma =", i))
  run_svm(kernel = "radial", newdata = loan.oversample, gamma = i)
}
## Test Set
for (i in gammas){
  print(paste("Predicting Test Set with Gamma =", i))
  run_svm(kernel = "radial", gamma = i)
}


# naive bayes
library(e1071)
classifier <- naiveBayes(identification~., loan.oversample)
classifier
prediction <- predict(classifier, select(test.df), type="raw")
summary(prediction)
test.df$identification <- ifelse(prediction[,"1"] > 0.75, "1", "0")
table(prediction, test.df$identification)
library(caret)
confusionMatrix(prediction, test.df$identification)

# linear discriminant analysis  
model.LDA <- lda(identification~., data=loan.oversample, na.action="na.omit")
model.LDA
pre.LDA <- predict(model.LDA, na.roughfix(test.df))
summary(pre.LDA$identification)
LDA <- table(pre.LDA, test.df$identification)
caret::confusionMatrix(pre.LDA, test.df$identification)






