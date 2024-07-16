##---Libraries used in this project---##
library("dplyr")            #Data Manipulating 
library("ggplot2")          #Data Visualization
library("Amelia")           #multiple imputation 
library("caret")
library("corrplot")
library("factoextra")
library("plot3D")
library("rpart")
library("rpart.plot")
library("tidyr")

library("car")              #Regression Visualization
library("corrplot")
library("Metrics")
library("stats")

# Importing the data
db <- read.csv(choose.files())
#getting overview Insights from it 
View(db)
summary(db)
str(db)
head(db)

db$Outcome <- as.factor(db$Outcome)
#checking for any missing values in the dataset 
any(is.na(db))
any(is.null(db))

ggplot(gather(db[,-9]),aes(value)) + geom_histogram() + facet_wrap(key~.,scales="free_x")


#Bloodpressure cant have zero value
#normal people
ind_n <-which(db$BloodPressure==0 & db$Outcome==0)
#diabetic people
ind_d <-which(db$BloodPressure==0 & db$Outcome==1)

#replace normal people  and diabetic person with respective means
v_n <- mean(db$BloodPressure[!db$BloodPressure==0 & db$Outcome==0])
v_d <- mean(db$BloodPressure[!db$BloodPressure==0 & db$Outcome==1])
db$BloodPressure[ind_n] <- v_n
db$BloodPressure[ind_d] <- v_d

#SkinThickness
ind_n <-which(db$SkinThickness==0 & db$Outcome==0)
ind_d <-which(db$SkinThickness==0 & db$Outcome==1)
v_n <- mean(db$SkinThickness[!db$SkinThickness==0 & db$Outcome==0])
v_d <- mean(db$SkinThickness[!db$SkinThickness==0 & db$Outcome==1])
db$SkinThickness[ind_n] <- v_n
db$SkinThickness[ind_d] <- v_d

#BMI
ind_n <-which(db$BMI==0 & db$Outcome==0)
ind_d <-which(db$BMI==0 & db$Outcome==1)
v_n <- mean(db$BMI[!db$BMI==0 & db$Outcome==0])
v_d <- mean(db$BMI[!db$BMI==0 & db$Outcome==1])
db$BMI[ind_n] <- v_n
db$BMI[ind_d] <- v_d

#insuline
ind_n <-which(db$Insulin==0 & db$Outcome==0)
ind_d <-which(db$Insulin==0 & db$Outcome==1)
v_n <- mean(db$Insulin[!db$Insulin==0 & db$Outcome==0])
v_d <- mean(db$Insulin[!db$Insulin==0 & db$Outcome==1])
db$Insulin[ind_n] <- v_n
db$Insulin[ind_d] <- v_d

#glucose
ind_n <-which(db$Glucose==0 & db$Outcome==0)
ind_d <-which(db$Glucose==0 & db$Outcome==1)
v_n <- mean(db$Glucose[!db$Glucose==0 & db$Outcome==0])
v_d <- mean(db$Glucose[!db$Glucose==0 & db$Outcome==1])
db$Glucose[ind_n] <- v_n
db$Glucose[ind_d] <- v_d

#now we can Proceed


#lest see the corrplot

corrplot(cor(db[,-9]),method="number")
pairs(db[,-9],col=db$Outcome)


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



ggplot(db,aes(x=DiabetesPedigreeFunction,y=Insulin)) + geom_smooth() + geom_point(aes(color=Outcome))




ggplot(db,aes(x=BloodPressure,y=Insulin)) + geom_smooth() + geom_point(aes(color=Outcome))




ggplot(db,aes(x=Age,y=Insulin)) + geom_smooth() + geom_point(aes(color=Outcome))



ggplot(db,aes(x=BloodPressure,y=Insulin,color=BMI,size=Age)) + geom_point(alpha=0.5) + facet_wrap(Outcome~.) + ggtitle("Insulin vs BloodPressure")



~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#data model accuracy

#test train split
ind <- sample(2,nrow(db),replace=T,prob=c(0.75,0.25))
train <- db[ind==1,]
test <- db[ind==2,]


#Building a model
my_model <- glm(Outcome~.,data=train,family="binomial")
summary(my_model)


#Evaluating
p1 <- predict(my_model,test,type="response")
p1 <- ifelse(p1>0.5,1,0)
tab1 <- table(predicted=p1,actual=test$Outcome)
tab1
acc <- (sum(diag(tab1))/sum(tab1))*100
q <- paste("Accuracy is ",acc)
print(q)


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



library(e1071)
library(caTools)
library(class)

# Calculating No. Diabetes Patients
my_summary <- db %>% count(Outcome)
my_summary

ran <- sample(1:nrow(db),0.9*nrow(db))
nor <- function(x) {(x-min(x))/max(x)-min(x)}


#KNN
# Run normalization on  coulumns of dataset because they are the predictors
db_norm <- as.data.frame(lapply(db[-9],nor))

summary(db_norm)
#extracting training dataset
db_train<- db_norm[ran,]
#extracting testing set
db_test<- db_norm[-ran,]
# Extracting 9th column of train dataset bcoz it will be the cl argument in knn
#function
db_target_category <- db[ran,9]
# Extracting 9th column of test dataset to measure accuracy
db_test_category <- db[-ran,9]
#loaing class
library(class)

?knn

# run KNN fuction
pr <- knn(db_train,db_test,cl=db_target_category,k=90)

#create confusion matrix 
tab <- table(pr,db_test_category)
#this fin divides thr correct predictions by total number of predictions that tells us how accurate the model is
tab
accuracy <-function(x) { sum(diag(x)/(sum(rowSums(x))))*100 }
accuracy(tab)

#kmeans

result<-kmeans(db,8,nstart = 150)
print(result)

aggregate(db[-9],by=list(cluster=result$cluster),mean)

newdata<-cbind(db[-9],cluster=result$cluster)

ggplot(data=newdata,aes(x=result$cluster))+geom_bar(fill='steelblue')
fviz_cluster(result,newdata)
########

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#decision tree
db_decision_tree_model= rpart(Outcome~.,data=db,method = "class")
plot(db_decision_tree_model)
text(db_decision_tree_model)

rpart.plot(db_decision_tree_model)
rpart.plot(db_decision_tree_model,type=3,extra = 103)

#linera regression


#Linear Regression with multiple variables
model=lm(db_train$Insulin~.,data=db_train)
print(model)
db_train1 <- db_train
db_train1$Predicted_val <- predict(model,db_train[-9])
head(db_train1)
db_test1 <- db_test
db_test1$Predicted_val <- predict(model,db_test[-9])
head(db_test1)
#plotting the insulin level vs predicted 
ggplot()+
  geom_point(aes(x=db_train1$Insulin,y=db_train1$Predicted_val),size=3,color="blue")
ggplot()+
  geom_point(aes(x=db_test1$Insulin,y=db_test1$Predicted_val),size=3,color="blue")

#Error checking using Root Mean Square Error
rmse(db_train1$Predicted_val,db_train1$Insulin)
rmse(db_test1$Predicted_val,db_test1$Insulin)
# Variance inflation factor
vif(model)

