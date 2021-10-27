library(Matrix)
library(xgboost)
library(data.table)

#Get the data
data(iris)

#Create a mock output vector
output_train<-c(rnorm(50,mean = 25,sd=5),rnorm(50,mean = 15,sd=1),rnorm(50,mean = 20,sd=3))

#Prepare the dataset/change the categorical variables into numerical variables
iris<-cbind(iris,output_train)
sparse_matix<-sparse.model.matrix(output_train~.-1,data=iris)

#Set seed
set.seed(777)

#Split the dataset into training and testing parts
testno<-sample(1:150,30)
ir.test<-sparse_matix[testno,]
out.test<-output_train[testno]
ir.train<-sparse_matix[-testno,]
out.train<-output_train[-testno]

#Train the model
modeltest<-xgboost(data=ir.test, label=out.test,eta=0.1,nrounds = 1000,early_stopping_rounds = 40,max_depth=20,nthread=4,save_period = 0,save_name = "Iris_mod.model")

rm(modeltest)

oldmodel<-xgboost(data=sparse_matix, label=output_train,eta=0.1,nrounds = 1000,max_depth=20,nthread=4,xgb_model = "Iris_mod.model")

#Test the model
testpred<-predict(modeltest,ir.test)
plot(x=out.test,y=testpred)
abline(lm(testpred~out.test))
summary(lm(testpred~out.test))
modelimportance<-xgb.importance(model=modeltest)
xgb.plot.importance(importance_matrix = modelimportance)
