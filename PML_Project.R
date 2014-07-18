library(caret)
library(ggplot2)

set.seed(123)
training  <- read.csv("pml-training.csv",na.strings=c("","NA"))
testing   <- read.csv("pml-testing.csv",na.strings=c("","NA"))

#Looking through the data, Most of the columns are simply NA or 0
#Removing those columns for trainig will reduce data-size and is likely 
#to produce good model for testing 

bad_column_test = lapply(training, function(x) {sum(is.na(x))})
bad_column_name = names(bad_column_test[bad_column_test !=0])

ptraining <- training[,!names(training) %in% bad_column_name]
ptraining <- ptraining[,-c(1:7)]
ptesting  <- testing[,!names(testing) %in% bad_column_name]
ptesting  <- ptesting[,-c(1:7)]

inTrain   <- createDataPartition(y=ptraining$classe,p=0.7,list=FALSE)
tr        <- ptraining[inTrain,]
cv        <- ptraining[-inTrain,]
                       
rfmodel   <- train(tr$classe~.,method="rf",data=tr)
saveRDS(rfmodel, "rfmodel.RDS")


tc        <- trainControl(method="cv",number=3)
rfcvmodel <- train(cv$classe~.,method="rf",data=cv,trControl=tc)
saveRDS(rfcvmodel,"rfcvmodel.RDS")
predcv    <- predict(rfcvmodel,cv)

confusionMatrix(predcv,cv$classe)


test_prediction<-predict(rfmodel, newdata=ptesting)
test_prediction
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(test_prediction)