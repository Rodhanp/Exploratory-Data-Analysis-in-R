
#Importing Libraries
library(datasets)
library(caret)

#Importing the dhfr Dataset
data(dhfr)
#View(dhfr)

#Check to see if there are missing data
sum(is.na(dhfr))

#To achieve reproducible model, set the random seed number
set.seed(100)

#Performs stratifies random splits of dataset
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list=FALSE)
TrainingSet <- dhfr[TrainingIndex,] #Makes Training subset(80%)
TestingSet <- dhfr[-TrainingIndex,] #Makes Testing subset (20%)


#########################
#SVM model(Support vector model)-polynomial kernal
#Build Training Model
Model <- train(Y ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trcontrol= trainControl(method="none"),
               tuneGrid=data.frame(degree=1,scale=1,C=1) )


#Save Model to RDS
saveRDS(Model, "Model.rds")

#Read Model from RDS
read.Model <- readRDS("Model.rds")


#Apply Model for prediction
Model.TrainingSet <- predict(read.Model, TrainingSet) #Predict Training Set using the model
Model.TestingSet <- predict(read.Model, TestingSet) #Predict Testing Set using the model

#Model Performance-Displays Confusion Matrix and other statistics
Model.trainingset.confusion <- confusionMatrix(Model.TrainingSet, TrainingSet$Y)
Model.testingset.confusion <- confusionMatrix(Model.TestingSet, TestingSet$Y )

print(Model.trainingset.confusion)
print(Model.testingset.confusion)

#Feature Importance
Importance <- varImp(read.Model)
plot(Importance)
plot(Importance,col="red")
plot(Importance,col="red", top = 25) # Visualizing top 25 values for better representation





