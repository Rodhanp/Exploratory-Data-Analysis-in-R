
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

# Build Cross Validation (CV) Model
Model.CV <- train(Y ~., data=TrainingSet,
                  method="svmPoly",
                  na.action=na.omit,
                  preProcess=c("center","scale"),
                  trControl=trainControl(method="cv",number=10),
                  tuneGrid=data.frame(degree=1,scale=1,C=1))  

#Apply Model for prediction
Model.TrainingSet <- predict(Model, TrainingSet) #Predict Training Set using the model
Model.TestingSet <- predict(Model, TestingSet) #Predict Testing Set using the model
Model.CrossValidation <- predict(Model.CV, TrainingSet) # Predict Cross Validation of the Training Set

#Model Performance-Displays Confusion Matrix and other statistics
Model.trainingset.confusion <- confusionMatrix(Model.TrainingSet, TrainingSet$Y)
Model.testingset.confusion <- confusionMatrix(Model.TestingSet, TestingSet$Y )
Model.cv.confiusion <- confusionMatrix(Model.CrossValidation, TrainingSet$Y)

print(Model.trainingset.confusion)
print(Model.testingset.confusion)
print(Model.cv.confiusion)

#Feature Importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance,col="red")
plot(Importance,col="red", top = 25) # Visualizing top 25 values for better representation





