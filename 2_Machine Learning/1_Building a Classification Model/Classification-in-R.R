
#Importing Libraries
library(datasets)
library(caret)

#Importing the Iris Dataset
data(iris)
#View(iris)

#Check to see if there are missing data
sum(is.na(iris))

#To achieve reproducible model, set the random seed number
set.seed(100)

#Performs stratifies random splits of dataset
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list=FALSE)
TrainingSet <- iris[TrainingIndex,] #Makes Training subset(80%)
TestingSet <- iris[-TrainingIndex,] #Makes Testing subset (20%)

#Scatter plot for Training and Testing Subset
plot(TrainingSet,col ="red")
plot (TestingSet, col ="blue")
plot(TrainingSet$Sepal.Length, TrainingSet$Sepal.Width, col="blue",
     xlab="Training Set-Sepal Length", ylab="Training Set-Sepal Width")

plot(TestingSet$Sepal.Length, TestingSet$Sepal.Width, col="red",
     xlab="Testing Set-Sepal Length", ylab="Testing Set-Sepal Width")

#########################
#SVM model(Support vector model)-polynomial kernal
#Build Training Model
Model <- train(Species ~., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trcontrol= trainControl(method="none"),
               tuneGrid=data.frame(degree=1,scale=1,C=1) )
# Build Cross Validation (CV) Model
Model.CV <- train(Species ~., data=TrainingSet,
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
Model.trainingset.confusion <- confusionMatrix(Model.TrainingSet, TrainingSet$Species)
Model.testingset.confusion <- confusionMatrix(Model.TestingSet, TestingSet$Species )
Model.cv.confiusion <- confusionMatrix(Model.CrossValidation, TrainingSet$Species)

print(Model.trainingset.confusion)
print(Model.testingset.confusion)
print(Model.cv.confiusion)

#Feature Importance
Importance <- varImp(Model)
plot(Importance)
plot(Importance,col="red")





