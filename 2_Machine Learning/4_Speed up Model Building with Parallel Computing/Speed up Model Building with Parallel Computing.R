#Speed up Model Building with Parallel Computing

#Importing datasets library
library(datasets)
library(caret)
library(randomForest)
library(e1071)

#Importing dhfr dataset
data(dhfr)

#Checking to see any null values
sum(is.na(dhfr))

#seting seed for maintaining reproducibilty
set.seed(100)

#random split of data for machine learning
TrainingIndex <- createDataPartition(dhfr$Y,p=0.8,list=FALSE)
TrainingSet <- dhfr[TrainingIndex,] #Training set
TestingSet <- dhfr[-TrainingIndex,] #Testing set

#############################
#Random Forest

#Run normally without parallel processing
start.time <- proc.time()
Model <- train(Y ~.,
               data=TrainingSet, #Build model for training set
               method="rf" #Learning Algorithm
               )
stop.time <- proc.time()
run.time <- start.time - stop.time
print(run.time)

#Time taken is 82.40 seconds (Hp Omen-Core I7-7th Gen)

#Use doParallel
# https://topepo.github.io/caret/parallel-processing.html

#install.packages('doParallel')

library(doParallel)

cl <- makePSOCKcluster(3) #i7 7th Gen has 4 cores, so using 3 so that 1 is free to handle other tasks from the PC
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf" # Learning algorithm
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)

#Time taken with Parallel Computing is 28.31 seconds i.e 2.9 faster than normal computing

#Hyperparameter tuning with parallel computing

# Run without parallel processing

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

#Time taken without parallel computing for Hyperparameter tuning is 57.71 seconds

# Using doParallel

library(doParallel) 

cl <- makePSOCKcluster(3) #i7 7th Gen has 4 cores, so using 3 so that 1 is free to handle other tasks from the PC
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(Y ~ ., 
               data = TrainingSet, # Build model using training set
               method = "rf", # Learning algorithm
               tuneGrid = data.frame(mtry = seq(5,15, by=5))
)
stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopCluster(cl)
#Time taken with parallel computing for Hyperparameter tuning is 25.49 seconds i.e 2.2 times faster than normal computing

##############################
#Apply Model for Prediction
Model.training <- predict(Model,TrainingSet) #Prediction on training set

#Model Performance by confusion matrix
ModelPerformance <- confusionMatrix(Model.training, TrainingSet$Y)
print(ModelPerformance)

#Feature Importance
Importance <- varImp((Model))
plot(Importance)
plot(Importance,col="red",top = 25)
