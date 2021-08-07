#########################
# Loading Iris data set
#########################

#Method 1

#install.packages("datasets")
library(datasets)
data(iris)
iris2 <- datasets::iris
View(iris)

#Method 2
#install.packages("RCurl")
#library(RCurl)
#iris3 <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv") )

#############################
# Display summary statistics
#############################

# head() / tail()
head(iris, 5)
tail(iris, 10)

# summary()
summary(iris)
summary(iris$Sepal.Length)

#check to see if there are any missing data
is.na(iris)
sum(is.na(iris))

#skimr() -expands on summary() by providing larger statistics
#install.packages("skimr")
# https://github.com/ropensci/skimr
library(skimr)
skim(iris)#perform skim on iris dataset to display summary
skim(iris$Sepal.Length)

#Group data by species and then perform skim
iris %>%
  dplyr::group_by(Species) %>%
  skim()

#############################
# Quick data visualization
#
# R base plot()
#############################

#Panel Plots
plot(iris)
plot(iris, col= "red")

#scatter plots
plot(iris$Sepal.Width, iris$Sepal.Length, col = "red") #Makes red circles

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",
     xlab = "Sepal Width", ylab ="Sepal Length") # Makes red circles plus adds x and y labels
#Histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col = "red") #Makes Red Bars

#Feature Plots
# https://www.machinelearningplus.com/machine-learning/caret-package/
#install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

# Load the caret package
library(caret)


featurePlot(x=iris[,1:4],
            y=iris$Species,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x=list(relation="free"),
                          y=list(relation="free")))

