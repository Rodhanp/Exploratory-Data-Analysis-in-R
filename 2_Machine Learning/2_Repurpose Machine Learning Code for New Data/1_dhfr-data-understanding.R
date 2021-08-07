#########################
# Loading dhfr data set
#########################

#Method 1

#install.packages("datasets")
library(datasets)
library(caret)
data(dhfr)

#dhfr2 <- datasets::dhfr

View(dhfr)

#Method 2
#install.packages("RCurl")
#library(RCurl)
#dhfr3 <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/iris.csv") )

#############################
# Display summary statistics
#############################

# head() / tail()
head(dhfr, 5)
tail(dhfr, 10)
dhfr$Y

# summary()
summary(dhfr)
summary(dhfr$Y)

#check to see if there are any missing data
#is.na(dhfr)
sum(is.na(dhfr))

#skimr() -expands on summary() by providing larger statistics
#install.packages("skimr")
# https://github.com/ropensci/skimr
library(skimr)
skim(dhfr)#perform skim on dhfr dataset to display summary
skim(dhfr$Y)

#Group data by Y (Biological activity) and then perform skim

#dhfr %>%
#  dplyr::group_by(Y)

dhfr %>%
  dplyr::group_by(Y) %>%
  skim()

#############################
# Quick data visualization
#
# R base plot()
#############################

#Panel Plots
#plot(dhfr)
#plot(dhfr, col= "red")

#scatter plots
plot(dhfr$moeGao_Abra_basicity, dhfr$moeGao_Abra_acidity, col = "red") #Makes red circles

plot(dhfr$Y, dhfr$moeGao_Abra_acidity, col = "red") #Makes red circles

plot(dhfr$moeGao_Abra_basicity, dhfr$moeGao_Abra_acidity, col = "red",
     xlab = "Basicity", ylab ="Acidity") # Makes red circles plus adds x and y labels
#Histogram
hist(dhfr$moe2D_Weight)
hist(dhfr$moe2D_Weight, col = "red") #Makes Red Bars

#Feature Plots
# https://www.machinelearningplus.com/machine-learning/caret-package/
#install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth'))

# Load the caret package
library(caret)


featurePlot(x=dhfr[,2:30],
            y=dhfr$Y,
            plot="box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x=list(relation="free"),
                          y=list(relation="free")))

#dhfr$moe2D_FCharge has the same values in all but one cell and therefore its feature plot is same for active and inactive
