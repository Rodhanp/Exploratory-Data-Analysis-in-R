#loading library
library(caret)

#loading dhfr dataset
data(dhfr)

#view(dhfr)

#check for missing values
sum(is.na(dhfr))

#function to add missing values in the dataset
na.gen <- function(data,n){
  i <- 1
  while (i < n+1) {
    idx1 <- sample(1:nrow(data), 1)
    idx2 <- sample(1:ncol(data), 1)
    data[idx1,idx2] <- NA
    i = i+1
  }
  return(data)
}

#Remove the Y column before introducing the NA values
dhfr <- dhfr[,-1]
#View(dhfr)

#adding NA to dataset
dhfr <- na.gen(dhfr,100)

#Check for missing data
sum(is.na(dhfr))
colSums(is.na(dhfr))
View(colSums(is.na(dhfr)))
str(dhfr)
View(str(dhfr))

#List rows with missing data
missingdata <- dhfr[!complete.cases(dhfr),]
View(missingdata)
sum(is.na(missingdata))

# If above sum is 0, this means that there is no missing data and proceed to modeling.
# If above sum is greater than 0, then proceed to handling the missing data. 
# There are 2 options-

# 1. Simply delete all entries with missing data
clean.Data <- na.omit(dhfr)
sum(is.na(clean.Data))

# 2.Imputation: Replace missing values with the column's 

# Mean

dhfr.impute <- dhfr
for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- mean(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))

# Median
dhfr.impute <- dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) { 
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- median(dhfr.impute[, i],  na.rm = TRUE) 
}

sum(is.na(dhfr.impute))
