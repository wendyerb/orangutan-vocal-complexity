# Create confusion matrix for publication

# Load necessary libraries
library(caret)
library(MASS)
library(e1071)

####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Create new variable for SVM that keeps pulse type column
all.features.svm <- all.features

# Check out different pulse types
table(all.features.svm$Pulse.Type)

# Part 1. Identify the best kernel type -----------------------------------
# We will use the same number of samples for each class

# Create list to look at results
ConfMatrixDFlist <- data.frame()

# Vector of kernels
kernels <- c('linear','polynomial','radial','sigmoid')

# Remove pulse type
N.randomization <- 25

N.samples <- min(table(all.features.svm$Pulse.Type)) - 5

  
Unique.pulsetype <- unique(all.features.svm$Pulse.Type)


for(a in 1:N.randomization){
  CombinedBalancedPulseTypeDF <- data.frame()
  for(c in 1:length(Unique.pulsetype)){   
    
    TempSubsetPulse <- subset(all.features.svm,Pulse.Type==Unique.pulsetype[c])
    
    Samples.vec <- sample( c(1:nrow(TempSubsetPulse)), size = N.samples, replace = FALSE)
    
    all.features.temp <- TempSubsetPulse[Samples.vec,]
    
    CombinedBalancedPulseTypeDF <- rbind.data.frame(CombinedBalancedPulseTypeDF,all.features.temp)
  }
  
  print(table(CombinedBalancedPulseTypeDF$Pulse.Type))
  
  
for(z in 1:length(kernels)) {

    Samples.vec <- sample( c(1:nrow(CombinedBalancedPulseTypeDF)), size = nrow(CombinedBalancedPulseTypeDF)*.6, replace = FALSE)
    
    all.features.sub <- CombinedBalancedPulseTypeDF[Samples.vec,]
    all.features.test <- CombinedBalancedPulseTypeDF[-Samples.vec,]
    
    # SVM
    svm.sig.method.1.all <-
      svm(
        all.features.sub[, 1:46],  # Selecting columns 1 to 46 as features for training
        all.features.sub$Pulse.Type,  # The target variable for training
        kernel = kernels[z],  # Using the sigmoid kernel for SVM
        cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
        # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
      )
    
    # Predicting the target variable using the SVM model on the test data.
    SVMPredictions <- predict(svm.sig.method.1.all,all.features.test[, 1:46])
    
    # Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
    ConfMatrix <-caret::confusionMatrix(all.features.test$Pulse.Type,SVMPredictions,'everything')
    
    #Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
    ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))
    
    # Take the total accuracy for each class
    ProportionCorrect <- as.numeric(ConfMatrix$overall[1])
    
    # Calculate accuracy for each class
    ProportionCorrect <- diag(as.matrix(ConfMatrixDF))/rowSums(ConfMatrixDF)
    
    # Round the proportion values
    ConfMatrixDF$ProportionCorrect <- round(ProportionCorrect,2)
    
    # Print the confusion matrix and saving it as a CSV file.
    print(ConfMatrixDF)
    kerneltype <- kernels[z]
    ConfMatrixDF$ProportionCorrect[is.na(ConfMatrixDF$ProportionCorrect)] <- 0
    Values <- as.data.frame(t(c(sum(ConfMatrixDF$ProportionCorrect),kerneltype, a)))
    colnames(Values) <- c('Sum','kernel','iteraction')
    print(Values)
    ConfMatrixDFlist<-  rbind.data.frame(ConfMatrixDFlist,Values)
  }
} 


# Aggregate the sums across iterations
AggregatePerform <- aggregate(as.numeric(ConfMatrixDFlist$Sum), 
          list(ConfMatrixDFlist$kernel), FUN=mean) 

# Which is the best performing kernel?
AggregatePerform$Group.1[which.max(AggregatePerform$x)]

# Part 2. Calculate accuracy over 20 iterations -------------------------------------------------------------------------
# Now we have chosen our kernel we run over multiple iterations 

# Initialize an empty data frame to store confusion matrix values
ConfMatrixsvmDF <- data.frame()

# Loop over 20 iterations
for(a in 1:20){
  
  CombinedBalancedPulseTypeDF <- data.frame()
  for(c in 1:length(Unique.pulsetype)){   
    
    TempSubsetPulse <- subset(all.features.svm,Pulse.Type==Unique.pulsetype[c])
    
    Samples.vec <- sample( c(1:nrow(TempSubsetPulse)), size = N.samples, replace = FALSE)
    
    all.features.temp <- TempSubsetPulse[Samples.vec,]
    
    CombinedBalancedPulseTypeDF <- rbind.data.frame(CombinedBalancedPulseTypeDF,all.features.temp)
  }
  
  # Randomly sample indices for training data
  Samples.vec <- sample( c(1:nrow(CombinedBalancedPulseTypeDF)), size = nrow(CombinedBalancedPulseTypeDF)*0.6, replace = FALSE)
  
  # Subset training and test data based on sampled indices
  all.features.sub <- CombinedBalancedPulseTypeDF[Samples.vec,]
  all.features.test <- CombinedBalancedPulseTypeDF[-Samples.vec,]
  
  # SVM model training using polynomial kernel
  svm.sig.method.1.all <-
    svm(
      all.features.sub[, 1:46],  # Selecting columns 1 to 46 as features for training
      all.features.sub$Pulse.Type,  # The target variable for training
      kernel = 'linear',  # Using the polynomial kernel for SVM
      cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
      # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
    )
  
  # Predicting the target variable using the SVM model on the test data.
  SVMPredictions <- predict(svm.sig.method.1.all,all.features.test[, 1:46])
  
  # Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
  ConfMatrix <-caret::confusionMatrix(all.features.test$Pulse.Type,SVMPredictions,'everything')
   
  #Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
  ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))
  
  # Calculate accuracy for each class
  ProportionCorrect <- diag(as.matrix(ConfMatrixDF))/rowSums(ConfMatrixDF)
  
  # Round the proportion values
  ProportionCorrect <- round(ProportionCorrect,2)
  
  
  Labels <- colnames(ConfMatrixDF)
  

  # Create a data frame with label and balanced accuracy values
  Values <- cbind.data.frame(Labels,ProportionCorrect)
  colnames(Values) <- c('Labels','Proportion Correct')
  
  # Add iteration number
  Values$Iteration <- a
  
  # Append Values to ConfMatrixsvmDF
  ConfMatrixsvmDF <- rbind.data.frame(ConfMatrixsvmDF ,Values)
}

# Calculate mean balanced accuracy for each label
AggregatePerformSVMean <- aggregate(as.numeric(ConfMatrixsvmDF$`Proportion Correct`), 
                                    list(ConfMatrixsvmDF$Labels), FUN=mean) 

# Calculate standard deviation of balanced accuracy for each label
AggregatePerformSVsd <- aggregate(as.numeric(ConfMatrixsvmDF$`Proportion Correct`), 
                                  list(ConfMatrixsvmDF$Labels), FUN=sd) 

# Round to two decimal
AggregatePerformSVMean$x <- round(AggregatePerformSVMean$x,2)
AggregatePerformSVMean$x

# Round to two decimal
AggregatePerformSVsd$x <- round(AggregatePerformSVsd$x,2)
AggregatePerformSVsd$x

MeanSD <- paste(AggregatePerformSVMean$x, '±', AggregatePerformSVsd$x)
PulseType <- AggregatePerformSVMean$Group.1

# Now we can print the mean and SD for balanced accuracy by pulse type
cbind.data.frame(PulseType,MeanSD)

# NOTE: there is some randomization so the values might be slightly different
# PulseType      MeanSD
# 1        HR 0.47 ± 0.14
# 2        HU  0.74 ± 0.1
# 3        IN 0.45 ± 0.15
# 4        LR 0.39 ± 0.12
# 5        SI 0.54 ± 0.13
# 6        VO 0.51 ± 0.18


# Part 3. Recursive feature elimination -------------------------------------------

source("code/msvmRFE.R")

##### The first column needs to include class labels
all.features.temp <- all.features[,-c(47)]

##### We want k=10 for the k-fold cross validation as the “multiple” part of mSVM-RFE.
svm.rfe.output <- svmRFE(all.features.rfe, k = 10, halve.above = 100)
str(svm.rfe.output) 

##### Reorder the data so highest ranked feature is first
new.svm.rfe <-
  all.features[, 1:ncol(all.features)][, dput(svm.rfe.output)]


Pulse.Type <- all.features$Pulse.Type

new.svm.rfe <- cbind.data.frame(Pulse.Type,new.svm.rfe)

str(new.svm.rfe)

##### Create a list to store cross-validation accuracies
accuracy.list <- list()

##### Set prior so that class membership is equally likely
n.pulses <- length(unique(all.features$Pulse.Type))

##### Loop to add features one by one and calculate balanced accuracy 
for (j in 3:length(svm.rfe.output)) {
  svm.rfe.sub <- new.svm.rfe[,2:j]
  
  svm.sig.method.1.all <-
    svm(
      new.svm.rfe[,2:j],  # Selecting columns 1 to 46 as features for training
      new.svm.rfe$Pulse.Type,  # The target variable for training
      kernel = 'linear',  # Using the polynomial kernel for SVM
      cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
      # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
    )
  
  ##### total percent correct
  percent <- svm.sig.method.1.all$tot.accuracy
  print(percent)
  accuracy.list[[j]] <- percent
} 

##### Find which number of features provides the maximum classification accuracy
max.feature <- which.max(unlist(accuracy.list)) + 1   

##### Subset the highest ranked variables which yield the highest accuracy
svm.rfe.updated <- new.svm.rfe[,1:max.feature]

colnames(svm.rfe.updated)

##### Combine class labels with new subset of features into a data frame for analysis
svm.rfe.for.classification <-
  cbind.data.frame(all.features$Pulse.Type, svm.rfe.updated)

colnames(svm.rfe.for.classification)[1] <- "Pulse.Type"

##### Combine class labels with new subset of features into a data frame for analysis
svm.rfe.for.classification <-
  cbind.data.frame(all.features$Pulse.Type, svm.rfe.updated)

colnames(svm.rfe.for.classification)[1] <- "Pulse.Type"

##### Assess how well the leave one out cross validation did
svm.sig.method.1.all <-
  svm(
    new.svm.rfe[,2:j],  # Selecting columns 1 to 46 as features for training
    new.svm.rfe$Pulse.Type,  # The target variable for training
    kernel = 'linear',  # Using the polynomial kernel for SVM
    cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
    # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
  )


##### total percent correct
percent <- svm.sig.method.1.all$tot.accuracy
percent
