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


# Part 1. Identify the best kernel type -----------------------------------
# Create vector that includes number of training samples
n.train <- 620

# Create list to look at results
ConfMatrixDFlist <- data.frame()

kernels <- c('linear','polynomial','radial','sigmoid')

for(z in 1:length(kernels)) {
  for(a in 1:10){
    Samples.vec <- sample( c(1:nrow(all.features)), size = n.train, replace = FALSE)
    
    all.features.sub <- all.features[Samples.vec,]
    all.features.test <- all.features[-Samples.vec,]
    
    # SVM
    svm.sig.method.1.all <-
      svm(
        all.features.sub[, 1:46],  # Selecting columns 1 to 46 as features for training
        all.features.sub$Pulse.Type,  # The target variable for training
        kernel = kernels[z],  # Using the sigmoid kernel for SVM
        # cost = cost.sig.all,  # Optional hyperparameter (not specified in this code)
        # gamma = gamma.sig.all,  # Optional hyperparameter (not specified in this code)
        cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
        # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
      )
    
    # Predicting the target variable using the SVM model on the test data.
    SVMPredictions <- predict(svm.sig.method.1.all,all.features.test[, 1:46])
    
    # Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
    ConfMatrix <-caret::confusionMatrix(all.features.test$Pulse.Type,SVMPredictions,'everything')
    
    # CTakng the balanced accuracy for each class
    ProportionCorrect <- as.numeric(ConfMatrix$byClass[,11])
    
    #Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
    ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))
    
    # Round the proportion values
    ConfMatrixDF$BalancedAccuracy <- round(ProportionCorrect,2)
    
    # Print the confusion matrix and saving it as a CSV file.
    print(ConfMatrixDF)
    kerneltype <- kernels[z]
    ConfMatrixDF$BalancedAccuracy[is.na(ConfMatrixDF$BalancedAccuracy)] <- 0
    Values <- as.data.frame(t(c(sum(ConfMatrixDF$BalancedAccuracy),kerneltype, a)))
    colnames(Values) <- c('Sum','kernel','iteraction')
    print(Values)
    ConfMatrixDFlist<-  rbind.data.frame(ConfMatrixDFlist,Values)
  }
}

# This provides the sum of the percent correct for all classes; linear seems to do best


ConfMatrixDFlist[which.max(ConfMatrixDFlist$Sum),]

AggregatePerform <- aggregate(as.numeric(ConfMatrixDFlist$Sum), 
          list(ConfMatrixDFlist$kernel), FUN=mean) 

# Which is the best performing kernel?
AggregatePerform$Group.1[which.max(AggregatePerform$x)]

# Save the output
# NOTE: The best kernel was the linear kernel
write.csv(ConfMatrixDFlist,'data_V1/KernelComparisons.csv')

ConfMatrixDF <- read.csv('data_V1/ConfusionMatrix.csv')


# Part 2. Calculate balanced accuracy over 20 iterations -------------------------------------------------------------------------
# Now we have chosen our kernel we run over multiple iterations 

# Initialize an empty data frame to store confusion matrix values
ConfMatrixsvmDF <- data.frame()

# Loop over 20 iterations
for(a in 1:20){
  # Randomly sample indices for training data
  Samples.vec <- sample( c(1:nrow(all.features)), size = n.train, replace = FALSE)
  
  # Subset training and test data based on sampled indices
  all.features.sub <- all.features[Samples.vec,]
  all.features.test <- all.features[-Samples.vec,]
  
  # SVM model training using polynomial kernel
  svm.sig.method.1.all <-
    svm(
      all.features.sub[, 1:46],  # Selecting columns 1 to 46 as features for training
      all.features.sub$Pulse.Type,  # The target variable for training
      kernel = 'polynomial',  # Using the polynomial kernel for SVM
      cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
      # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
    )
  
  # Predicting the target variable using the SVM model on the test data.
  SVMPredictions <- predict(svm.sig.method.1.all,all.features.test[, 1:46])
  
  # Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
  ConfMatrix <-caret::confusionMatrix(all.features.test$Pulse.Type,SVMPredictions,'everything')
  
  # Taking the balanced accuracy for each class
  ProportionCorrect <- as.numeric(ConfMatrix$byClass[,11])
  
  # Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
  ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))
  
  Labels <- colnames(ConfMatrixDF)
  
  # Round the proportion values
  ConfMatrixDF$BalancedAccuracy <- round(ProportionCorrect,2)
  
  # Create a data frame with label and balanced accuracy values
  Values <- cbind.data.frame(Labels,ConfMatrixDF$BalancedAccuracy)
  colnames(Values) <- c('Labels','BalancedAccuracy')
  
  # Add iteration number
  Values$Iteration <- a
  
  # Append Values to ConfMatrixsvmDF
  ConfMatrixsvmDF <- rbind.data.frame(ConfMatrixsvmDF ,Values)
}

# Calculate mean balanced accuracy for each label
AggregatePerformSVMean <- aggregate(as.numeric(ConfMatrixsvmDF$BalancedAccuracy), 
                                    list(ConfMatrixsvmDF$Labels), FUN=mean) 

# Calculate standard deviation of balanced accuracy for each label
AggregatePerformSVsd <- aggregate(as.numeric(ConfMatrixsvmDF$BalancedAccuracy), 
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

# This is for balanced accuracy
# PulseType      MeanSD
# 1        HR 0.81 ± 0.02
# 2        HU 0.91 ± 0.07
# 3        IN 0.76 ± 0.06
# 4        LR 0.68 ± 0.03
# 5        SI 0.82 ± 0.02
# 6        VO 0.71 ± 0.08


# Recursive feature elimination -------------------------------------------

source("code/msvmRFE.R")

##### The first column needs to include class labels
all.features.temp <- all.features[,-c(47)]
Pulse.Type <- all.features$Pulse.Type

all.features.rfe <- cbind.data.frame(Pulse.Type,all.features.temp)
##### We want k=10 for the k-fold cross validation as the “multiple” part of mSVM-RFE.
svm.rfe.output <- svmRFE(all.features.rfe, k = 10, halve.above = 100)
str(svm.rfe.output) 

##### Reorder the data so highest ranked feature is first
new.svm.rfe <-
  all.features[, 1:ncol(all.features)][, dput(svm.rfe.output)]
str(new.svm.rfe)

## Center.Freq.Hz 
## Peak.Freq.Hz  
## meanpeakf  
## Freq.75..Hz 
## Freq.25..Hz 

##### Create a list to store cross-validation accuracies
accuracy.list <- list()

##### Set prior so that class membership is equally likely
n.pulses <- length(unique(all.features$Pulse.Type))

##### Loop to add features one by one and calculate balanced accuracy 
for (j in 2:length(svm.rfe.output)) {
  svm.rfe.for.lda <- new.svm.rfe[1:j]
  
  fit.svm.rfe.all <- lda(
    svm.rfe.for.lda,
    center = TRUE,
    prior = rep(1 / n.pulses, n.pulses),
    
    scale. = TRUE,
    grouping = all.features$Pulse.Type,
    CV = T
  )
  
  ##### Run LDA on the data subset using RFE
  fit.standard.number.windows.svm.rfe <- lda(
    svm.rfe.for.classification[1:ncol(svm.rfe.for.classification)],
    center = TRUE,
    prior = rep(1 / n.pulses, n.pulses),
    scale. = TRUE,
    grouping = svm.rfe.for.classification$Pulse.Type,
    CV = T
  )
  
  ##### Assess how well the leave one out cross validation did
  ct <- caret::confusionMatrix(all.features$Pulse.Type, fit.svm.rfe$class)
  
  ##### total percent correct
  percent <- mean( as.numeric(ConfMatrix$byClass[,11]))
  print(percent)
  accuracy.list[[j]] <- percent
} 

##### Find which number of features provides the maximum classification accuracy
max.feature <- which.max(unlist(accuracy.list)) + 1   

##### Subset the highest ranked variables which yield the highest accuracy
svm.rfe.for.lda <- new.svm.rfe[1:max.feature]

##### Combine class labels with new subset of features into a data frame for analysis
svm.rfe.for.classification <-
  cbind.data.frame(all.features$Pulse.Type, svm.rfe.for.lda)
colnames(svm.rfe.for.classification)[1] <- "Pulse.Type"

##### Combine class labels with new subset of features into a data frame for analysis
svm.rfe.for.classification <-
  cbind.data.frame(all.features$Pulse.Type, svm.rfe.for.lda)
colnames(svm.rfe.for.classification)[1] <- "Pulse.Type"

##### Assess how well the leave one out cross validation did
ct <-
  table(grouping = svm.rfe.for.classification$Pulse.Type,
        fit.standard.number.windows.svm.rfe$class)
ct

##### Calculate total percent correct
percent <- sum(diag(prop.table(ct)))
print(percent) 

