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
# Some variation based on randomization but 'linear' seems to be consistently the best
AggregatePerform$Group.1[which.max(AggregatePerform$x)]

# Part 2. Calculate accuracy over 20 iterations -------------------------------------------------------------------------
# Now we have chosen our kernel we run over multiple iterations 
Unique.pulsetype <- unique(all.features.svm$Pulse.Type)

# Initialize an empty data frame to store confusion matrix values
ConfMatrixsvmDF <- data.frame()
TotalAccuracyList <- list()

# Loop over 20 iterations
for(a in 1:20){
  
  # CombinedBalancedPulseTypeDF <- data.frame()
  # for(c in 1:length(Unique.pulsetype)){   
  #   
  #   TempSubsetPulse <- subset(all.features.svm,Pulse.Type==Unique.pulsetype[c])
  #   
  #   Samples.vec <- sample( c(1:nrow(TempSubsetPulse)), size = N.samples, replace = FALSE)
  #   
  #   all.features.temp <- TempSubsetPulse[Samples.vec,]
  #   
  #   CombinedBalancedPulseTypeDF <- rbind.data.frame(CombinedBalancedPulseTypeDF,all.features.temp)
  # }
  
  # Randomly sample indices for training data
  Samples.vec <- sample( c(1:nrow(all.features.svm)), size = nrow(all.features.svm)*0.6, replace = FALSE)
  
  # Subset training and test data based on sampled indices
  all.features.sub <- all.features.svm[Samples.vec,]
  all.features.test <- all.features.svm[-Samples.vec,]
  
  # SVM model training using best kernel
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
   
  TotalAccuracyList[[a]] <- as.numeric(ConfMatrix$overall[1])
  
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

# Calculate mean accuracy for each label
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
# 1        HR 0.64 ± 0.09
# 2        HU 0.74 ± 0.12
# 3        IN  0.27 ± 0.1
# 4        LR 0.56 ± 0.06
# 5        SI 0.93 ± 0.02
# 6        VO 0.33 ± 0.11

Mean <- AggregatePerformSVMean$x

OriginalPulseClassification <- cbind.data.frame(PulseType,Mean,MeanSD)
write.csv(OriginalPulseClassification,'data_V1/OriginalPulseClassification.csv',row.names = F)

# Now for overall
mean(unlist(TotalAccuracyList))
min(unlist(TotalAccuracyList))
max(unlist(TotalAccuracyList))
sd(unlist(TotalAccuracyList))

# Part 3. Recursive feature elimination -------------------------------------------

source("code/msvmRFE.R")

##### The first column needs to include class labels
all.features.temp <- all.features[,c(47,1:46)]

##### We want k=10 for the k-fold cross validation as the “multiple” part of mSVM-RFE.
svm.rfe.output <- svmRFE(all.features.temp, k = 10, halve.above = 100)
str(svm.rfe.output) 

##### Reorder the data so highest ranked feature is first
new.svm.rfe <-
  all.features[, 1:ncol(all.features)][, dput(svm.rfe.output)]

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

# Check column names
colnames(svm.rfe.updated)

##### Combine class labels with new subset of features into a data frame for analysis
svm.rfe.for.classification <-
  cbind.data.frame(all.features$Pulse.Type, svm.rfe.updated)

colnames(svm.rfe.for.classification)[1] <- "Pulse.Type"

##### Assess how well the leave one out cross validation did
svm.sig.method.1.all <-
  svm(
    svm.rfe.for.classification[,-c(1)],  # Selecting columns 1 to 46 as features for training
    svm.rfe.for.classification$Pulse.Type,  # The target variable for training
    kernel = 'linear',  # Using the polynomial kernel for SVM
    cross = nrow(svm.rfe.for.classification) # Setting the 'cross' parameter for cross-validation
   )


##### total percent correct
percent <- svm.sig.method.1.all$tot.accuracy
percent


# Part 4. Confusion matrix ------------------------------------------------

####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Create new variable for SVM that keeps pulse type column
all.features.svm <- all.features


    Samples.vec <- sample( c(1:nrow(all.features)), size = 620, replace = FALSE)
    
    all.features.sub <- all.features[Samples.vec,]
    all.features.test <- all.features[-Samples.vec,]
    
    # SVM
    svm.sig.method.1.all <-
      e1071::svm(
        all.features.sub[, 1:46],  # Selecting columns 1 to 46 as features for training
        all.features.sub$Pulse.Type,  # The target variable for training
        kernel = 'linear',  # Using the linear kernel for SVM
        cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
        # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
      )
    
    # Predicting the target variable using the SVM model on the test data.
    SVMPredictions <- predict(svm.sig.method.1.all,all.features.test[, 1:46])
    
    # Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
    ConfMatrix <-caret::confusionMatrix(all.features.test$Pulse.Type,SVMPredictions,'everything')
    
    # Calculating the proportion of correct predictions for each class in the confusion matrix.
    ProportionCorrect <- diag(ConfMatrix$table)/rowSums(ConfMatrix$table)
    
    #Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
    ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))
    
    # Round the proportion values
    ConfMatrixDF$Correct <- round(ProportionCorrect,2)
    
    # Print the confusion matrix and saving it as a CSV file.
    print(ConfMatrixDF)

    # Save the output
    write.csv(ConfMatrixDF,'data_V1/ConfusionMatrix.csv')

# Part 5. SVM random samples---------------------------------------------------------------------

# Set number of samples to iterate over
N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)

N.randomization <-25

SVM.rand.df <- data.frame()

for(b in 1:N.randomization){
  for(a in 1:length(N.samples)){
    
    Samples.vec <- sample( c(1:nrow(all.features.svm)), size = N.samples[a], replace = FALSE)
    
    all.features.svm.sub <- all.features.svm[Samples.vec,]
    
    svm.sig.method.1.all <-
      svm(
        all.features.svm.sub[, 1:46],
        all.features.svm.sub$Pulse.Type,
        kernel = "linear",
        cross = nrow(all.features.svm.sub) # When cross = number of observations this indicates leave-one-out cross-validation
      )
    
    svm.accuracy <- svm.sig.method.1.all$tot.accuracy ## 64.76283 DJC: 74.44337
    
    Temp.row <- cbind.data.frame(svm.accuracy,a,b)
    print(Temp.row)
    
    colnames(Temp.row) <- c('svm.accuracy','n.samples','randomization')
    SVM.rand.df <- rbind.data.frame(SVM.rand.df,Temp.row)
    write.csv(SVM.rand.df,'data_V1/SVM.rand.df.csv')  
  }
}

SVM.rand.df <- read.csv('data_V1/SVM.rand.df.csv')
SVM.rand.df$n.samples <- as.factor(SVM.rand.df$n.samples)
levels(SVM.rand.df$n.samples) <- N.samples

RandomSVM <- ggpubr::ggboxplot(data=SVM.rand.df,
                               x='n.samples', y='svm.accuracy',outlier.shape = NA)+ ylab('SVM accuracy')+ xlab('N samples')

cowplot::plot_grid(RandomAffinity,RandomFuzzy,RandomTypicality,RandomSVM,
                   labels=c('A)', 'B)','C)','D)'),label_x = 0.9)

# Part 6. Calculate accuracy over 20 iterations for new classification -------------------------------------------------------------------------
all.features.updated <- read.csv('data_V1/500_pulses_new_classes_46features.csv')

#### Check distribution of pulse types
table(all.features.updated$New.Pulse)

####Make pulse type a factor
all.features.updated$Pulse.Type <- factor(all.features.updated$New.Pulse, levels = c("R", "I", "S"))

# Now we have chosen our kernel we run over multiple iterations 
Unique.pulsetype <- unique(all.features.updated$Pulse.Type)

# Initialize an empty data frame to store confusion matrix values
ConfMatrixsvmDF <- data.frame()
TotalAccuracyList <- list()

# Loop over 20 iterations
for(a in 1:20){
  
    # Randomly sample indices for training data
  Samples.vec <- sample( c(1:nrow(all.features.updated)), size = nrow(all.features.updated)*0.6, replace = FALSE)
  
  # Subset training and test data based on sampled indices
  all.features.sub <- all.features.updated[Samples.vec,]
  all.features.test <- all.features.updated[-Samples.vec,]
  
  # SVM model training using best kernel
  svm.sig.method.1.all <-
    svm(
      all.features.sub[, -c(1:14)],  # Selecting columns 1 to 46 as features for training
      all.features.sub$Pulse.Type,  # The target variable for training
      kernel = 'linear',  # Using the polynomial kernel for SVM
      cross = nrow(all.features.sub) # Setting the 'cross' parameter for cross-validation
      # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
    )
  
  # Predicting the target variable using the SVM model on the test data.
  SVMPredictions <- predict(svm.sig.method.1.all,all.features.test[, -c(1:14)])
  
  # Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
  ConfMatrix <-caret::confusionMatrix(all.features.test$Pulse.Type,SVMPredictions,'everything')
  
  TotalAccuracyList[[a]] <- as.numeric(ConfMatrix$overall[1])
  
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

# Calculate mean accuracy for each label
AggregatePerformSVMean <- aggregate(as.numeric(ConfMatrixsvmDF$`Proportion Correct`), 
                                    list(ConfMatrixsvmDF$Labels), FUN=mean) 

# Calculate standard deviation of balanced accuracy for each label
AggregatePerformSVsd <- aggregate(as.numeric(ConfMatrixsvmDF$`Proportion Correct`), 
                                  list(ConfMatrixsvmDF$Labels), FUN=sd) 

# Round to two decimal
AggregatePerformSVMean$x <- round(AggregatePerformSVMean$x,2)
Mean <- AggregatePerformSVMean$x

# Round to two decimal
AggregatePerformSVsd$x <- round(AggregatePerformSVsd$x,2)
AggregatePerformSVsd$x

MeanSD <- paste(AggregatePerformSVMean$x, '±', AggregatePerformSVsd$x)
PulseType <- AggregatePerformSVMean$Group.1

# Now we can print the mean and SD for balanced accuracy by pulse type
cbind.data.frame(PulseType,MeanSD)

# NOTE: there is some randomization so the values might be slightly different
# PulseType      MeanSD
# 1         I 0.65 ± 0.05
# 2         R 0.86 ± 0.05
# 3         S 0.92 ± 0.04

# Create new dataframe
NewPulseClassification <- cbind.data.frame(PulseType,Mean,MeanSD)

# Save it as a .csv
write.csv(NewPulseClassification,'data_V1/NewPulseClassification.csv',row.names = F)

# Now for overall
mean(unlist(TotalAccuracyList))
min(unlist(TotalAccuracyList))
max(unlist(TotalAccuracyList))
sd(unlist(TotalAccuracyList))
