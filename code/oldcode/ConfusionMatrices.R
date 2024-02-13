# Create confusion matrix for publication

# Load necessary libraries
library(caret)
library(MASS)

####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Create new variable for SVM that keeps pulse type column
all.features.svm <- all.features

# Create list to look at results
ConfMatrixDFlist <- list()

kernels <- c('linear','polynomial','radial','sigmoid')

for(z in 1:length(kernels)) {
  for(a in 1:2){
    Samples.vec <- sample( c(1:nrow(all.features)), size = 620, replace = FALSE)
    
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
    
    # Calculating the proportion of correct predictions for each class in the confusion matrix.
    ProportionCorrect <- diag(ConfMatrix$table)/rowSums(ConfMatrix$table)
    
    #Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
    ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))
    
    # Round the proportion values
    ConfMatrixDF$Correct <- round(ProportionCorrect,2)
    
    # Print the confusion matrix and saving it as a CSV file.
    print(ConfMatrixDF)
    kerneltype <- kernels[z]
    ConfMatrixDFlist[[z]] <- c(sum(ConfMatrixDF$Correct),kerneltype)
  }
}

# This provides the sum of the percent correct for all classes; linear seems to do best
do.call(rbind,ConfMatrixDFlist)

# Save the output
# NOTE: The best kernel was the linear kernel
write.csv(ConfMatrixDF,'data_V1/ConfusionMatrix.csv')

# Here we run over LDA
ConfMatrixldaDFlist <- list()
for(a in 1:20){
  Samples.vec <- sample( c(1:nrow(all.features)), size = 620, replace = FALSE)
  
  all.features.sub <- all.features[Samples.vec,]
  all.features.test <- all.features[-Samples.vec,]
  
  lda.sig.method.1.all <- MASS::lda(
    all.features.sub[, 1:46],
    center = TRUE,
    prior = rep(1 / 6, 6),
    scale. = TRUE,
    grouping =  all.features.sub$Pulse.Type
  )
  
  # Predicting the target variable using the lda model on the test data.
  ldaPredictions <- predict(lda.sig.method.1.all,all.features.test[, 1:46])
  
  # Generating a confusion matrix to evaluate the performance of the lda model on the test data.
  ConfMatrixlda <-caret::confusionMatrix(all.features.test$Pulse.Type,ldaPredictions$class,'everything')
  
  # Calculating the proportion of correct predictions for each class in the confusion matrix.
  ProportionCorrect <- diag(ConfMatrixlda$table)/rowSums(ConfMatrixlda$table)
  
  #Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
  ConfMatrixldaDF <- as.data.frame(as.matrix(ConfMatrixlda))
  
  # Round the proportion values
  ConfMatrixldaDF$Correct <- round(ProportionCorrect,2)
  
  # Print the confusion matrix and saving it as a CSV file.
  print(ConfMatrixldaDF)
  ConfMatrixldaDFlist[[a]] <- ConfMatrixldaDF
}

