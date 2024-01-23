### Part 2: Support Vector Machines & RFE ###

# Code adapted from Clink & Klinck 2020

##### SVM ##### --------------------------------------------

##### Load required libraries
library(MASS)
library(e1071)

####Read in features 
all.features <- read.csv('46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

#### Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

#### Create new variable for SVM that keeps pulse type column
all.features.svm <- all.features

#### Tune the parameters and run the SVM with sigmoid kernel with feature extraction method 1
tune.sig.method.1.all <-
  tune(
    svm,
    all.features[, 1:46],
    all.features$Pulse.Type,
    kernel = "sigmoid",
    ranges = list(
      cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
      gamma = c(0.001, 0.01, 0.1, 0.5, 1.0, 2.0)
    )
  )

cost.sig.all <- tune.sig.method.1.all$best.parameters$cost
cost.sig.all ## 100
gamma.sig.all <-  tune.sig.method.1.all$best.parameters$gamma
gamma.sig.all ## 0.001

svm.sig.method.1.all <-
  svm(
    all.features[, 1:46],
    all.features$Pulse.Type,
    kernel = "sigmoid",
    cost = cost.sig.all,
    gamma = gamma.sig.all,
    cross = nrow(all.features) # When cross = number of observations this indicates leave-one-out cross-validation
  )

svm.sig.method.1.all$tot.accuracy ## 64.76283 

## Run line 41 9 more times to get a range of outputs from leave-one-out validation
## 64.95644
## 64.47241
## 65.44046
## 64.95644
## 64.2788
## 64.85963
## 64.66602
## 64.76283
## 64.56922

summary(svm.sig.method.1.all)

##### Recursive Feature Elimination #### -------------------------

##### Set source code (downloaded from https://github.com/johncolby/SVM-RFE)
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

##### Loop to add features one by one and calculate accuracy using leave-one-out cross-validation
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
  ct <- table(grouping = all.features$Pulse.Type, fit.svm.rfe$class)
  
  ##### total percent correct
  percent <- sum(diag(prop.table(ct)))
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
