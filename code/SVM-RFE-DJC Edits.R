### Part 2: Support Vector Machines & RFE ###

# Code adapted from Clink & Klinck 2020

##### SVM ##### --------------------------------------------

##### Load required libraries
library(MASS)
library(e1071)
set.seed(123)

####Read in features 
all.features <- read.csv('data/46-features.csv')

####Make pulse type a factor
all.features$Pulse.Type <- as.factor(all.features$Pulse.Type) 

####Check structure
str(all.features)

####Tune the parameters and run the SVM with sigmoid kernel with feature extraction method 1
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

svm.sig.method.1.all$tot.accuracy ## 64.76283 DJC: 74.44337

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

##### The first column needs to include class labels so the following lines reorder
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
head(new.svm.rfe)

## Center.Freq.Hz 
## Peak.Freq.Hz  
## meanpeakf  
## Freq.75..Hz 
## Freq.25..Hz 

##### Assess how well the leave one out cross validation did

SVM_predict <- predict(svm.sig.method.1.all,all.features[, 1:46])

ct_svm <-
  table(grouping = all.features$Pulse.Type,
        SVM_predict)
ct_svm

##### Calculate total percent correct_svm
percent <- sum(diag(prop.table(ct_svm)))
print(percent) 


## LDA
fit.lda.rfe.all <- lda(
  all.features[,-(47)],
  center = TRUE,
  prior = rep(1 / n.pulses, n.pulses),
  scale. = TRUE,
  grouping = all.features$Pulse.Type,
  CV = T
)

ct_lda <- table(grouping = all.features$Pulse.Type, fit.lda.rfe.all$class)
ct_lda

##### Calculate total percent correct_lda
percent <- sum(diag(prop.table(ct_lda)))
print(percent) 
