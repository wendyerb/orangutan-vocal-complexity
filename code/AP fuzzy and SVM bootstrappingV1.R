### Part 3: Bootstrapping number of training samples and number of features ###

# Load packages
library(ggpubr)    # For creating plots
library(multcomp)  # For multiple comparisons
library(apcluster) # For affinity propagation clustering
library(tidyverse) # For data manipulation and visualization
library(e1071)     # For support vector machine (SVM) classification
library(cluster)   # For cluster analysis
library(clValid)   # For cluster validation

# Set number of samples to iterate over
N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)

# Read in data sheet for affinity
Affinity.rand.df <- read.csv('data_V1/Affinity.rand.df.csv')

# Convert 'n.samples' column to factor and assign levels
Affinity.rand.df$n.samples <- as.factor(Affinity.rand.df$n.samples)
levels(Affinity.rand.df$n.samples) <- N.samples

# Convert 'n.clusters' column to factor
Affinity.rand.df$n.clusters <- as.factor(Affinity.rand.df$n.clusters)

# Add 0 so that all categories are shown
complete.affinity <- complete(Affinity.rand.df, n.samples, n.clusters, fill = list(count = 0))

# Create histogram plot for Affinity clustering
RandomAffinity <- ggpubr::gghistogram(data=complete.affinity,
                                      x='n.samples', group='n.clusters',
                                      fill='n.clusters', stat="count",position="dodge")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_manual(values = matlab::jet.colors(5) )+
  ylab('N iterations')+ xlab('N samples')+
  labs(fill='N clusters')

# Read in data sheet for fuzzy clustering
fuzzy.rand.df <- read.csv('data_V1/fuzzy.rand.df.csv')

# Convert 'n.samples' column to factor and assign levels
fuzzy.rand.df$n.samples <- as.factor(fuzzy.rand.df$n.samples)
levels(fuzzy.rand.df$n.samples) <- N.samples

# Convert 'n.clusters' column to factor
fuzzy.rand.df$n.clusters <- as.factor(fuzzy.rand.df$n.clusters)

# Add 0 so that all categories are shown
complete.fuzzy <- complete(fuzzy.rand.df, n.samples, n.clusters, fill = list(count = 0))

# Create histogram plot for fuzzy clustering
RandomFuzzy <- ggpubr::gghistogram(data=complete.fuzzy,
                                   x='n.samples', group='n.clusters',
                                   fill='n.clusters', stat="count",position="dodge")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_manual(values = matlab::jet.colors(5) )+
  ylab('N iterations')+xlab('N samples')+
  labs(fill='N clusters')

# Create boxplot for fuzzy clustering (mean typicality)
RandomTypicality <- ggerrorplot(data=fuzzy.rand.df, x='n.samples', y='Typicality', outlier.shape = NA)+
  ylab('Mean typicality')+ xlab('N samples')+ylim(0.975,1)

# Read in data sheet for SVM classification
SVM.rand.df <- read.csv('data_V1/SVM.rand.df.csv')

# Convert 'n.samples' column to factor and assign levels
SVM.rand.df$n.samples <- as.factor(SVM.rand.df$n.samples)
levels(SVM.rand.df$n.samples) <- N.samples

# Create boxplot for SVM accuracy
RandomSVM <- ggpubr::ggerrorplot(data=SVM.rand.df,
                               x='n.samples', y='svm.accuracy',outlier.shape = NA)+ ylab('SVM accuracy')+ xlab('N samples')

# Arrange all the plots in a grid
cowplot::plot_grid(RandomAffinity,RandomFuzzy,RandomTypicality,RandomSVM,
                   labels=c('A)', 'B)','C)','D)'),label_x = 0.9)


CombinedRandomFeatures.df <- read.csv('data_V1/CombinedRandomFeatures.df.csv')

N.features <- c(2,4,8,16,32,40)

CombinedRandomFeatures.df$N.features <- as.factor(CombinedRandomFeatures.df$N.features)

levels(CombinedRandomFeatures.df$N.features) <- N.features
CombinedRandomFeatures.df$algorithm <- as.factor(CombinedRandomFeatures.df$algorithm)
levels(CombinedRandomFeatures.df$algorithm) <- c('Affinity','Fuzzy')

ggpubr::ggerrorplot(data=CombinedRandomFeatures.df,
                  x='N.features', y='n.clusters',facet.by ='algorithm' )+
  xlab('N features')+ ylab('N clusters')

CombinedRandomFeatures.df$n.clusters <- as.factor(CombinedRandomFeatures.df$n.clusters)

ggpubr::gghistogram(data=CombinedRandomFeatures.df,
                    x='N.features', group='n.clusters',
                    fill='n.clusters', stat="count",position="dodge",
                    facet.by = 'algorithm',preserve = "single")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_manual(values = matlab::jet.colors(9) )+
  ylab('N iterations')+xlab('N features')+
  labs(fill='N clusters')

# Create confusion matrix for publication
# Create a random 60/40 split
Samples.vec <- sample( c(1:nrow(all.features.svm)), size = nrow(all.features.svm)*0.6, replace = FALSE)
# The 'Samples.vec' vector is created by randomly sampling 60% of the row indices of the 'all.features.svm' dataset without replacement.
# This split is often used for training and testing machine learning models.

all.features.svm.sub <- all.features.svm[Samples.vec,]
all.features.svm.test <- all.features.svm[-Samples.vec,]
# 'all.features.svm.sub' contains the 60% of data points selected for training, and 'all.features.svm.test' contains the remaining 40% for testing.

svm.sig.method.1.all <-
  svm(
    all.features.svm.sub[, 1:46],  # Selecting columns 1 to 46 as features for training
    all.features.svm.sub$Pulse.Type,  # The target variable for training
    kernel = "sigmoid",  # Using the sigmoid kernel for SVM
    # cost = cost.sig.all,  # Optional hyperparameter (not specified in this code)
    # gamma = gamma.sig.all,  # Optional hyperparameter (not specified in this code)
    cross = nrow(all.features.svm.sub) # Setting the 'cross' parameter for cross-validation
    # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
  )

# Predicting the target variable using the SVM model on the test data.
SVMPredictions <- predict(svm.sig.method.1.all,all.features.svm.test[, 1:46])

# Generating a confusion matrix to evaluate the performance of the SVM model on the test data.
ConfMatrix <-caret::confusionMatrix(all.features.svm.test$Pulse.Type,SVMPredictions,'everything')

# Calculating the proportion of correct predictions for each class in the confusion matrix.
ProportionCorrect <- diag(ConfMatrix$table)/rowSums(ConfMatrix$table)

#Creating a data frame from the confusion matrix and adding a column with the percentage of correct predictions.
ConfMatrixDF <- as.data.frame(as.matrix(ConfMatrix))

# Round the proportion values
ConfMatrixDF$Correct <- round(ProportionCorrect,2)

# Print the confusion matrix and saving it as a CSV file.
ConfMatrixDF

write.csv(ConfMatrixDF,'data_V1/ConfusionMatrix.csv')


# Data preparation for bootstrapping --------------------------------------

#### Set working directory
#setwd('/Users/Wendy/github/orangutan-vocal-complexity/data')

####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Create new variable for SVM that keeps pulse type column
all.features.svm <- all.features

# I. Affinity propagation clustering random samples ---------------------------------------------------------------------

# Remove pulse type
all.features <- subset(all.features, select=-c(Pulse.Type))

N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)
N.randomization <-25

Affinity.rand.df <- data.frame()

for(a in 1:length(N.samples)){
for(b in 1:N.randomization){

  Samples.vec <- sample( c(1:nrow(all.features)), size = N.samples[a], replace = FALSE)
  
  all.features.sub <- all.features[Samples.vec,]

# Affinity prop clustering by pulse type with q=0

cluster.dfq0 <- apcluster::apcluster(
  negDistMat(r = 2), q=0,
  all.features.sub,
  maxits = 5000,
  convits = 500,
  nonoise = T
)


if(length(cluster.dfq0@exemplars) >1){
silq0 <- cluster::silhouette(x = cluster.dfq0@idx, dist = dist(all.features.sub))
  
sil.coef <- summary(silq0)$avg.width
sil.coef 

n.clusters <- length(cluster.dfq0@exemplars)

Temp.row <- cbind.data.frame(n.clusters,sil.coef,a,b)
print(Temp.row)
} else{
  Temp.row <- cbind.data.frame(1,NA,a,b)
}

colnames(Temp.row) <- c('n.clusters','sil.coef','n.samples','randomization')
Affinity.rand.df <- rbind.data.frame(Affinity.rand.df,Temp.row)
write.csv(Affinity.rand.df,'data_V1/Affinity.rand.df.csv')  
}
}

Affinity.rand.df <- read.csv('data_V1/Affinity.rand.df.csv')
Affinity.rand.df$n.samples <- as.factor(Affinity.rand.df$n.samples)
levels(Affinity.rand.df$n.samples) <- N.samples

Affinity.rand.df$n.clusters <- as.factor(Affinity.rand.df$n.clusters)

complete.affinity <- complete(Affinity.rand.df, n.samples, n.clusters, fill = list(count = 0))

RandomAffinity <- ggpubr::gghistogram(data=complete.affinity,
                  x='n.samples', group='n.clusters',
                  fill='n.clusters', stat="count",position="dodge")+
                  scale_x_discrete(drop = FALSE)+
                  scale_fill_manual(values = matlab::jet.colors(5) )+
  ylab('N iterations')+ xlab('N samples')+
  labs(fill='N clusters')



# II. Fuzzy random samples---------------------------------------------------------------------
# Read in data
all.features <- read.csv('data_V1/46-features.csv')

# Remove pulse type
all.features <- subset(all.features, select=-c(Pulse.Type))

# Scale all features
z_all.features<-scale(all.features, center = TRUE, scale = TRUE)

N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)

fuzzy.rand.df <- data.frame()

for(b in 1:N.randomization){
for(a in 1:length(N.samples)){

    
    Samples.vec <- sample( c(1:nrow(all.features)), size = N.samples[a], replace = FALSE)
    
    all.features.sub <- z_all.features[Samples.vec,]
    
    ## Interate memb.exp (1.1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5) to find best solution
    
    sil.coef.list <- list()
    for(c in 2:7){
      Fclustoutput <-  fanny(all.features.sub, k = c, memb.exp = 1.1)
      silq0 <- cluster::silhouette(x = Fclustoutput$clustering ,  dist = dist(all.features.sub))
      sil.coef.list[[c]] <- summary(silq0)$avg.width
    }
    
    sil.index <- which.max(unlist(sil.coef.list)[-1])
    
    max.sil <-  sil.coef.list[[(2:7) [sil.index]]]
    NClust <- (2:7) [sil.index]
    
    Fclustoutput <- fanny(all.features.sub, k =  NClust, memb.exp = 1.1)
    
    PulseHardAssignment <- Fclustoutput$clustering
    NPulseHardAssignment <- unique(PulseHardAssignment)
    
   if(length(NPulseHardAssignment) >1){
    
     TypicalityList <- list()
     for(c in 1:nrow( Fclustoutput$membership)){
            MaxMember <-   which.max(Fclustoutput$membership[c,])
            SecondMaxMember <-max(Fclustoutput$membership[c,-MaxMember])
            SecondMaxMember <-  which(Fclustoutput$membership[c,]==SecondMaxMember)
            # Substract second from first
           TypicalityList[[c]] <- Fclustoutput$membership[c,MaxMember] - Fclustoutput$membership[c,SecondMaxMember]
     }
    
     Typicality <- median(unlist(TypicalityList))
      sil.coef <- max.sil
      sil.coef 
      
      n.clusters <- length(NPulseHardAssignment)
      
      Temp.row <- cbind.data.frame(n.clusters,sil.coef,a,b,Typicality)
      print(Temp.row)
    } else{
      Temp.row <- cbind.data.frame(1,NA,a,b,NA)
    }
    
    colnames(Temp.row) <- c('n.clusters','sil.coef','n.samples','randomization','Typicality')
    fuzzy.rand.df <- rbind.data.frame(fuzzy.rand.df,Temp.row)
   write.csv(fuzzy.rand.df,'data_V1/fuzzy.rand.df.csv')  
  }
}

fuzzy.rand.df <- read.csv('data_V1/fuzzy.rand.df.csv')

fuzzy.rand.df$n.samples <- as.factor(fuzzy.rand.df$n.samples)
levels(fuzzy.rand.df$n.samples) <- N.samples

fuzzy.rand.df$n.clusters <- as.factor(fuzzy.rand.df$n.clusters)

complete.fuzzy <- complete(fuzzy.rand.df, n.samples, n.clusters, fill = list(count = 0))

RandomFuzzy <- ggpubr::gghistogram(data=complete.fuzzy,
                    x='n.samples', group='n.clusters',
                    fill='n.clusters', stat="count",position="dodge")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_manual(values = matlab::jet.colors(5) )+
  ylab('N iterations')+xlab('N samples')+
  labs(fill='N clusters')

# Membership coefficients correspond to the degree of being in a given cluster
RandomTypicality <- ggboxplot(data=fuzzy.rand.df, x='n.samples', y='Typicality', outlier.shape = NA)+
  ylab('Mean typicality')+ xlab('N samples')+ylim(0.975,1)


cowplot::plot_grid(RandomAffinity,RandomFuzzy,RandomTypicality,
                   labels=c('A)', 'B)','C)'),label_x = 0.9)
# III. SVM random samples---------------------------------------------------------------------

SVM.rand.df <- data.frame()

for(b in 1:N.randomization){
for(a in 1:length(N.samples)){
  
    Samples.vec <- sample( c(1:nrow(all.features.svm)), size = N.samples[a], replace = FALSE)
    
    all.features.svm.sub <- all.features.svm[Samples.vec,]
    
    
svm.sig.method.1.all <-
  svm(
    all.features.svm.sub[, 1:46],
    all.features.svm.sub$Pulse.Type,
    kernel = "sigmoid",
    # cost = cost.sig.all,
    # gamma = gamma.sig.all,
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
             

# IV. Affinity and fuzzy randomly select features ---------------------------------------------------------------------
####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

# Remove pulse type
all.features <- subset(all.features, select=-c(Pulse.Type))

### Loop to randomize features
CombinedRandomFeatures.df <- data.frame()

N.features <- c(2,4,8,16,32,40)

for(b in 1:(N.randomization)){
for(a in 1:length(N.features)){   
 
  Samples.vec <- sample( c(1:ncol(all.features)), size = N.features[a], replace = FALSE)
  
  all.features.sub <- all.features[,Samples.vec]
  
  # Affinity prop clustering by pulse type with q set to 0
  # q is set to zero based on manual iterations over different values of q
  cluster.dfq0 <- apcluster::apcluster(
    negDistMat(r = 2), q=0,
    all.features.sub,
    maxits = 5000,
    convits = 500,
    nonoise = T
  )
  
  cluster.dfq0
  
  silq0 <- cluster::silhouette(x = cluster.dfq0@idx, dist = dist(all.features.sub))
  summary(silq0)
  
  if(length(cluster.dfq0@exemplars) >1){
    
    sil.coef <- summary(silq0)$avg.width
    sil.coef 
    
    n.clusters <- length(cluster.dfq0@exemplars)
    
    Temp.row.affinity <- cbind.data.frame(n.clusters,sil.coef,a,b)
    print(Temp.row.affinity)
  } else{
    Temp.row.affinity <- cbind.data.frame(1,NA,a,b)
  }
  
  colnames(Temp.row.affinity) <- c('n.clusters','sil.coef','n.samples','randomization')
  
  sil.coef.list <- list()
  for(c in 2:7){
    Fclustoutput <- fanny(all.features.sub, k =  c, memb.exp = 1.1)
    silq0 <- cluster::silhouette(x = Fclustoutput$clustering,  dist = dist(all.features.sub))
    sil.coef.list[[c]] <- summary(silq0)$avg.width
  }
  
  sil.index <- which.max(unlist(sil.coef.list)[-1])
  
  max.sil.fuzzy <-  sil.coef.list[[(2:7) [sil.index]]]
  NClust <- (2:7) [sil.index]
  
  Fclustoutput <- fanny(all.features.sub, k =  NClust, memb.exp = 1.1)
  
  PulseHardAssignment <- Fclustoutput$clustering
  NPulseHardAssignment <- length(unique(PulseHardAssignment))
  
  Temp.row.fuzzy <- cbind.data.frame(NPulseHardAssignment,max.sil.fuzzy,'fuzzy',a,b)
  colnames(Temp.row.fuzzy) <- c('n.clusters','sil.coef','algorithm','N.features','randomization')
  Temp.row.affinity <- cbind.data.frame(n.clusters,sil.coef,'affinity',a,b)
  colnames(Temp.row.affinity) <- c('n.clusters','sil.coef','algorithm','N.features','randomization')
  
  CombinedRandomFeatures.row <- rbind.data.frame(Temp.row.affinity,Temp.row.fuzzy)
  
  CombinedRandomFeatures.df <-  rbind.data.frame(CombinedRandomFeatures.df,CombinedRandomFeatures.row)
  write.csv(CombinedRandomFeatures.df,'data_V1/CombinedRandomFeatures.df.csv')
}
}

CombinedRandomFeatures.df <- read.csv('data_V1/CombinedRandomFeatures.df.csv')

CombinedRandomFeatures.df$N.features <- as.factor(CombinedRandomFeatures.df$N.features)

levels(CombinedRandomFeatures.df$N.features) <- N.features
CombinedRandomFeatures.df$algorithm <- as.factor(CombinedRandomFeatures.df$algorithm)
levels(CombinedRandomFeatures.df$algorithm) <- c('Affinity','Fuzzy')

ggpubr::ggboxplot(data=CombinedRandomFeatures.df,
                  x='N.features', y='n.clusters',facet.by ='algorithm' )+
                  xlab('N features')+ ylab('N clusters')



