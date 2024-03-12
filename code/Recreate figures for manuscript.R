# Load packages
library(ggpubr)   
library(matlab)
library(tidyr)
library(e1071)

# Figure 3- Barplot of classification accuracy for original pulse  --------
all.features <- read.csv('data_V1/46-features.csv')

av.classification <- read.csv('data_V1/SVM-AV_Accuracy_10Jan24.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Run SVM
svm.sig.method.1.all <-
  svm(
    all.features[, 1:46],  # Selecting columns 1 to 46 as features for training
    all.features$Pulse.Type,  # The target variable for training
    kernel = 'linear',  # Using the polynomial kernel for SVM
    cross = nrow(all.features) # Setting the 'cross' parameter for cross-validation
    # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
  )

# Predict based on trained model
SVMPredictions <- predict(svm.sig.method.1.all,all.features[, 1:46])

# Create confusion matrix
table(all.features$Pulse.Type,SVMPredictions)

# Calculate percent correct
diag(table(all.features$Pulse.Type,SVMPredictions))/table(all.features$Pulse.Type)

# Create vector with unique pulse types
Pulse.Type <- unique(all.features$Pulse.Type)

# Calculate the percent accuracy
perc.accuracy <- as.numeric(diag(table(all.features$Pulse.Type,SVMPredictions))/table(all.features$Pulse.Type)*100)

# Create a vector with SVM for Method column
Method <- rep('SVM',length(unique(all.features$Pulse.Type)))

# Combine into a new dataframe
SVMdf <- cbind.data.frame(Pulse.Type,perc.accuracy,Method)

# Combine new SVM results with previous AV
CombinedAV <- rbind.data.frame(SVMdf,av.classification[7:12,])

# Create a bar plot
ggbarplot(data=CombinedAV,x='Pulse.Type',y='perc.accuracy',
          fill='Method', position = position_dodge(0.9), palette = "Paired")+
            ylab('Classification Accuracy')+ xlab('Pulse Type')



# Plots 4-6 need code from WENDY ------------------------------------------


# Figure 7 UMAP Plot ------------------------------------------------------
# See UMAP.R script


# Figure 8 Barplot of classification accuracy for revised pulse scheme --------

all.features.updated <- read.csv('data_V1/500_pulses_new_classes_46features.csv')

av.classification.updated <- read.csv('data_V1/SVM-AV_NewPulse_Accuracy.csv')

#### Check distribution of pulse types
table(all.features.updated$New.Pulse)

####Make pulse type a factor
all.features.updated$Pulse.Type <- as.factor(all.features.updated$New.Pulse)

# Run SVM
svm.sig.method.1.all <-
  svm(
    all.features.updated[, 15:50],  # Selecting columns 1 to 46 as features for training
    all.features.updated$Pulse.Type,  # The target variable for training
    kernel = 'linear',  # Using the polynomial kernel for SVM
    cross = nrow(all.features.updated) # Setting the 'cross' parameter for cross-validation
    # This is not used for the final model but indicates leave-one-out cross-validation, which means one data point is left out as a test set in each iteration.
  )

# Predict based on trained model
SVMPredictions <- predict(svm.sig.method.1.all,all.features.updated[, 15:50])

# Create confusion matrix
table(all.features.updated$Pulse.Type,SVMPredictions)

# Calculate percent correct
diag(table(all.features.updated$Pulse.Type,SVMPredictions))/table(all.features.updated$Pulse.Type)

# Create vector with unique pulse types
Pulse.Type <- unique(all.features.updated$Pulse.Type)

# Calculate the percent accuracy
perc.accuracy <- as.numeric(diag(table(all.features.updated$Pulse.Type,SVMPredictions))/table(all.features.updated$Pulse.Type)*100)

# Create a vector with SVM for Method column
Method <- rep('SVM',length(unique(all.features.updated$Pulse.Type)))

# Combine into a new dataframe
SVMdf <- cbind.data.frame(Pulse.Type,perc.accuracy,Method)

# Combine new SVM results with previous AV
CombinedAV <- rbind.data.frame(SVMdf,av.classification.updated[4:6,])

# Create a bar plot
ggbarplot(data=CombinedAV,x='Pulse.Type',y='perc.accuracy',
          fill='Method', position = position_dodge(0.9), palette = "Paired")+
  ylab('Classification Accuracy')+ xlab('Pulse Type')

# Figure S2. Boxplots of features that differed across human - labels WENDY --------
# Need from Wendy

# Figure S3 ---------------------------------------------------------------
# Read in data sheet for affinity
Affinity.rand.df <- read.csv('data_V1/Affinity.rand.df.csv')

N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)

# Set color scheme
color.palette <- matlab::jet.colors(7)

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
  scale_fill_manual(values = color.palette)+
  ylab('N iterations')+ xlab('N observations')+
  labs(fill='N clusters')

RandomAffinity

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
  scale_fill_manual(values = color.palette)+
  ylab('N iterations')+xlab('N observations')+
  labs(fill='N clusters')

RandomFuzzy

# Create boxplot for fuzzy clustering (mean typicality)
RandomTypicality <- ggerrorplot(data=fuzzy.rand.df, x='n.samples', y='Typicality', outlier.shape = NA)+
  ylab('Mean typicality coefficient')+ xlab('N observations')+ylim(0.995,1)

RandomTypicality

# Read in data sheet for SVM classification
SVM.rand.df <- read.csv('data_V1/SVM.rand.df.csv')

# Convert 'n.samples' column to factor and assign levels
SVM.rand.df$n.samples <- as.factor(SVM.rand.df$n.samples)
levels(SVM.rand.df$n.samples) <- N.samples

# Create boxplot for SVM accuracy
RandomSVM <- ggpubr::ggerrorplot(data=SVM.rand.df,
                                 x='n.samples', y='svm.accuracy',outlier.shape = NA)+ ylab('SVM accuracy (%)')+ xlab('N observations')

RandomSVM

# Arrange all the plots in a grid
cowplot::plot_grid(RandomAffinity,RandomFuzzy,RandomTypicality,RandomSVM,
                   labels=c('a)', 'b)','c)','d)'),label_x = 0.9)



# ## Figure S4 -------------------------------------------------------------------------

CombinedRandomFeatures.df <- read.csv('data_v1/CombinedRandomFeatures.df.csv')

N.features <- c(2,4,8,16,32,40)

CombinedRandomFeatures.df$N.features <- as.factor(CombinedRandomFeatures.df$N.features)

levels(CombinedRandomFeatures.df$N.features) <- N.features
CombinedRandomFeatures.df$algorithm <- as.factor(CombinedRandomFeatures.df$algorithm)
levels(CombinedRandomFeatures.df$algorithm) <- c('Affinity Propagation','Fuzzy Clustering')


CombinedRandomFeatures.df$n.clusters <- as.factor(CombinedRandomFeatures.df$n.clusters)

ggpubr::gghistogram(data=CombinedRandomFeatures.df,
                    x='N.features', group='n.clusters',
                    fill='n.clusters', stat="count",position="dodge",
                    facet.by = 'algorithm',preserve = "single")+
  scale_x_discrete(drop = FALSE)+
  scale_fill_manual(values = color.palette)+
  ylab('N iterations')+xlab('N features')+
  labs(fill='N clusters')
