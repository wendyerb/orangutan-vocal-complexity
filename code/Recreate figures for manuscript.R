# Load packages
library(ggpubr)   
library(matlab)
library(tidyr)
library(e1071)
library(tidyverse)
library(ggplot2)
library(umap)
library(factoextra)

# Figure 3- Barplot of classification accuracy (AV vs. SVM) for original pulse  --------
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
png(file="/Users/wme8/Library/CloudStorage/Dropbox/Manuscripts/Submitted/Long Call Classification/PeerJ/Peer J Revision 2/New Figs 18Mar24/Figure3.png",
    width=900, height=900)
  ggplot(data = CombinedAV, aes(
    fill = Method,
    x = Pulse.Type,
    y = perc.accuracy
  )) + ylim(0, 100) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer() + 
  theme_classic(base_size = 18) + 
  xlab('Pulse type') + ylab('Classification accuracy (%)') + labs(fill = "Method")
dev.off()

# Figure 4: Affinity propagation ------------------------------------------
all.features.affinity <- read.csv('data_V1/affinity_clusters.csv')
all.features.affinity$affinity.id <- as.factor(all.features.affinity$affinity.id)


cbPalette <- c("#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", "#1b7837")

# Create a stacked bar plot
png(file="/Users/wme8/Library/CloudStorage/Dropbox/Manuscripts/Submitted/Long Call Classification/PeerJ/Peer J Revision 2/New Figs 18Mar24/Figure4.png",
    width=900, height=900)
all.features.affinity %>% 
  ggplot(aes(x=affinity.id, fill = Pulse.Type)) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(title = element_text(size = 20)) +
  geom_bar() + scale_fill_manual(values = cbPalette) +
  labs(x = "Affinity cluster", y = "Count", fill = "Pulse")
dev.off()



# Figure 5: Typicality coefficients  ------------------------------------------
Typicality <- read.csv('data_V1/fanny_typicality.csv')
Typicality$Pulse.Type <- factor(Typicality$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

Fig5a <- Typicality %>% 
  ggplot(aes(x=Typicality.d., fill = Pulse.Type)) + 
  theme_classic() +
  geom_histogram(binwidth = 0.1) + scale_fill_manual(values = cbPalette) +
  labs(x = "Typicality coefficient", y = "Count", fill = "Pulse")

Fig5b <- Typicality %>% 
  ggplot(aes(x=Typicality.d., y=Pulse.Type, fill=Pulse.Type)) + scale_fill_manual(values = cbPalette) + 
  geom_boxplot(outlier.shape=1) +
  theme_classic() +
  scale_x_continuous(breaks = c(0,0.25, 0.5, 0.75, 1.0)) +
  # theme(axis.text = element_text(size=14)) +
  #theme(axis.title = element_text(size = 20)) +
  #theme(legend.text = element_text(size = 14)) +
  #theme(legend.title = element_text(size = 20)) +
  scale_y_discrete(limits=rev) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x = "Typicality coefficient", y = NULL, fill="Pulse") 

# Arrange figures 5a-5b in a grid and save
png(file="/Users/wme8/Library/CloudStorage/Dropbox/Manuscripts/Submitted/Long Call Classification/PeerJ/Peer J Revision 2/New Figs 18Mar24/Figure5.png",
    width=900, height=900)
cowplot::plot_grid(Fig5a,Fig5b,
                   labels=c('a)', 'b)'),label_x = 0.8)
dev.off()


# Figure 6 Typical calls from fuzzy clustering ------------------------------------------
Typical <- Typicality %>% 
  filter(Cluster_Typical == "1" | Cluster_Typical == "2")

Fig6a <- Typical %>% 
  ggplot(aes(x=Pulse.Type, fill=as.factor(Cluster_Typical))) + scale_fill_manual(values = cbPalette) +
  theme_classic() +
  #theme(axis.text = element_text(size = 14)) +
  #theme(axis.title = element_text(size = 20)) +
  #theme(legend.text = element_text(size = 14)) +
  #theme(legend.title = element_text(size = 20)) +
  geom_bar(position="fill") +
  guides(fill=guide_legend(title='Cluster')) +
  labs(x = "Pulse type", y = "Proportion")

Fig6b <- Typical %>% 
  ggplot(aes(x=Cluster_Typical, fill = Pulse.Type)) +
  theme_classic() +
  #theme(axis.text = element_text(size = 14)) +
  #theme(axis.title = element_text(size = 20)) +
  #theme(legend.text = element_text(size = 14)) +
  #theme(legend.title = element_text(size = 20)) +
  stat_bin(binwidth = 1) + scale_fill_manual(values = cbPalette) +
  labs(x = "Typicality coefficient", y = "Count", fill = "Pulse")

# Arrange figures 6a-5b in a grid
# Arrange figures 5a-5b in a grid and save
png(file="/Users/wme8/Library/CloudStorage/Dropbox/Manuscripts/Submitted/Long Call Classification/PeerJ/Peer J Revision 2/New Figs 18Mar24/Figure6.png",
    width=900, height=900)
cowplot::plot_grid(Fig6a,Fig6b,
                   labels=c('a)', 'b)'),label_x = 0.8)


# Figure 7 UMAP Plot ------------------------------------------------------
# See UMAP.R script


# Figure 8 Barplot of classification accuracy for revised pulse scheme --------

all.features.updated <- read.csv('data_V1/500_pulses_new_classes_46features.csv')

av.classification.updated <- read.csv('data_V1/SVM-AV_NewPulse_Accuracy.csv')

#### Check distribution of pulse types
table(all.features.updated$New.Pulse)

####Make pulse type a factor
all.features.updated$Pulse.Type <- factor(all.features.updated$New.Pulse, levels = c("R", "I", "S"))

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
CombinedAV.updated <- rbind.data.frame(SVMdf,av.classification.updated[4:6,])


# Create a bar plot
png(file="/Users/wme8/Library/CloudStorage/Dropbox/Manuscripts/Submitted/Long Call Classification/PeerJ/Peer J Revision 2/New Figs 18Mar24/Figure8.png",
    width=900, height=900)
ggplot(data = CombinedAV.updated, aes(
  fill = Method,
  x = Pulse.Type,
  y = perc.accuracy
)) + ylim(0, 100) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_brewer() + 
  theme_classic(base_size = 18) + 
  xlab('Pulse type') + ylab('Classification accuracy (%)') + labs(fill = "Method")
dev.off()

# Create a bar plot
ggbarplot(data=CombinedAV.updated,x='Pulse.Type',y='perc.accuracy',
          fill='Method', position = position_dodge(0.9), palette = "Paired")+
  ylab('Classification Accuracy')+ xlab('Pulse Type')

# Figure S2. Boxplots of features that differed across human - labels  --------

all.features.affinity <- read.csv('data_V1/affinity_clusters.csv')

all.features.affinity$Pulse.Type <- factor(all.features.affinity$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))
all.features.affinity$affinity.id <- as.factor(all.features.affinity$affinity.id)

##### Boxplots of features by human labels and affinity propagation clusters
aff.cent <- ggboxplot(data=all.features.affinity,x='affinity.id',y='Center.Freq.Hz', font.label = list(size = 16, face = "plain"), xlab="Affinity Cluster", ylab="Center Frequency [Hz]")

aff.mean.pf <- ggboxplot(data=all.features.affinity,x='affinity.id',y='meanpeakf', xlab="Affinity Cluster", ylab="Mean Peak Frequency [Hz]")

aff.max.pf <- ggboxplot(data=all.features.affinity,x='affinity.id',y='Peak.Freq.Hz', xlab="Affinity Cluster", ylab="Frequency with Max Power [Hz]")

aff.75 <- ggboxplot(data=all.features.affinity,x='affinity.id',y='Freq.75..Hz', xlab="Affinity Cluster", ylab="3rd Quartile Frequency [Hz]")

aff.25 <- ggboxplot(data=all.features.affinity,x='affinity.id',y='Freq.25..Hz', xlab="Affinity Cluster", ylab="1st Quartile Frequency [Hz]")

hum.cent <- ggboxplot(data=all.features.affinity,x='Pulse.Type',y='Center.Freq.Hz', xlab="Human Label", ylab=F)

hum.mean.pf <- ggboxplot(data=all.features.affinity,x='Pulse.Type',y='meanpeakf', xlab="Human Label", ylab=F)

hum.max.pf <- ggboxplot(data=all.features.affinity,x='Pulse.Type',y='Peak.Freq.Hz', xlab="Human Label", ylab=F)

hum.75 <- ggboxplot(data=all.features.affinity,x='Pulse.Type',y='Freq.75..Hz', xlab="Human Label", ylab=F)

hum.25 <- ggboxplot(data=all.features.affinity,x='Pulse.Type',y='Freq.25..Hz', xlab="Human Label", ylab=F)

cowplot::plot_grid(aff.cent,hum.cent)
cowplot::plot_grid(aff.mean.pf,hum.mean.pf)
cowplot::plot_grid(aff.max.pf,hum.max.pf)
cowplot::plot_grid(aff.75,hum.75)
cowplot::plot_grid(aff.25,hum.25)



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
