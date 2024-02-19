# Load packages
library(ggpubr)   
library(matlab)
library(tidyr)

# Read in data sheet for affinity
Affinity.rand.df <- read.csv('data_V1/Affinity.rand.df.csv')

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


## Figure S4
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
