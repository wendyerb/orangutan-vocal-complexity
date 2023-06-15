### Part 5: UMAP ###
library(umap)
library(tidyverse)
library(factoextra)

#### Set working directory
#setwd('/Users/Wendy/github/orangutan-vocal-complexity/data')

####Read in features 
all.features <- read.csv('data/46-features.csv')
all.features.affinity <- read.csv('data/affinity_clusters.csv')
all.features.fuzzy <- read.csv('data/fanny_typicality.csv')
raw.spectrograms <- read.csv('data/raw_spectrogram_umap_dimensions.csv')


## Create UMAP with affinity propagation clusters labeled
OrangPulse.umap <- 
  umap::umap(all.features[, 1:46],
             controlscale=TRUE,scale=3)


plot.for.OrangPulseAffinity <-
  cbind.data.frame(OrangPulse.umap$layout[,1:2],
                   as.factor(all.features.affinity$affinity.id))


colnames(plot.for.OrangPulseAffinity) <-
  c("Dim.1", "Dim.2", "Cluster")

myplot.for.OrangPulseAffinity <-
  ggplot(data = plot.for.OrangPulseAffinity, aes(
    x = Dim.1,
    y = Dim.2,
    colour = Cluster
  )) +
  geom_point(size = 1.5) +
  theme_classic() + ggtitle('Affinity propagation') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
  

myplot.for.OrangPulseAffinity


## Create UMAP with typical fuzzy clusters labeled

plot.for.OrangPulseFuzzy <-
  cbind.data.frame(OrangPulse.umap$layout[,1:2],
                   as.factor(all.features.fuzzy$Cluster), as.factor(all.features.fuzzy$Typical))

colnames(plot.for.OrangPulseFuzzy) <-
  c("Dim.1", "Dim.2", "Cluster", "Typical")

myplot.for.OrangPulseFuzzy <-
  ggplot(data = plot.for.OrangPulseFuzzy, aes(
    x = Dim.1,
    y = Dim.2,
    colour = Cluster, 
    alpha = Typical
  )) +
  geom_point(size = 1.5) +
  theme_classic() + ggtitle('Fuzzy cluster') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + labs(fill = "Cluster")

myplot.for.OrangPulseFuzzy

## Create UMAP based on 46 features with human (A/V) labels

all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

plot.for.OrangPulseHumanID <-
  cbind.data.frame(OrangPulse.umap$layout[,1:2],
                   as.factor(all.features$Pulse.Type))

colnames(plot.for.OrangPulseHumanID) <-
  c("Dim.1", "Dim.2", "Pulse")

myplot.for.OrangPulseHumanID <-
  ggplot(data = plot.for.OrangPulseHumanID, aes(
    x = Dim.1,
    y = Dim.2,
    colour = Pulse
  )) +
  geom_point(size = 1.5) +
  theme_classic() + ggtitle('Audio-visual: feature set') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) 

myplot.for.OrangPulseHumanID

## Create UMAP based on raw spectrograms with raw spectrograms
raw.spectrograms$Pulse <- factor(raw.spectrograms$Pulse, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

myplot.for.OrangPulseSpectrograms <-
  ggplot(data = raw.spectrograms, aes(
    x = Dim.1,
    y = Dim.2,
    colour = Pulse
  )) +
  geom_point(size = 1.5) +
  theme_classic() + ggtitle('Audio-visual: raw spectrogram') + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

myplot.for.OrangPulseSpectrograms

## Cowplot for all UMAP plots

cowplot::plot_grid(myplot.for.OrangPulseAffinity, myplot.for.OrangPulseFuzzy, myplot.for.OrangPulseHumanID, myplot.for.OrangPulseSpectrograms, align = "none")


# Hopkins Statistic -------------------------------------------------------
## 46 features
umap.dimensions.features <- OrangPulse.umap$layout[,1:2]

hopkins.features <- factoextra::get_clust_tendency(umap.dimensions.features, n = nrow(umap.dimensions.features)-1, graph = FALSE)
hopkins.features$hopkins_stat  ## 0.941246
## If the value of Hopkins statistic is close to 1 (far above 0.5), then we can conclude that the dataset is significantly clusterable

## raw spectrograms
umap.dimensions.spectrograms <- raw.spectrograms[,2:3]


hopkins.raw <- factoextra::get_clust_tendency(umap.dimensions.spectrograms, n = nrow(umap.dimensions.spectrograms)-1, graph = FALSE)
hopkins.raw$hopkins_stat  ## 0.9570386
