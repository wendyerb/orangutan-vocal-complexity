### Part 3: Affinity Propagation ###

library(ggpubr)
library(multcomp)
library(apcluster)
library(tidyverse)

#### Set working directory
setwd('/Users/Wendy/github/orangutan-vocal-complexity/data')

####Read in features 
all.features <- read.csv('46-features.csv')

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))


# Affinity prop clustering by pulse type with q set to 0
cluster.dfq0 <- apcluster::apcluster(
  negDistMat(r = 2), q=0,
  all.features,
  maxits = 5000,
  convits = 500,
  nonoise = T
)

cluster.dfq0

silq0 <- cluster::silhouette(x = cluster.dfq0@idx, dist = dist(all.features[, -c(1:10)]))
summary(silq0)

sil.coef <- summary(silq0)$avg.width
sil.coef        

## create new data frame for adding in cluster ID to feature set
all.features.affinity <- all.features 

## make a new column with the cluster ID (exemplar number)
all.features.affinity$affinity.id <- as.factor(cluster.dfq0@idx)
write.csv(all.features.affinity,'affinity_clusters_46-features.csv',row.names = F)
all.features.affinity <- read.csv('affinity_clusters_46-features.csv')

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


###### Nonparametric testing for feature differences by pulses and clusters
library(FSA)

human.mean.pf=kruskal.test(all.features.affinity$meanpeakf~all.features.affinity$Pulse.Type)
human.mean.pf
PT1 = dunnTest(meanpeakf~Pulse.Type, data=all.features.affinity, method="bh")

human.f75=kruskal.test(all.features.affinity$Freq.75..Hz~all.features.affinity$Pulse.Type)
human.f75
PT2 = dunnTest(Freq.75..Hz~Pulse.Type, data=all.features.affinity, method="bh")
PT2

human.f25=kruskal.test(all.features.affinity$Freq.25..Hz~all.features.affinity$Pulse.Type)
human.f25
PT3 = dunnTest(Freq.25..Hz~Pulse.Type, data=all.features.affinity, method="bh")
PT3

human.centerf=kruskal.test(all.features.affinity$Center.Freq.Hz~all.features.affinity$Pulse.Type)
human.centerf
PT4 = dunnTest(Center.Freq.Hz~Pulse.Type, data=all.features.affinity, method="bh")
PT4


##### Affinity
aff.mean.pf=kruskal.test(all.features.affinity$meanpeakf~all.features.affinity$affinity.id)
aff.mean.pf
PT5 = dunnTest(meanpeakf~affinity.id, data=all.features.affinity, method="bh")
print(PT5,dunn.test.results=TRUE)

aff.f75=kruskal.test(all.features.affinity$Freq.75..Hz~all.features.affinity$affinity.id)
aff.f75
PT6 = dunnTest(Freq.75..Hz~affinity.id, data=all.features.affinity, method="bh")
PT6

aff.f25=kruskal.test(all.features.affinity$Freq.25..Hz~all.features.affinity$affinity.id)
aff.f25
PT7 = dunnTest(Freq.25..Hz~affinity.id, data=all.features.affinity, method="bh")
PT7

aff.centerf=kruskal.test(all.features.affinity$Center.Freq.Hz~all.features.affinity$affinity.id)
aff.centerf
PT8 = dunnTest(Center.Freq.Hz~affinity.id, data=all.features.affinity, method="bh")
PT8

ggboxplot(data=all.features.affinity,x='affinity.id',y='F0_max')

ggboxplot(data=all.features.affinity,x='Pulse.Type',y='Center.Freq.Hz')


### Affinity-Pulse Type Plots
Affinity <- all.features.affinity %>% 
  ggplot(aes(x=affinity.id, fill = Pulse.Type)) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(title = element_text(size = 20)) +
  geom_bar() +
  labs(x = "Affinity cluster", y = "Count", fill = "Pulse")
Affinity


Affinity.Plot <- all.features.affinity %>% 
  ggplot(aes(x=affinity.id, fill = Pulse.Type)) +
  theme_classic() +
  scale_x_continuous(breaks = c(16, 152, 616, 812)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(title = element_text(size = 20)) +
  geom_bar() +
  labs(x = "Cluster number", y = "Count", fill = "Pulse Type", title = "Affinity propagation clusters")
Affinity.Plot


