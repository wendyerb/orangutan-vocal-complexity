### Part 3: Affinity Propagation ###

library(ggpubr)
library(multcomp)
library(apcluster)
library(tidyverse)
library(e1071)

#### Set working directory
#setwd('/Users/Wendy/github/orangutan-vocal-complexity/data')

####Read in features 
all.features <- read.csv('data/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Create new variable for SVM that keeps pulse type column
all.features.svm <- all.features

# Affinity propagation clustering ---------------------------------------------------------------------

# Remove pulse type
all.features <- subset(all.features, select=-c(Pulse.Type))

N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)
N.randomization <- 100

Affinity.rand.df <- data.frame()

for(a in 1:length(N.samples)){
for(b in 1:N.randomization){

  Samples.vec <- sample( c(1:nrow(all.features)), size = N.samples[a], replace = FALSE)
  
  all.features.sub <- all.features[Samples.vec,]

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

Temp.row <- cbind.data.frame(n.clusters,sil.coef,a,b)
print(Temp.row)
} else{
  Temp.row <- cbind.data.frame(1,NA,a,b)
}

colnames(Temp.row) <- c('n.clusters','sil.coef','n.samples','randomization')
Affinity.rand.df <- rbind.data.frame(Affinity.rand.df,Temp.row)
  
}
}

Affinity.rand.df$n.samples <- as.factor(Affinity.rand.df$n.samples)
levels(Affinity.rand.df$n.samples) <- N.samples

ggpubr::ggscatter(data=Affinity.rand.df,
                  x='n.samples', y='n.clusters',position = position_jitter(0.00001))

ggpubr::ggboxplot(data=Affinity.rand.df,
                  x='n.samples', y='n.clusters',outlier.shape = NA)

ggpubr::ggviolin(data=Affinity.rand.df,
                  x='n.samples', y='n.clusters')

# DBSCAN ---------------------------------------------------------------------
library(dbscan)
cl <- hdbscan(all.features, minPts = 10)
cl

UMAPout <- umap::umap(all.features)
plot(UMAPout$layout, col=cl$cluster+1, pch=20)

# Randomly select features ---------------------------------------------------------------------
####Read in features 
all.features <- read.csv('data/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

# Remove pulse type
all.features <- subset(all.features, select=-c(Pulse.Type))

### Loop to randomize
HDBSCAN.feature.df <- data.frame()

N.features <- c(2,4,8,16,32,40)
N.randomization.HDBSCAN <- 25

for(b in 1:(N.randomization.HDBSCAN)){
  for(a in 1:length(N.features)){   
    
    Samples.vec <- sample( c(1:ncol(all.features)), size = N.features[a], replace = FALSE)
    
    all.features.sub <- all.features[,Samples.vec]
    
    # HDBSCAN prop clustering by pulse type with q set to 0
    # q is set to zero based on manual iterations over different values of q
    cl <- hdbscan(all.features.sub, minPts = N.features[a])
    unique(cl$cluster)
    
    
    silq0 <- cluster::silhouette(x = cl$cluster, dist = dist(all.features.sub))
    summary(silq0)
    
    if(length(unique(cl$cluster)) >1){
      
      sil.coef <- summary(silq0)$avg.width
      sil.coef 
      
      n.clusters <- length(unique(cl$cluster))
      
      Temp.row <- cbind.data.frame(n.clusters,sil.coef,a,b)
      print(Temp.row)
    } else{
      Temp.row <- cbind.data.frame(1,NA,a,b)
    }
    
    colnames(Temp.row) <- c('n.clusters','sil.coef','n.samples','randomization')
    HDBSCAN.feature.df <-  rbind.data.frame(HDBSCAN.feature.df,Temp.row)
    
  }
}


HDBSCAN.feature.df$n.features <- as.factor(HDBSCAN.feature.df$n.samples)
levels(HDBSCAN.feature.df$n.features) <- N.features

ggpubr::ggviolin(data=HDBSCAN.feature.df,
                 x='n.features', y='n.clusters')

table(HDBSCAN.feature.df$n.clusters,HDBSCAN.feature.df$n.features)

# SVM ---------------------------------------------------------------------

N.samples.svm <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)
N.randomization.svm <- 25


SVM.rand.df <- data.frame()

for(a in 1:length(N.samples.svm)){
  for(b in 1:N.randomization.svm){
    
    Samples.vec <- sample( c(1:nrow(all.features.svm)), size = N.samples.svm[a], replace = FALSE)
    
    all.features.svm.sub <- all.features.svm[Samples.vec,]
    
# 
# tune.sig.method.1.all <-
#   tune(
#     svm,
#     all.features.svm.sub[, 1:46],
#     all.features.svm.sub$Pulse.Type,
#     kernel = "sigmoid",
#     ranges = list(
#       cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
#       gamma = c(0.001, 0.01, 0.1, 0.5, 1.0, 2.0)
#     )
#   )
# 
# cost.sig.all <- tune.sig.method.1.all$best.parameters$cost
# cost.sig.all ## 100
# gamma.sig.all <-  tune.sig.method.1.all$best.parameters$gamma
# gamma.sig.all ## 0.001

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
  }
}

SVM.rand.df$n.samples <- as.factor(SVM.rand.df$n.samples)
levels(SVM.rand.df$n.samples) <- N.samples.svm

ggpubr::ggboxplot(data=SVM.rand.df,
                  x='n.samples', y='svm.accuracy',outlier.shape = NA)

