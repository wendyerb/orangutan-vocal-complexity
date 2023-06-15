### Part 3: Affinity Propagation ###

library(ggpubr)
library(multcomp)
library(apcluster)
library(tidyverse)
library(e1071)
library(cluster)
library(clValid)

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
N.randomization <- 25

Affinity.rand.df <- data.frame()

for(a in 1:length(N.samples)){
for(b in 1:N.randomization){

  Samples.vec <- sample( c(1:nrow(all.features)), size = N.samples[a], replace = FALSE)
  
  all.features.sub <- all.features[Samples.vec,]

# Affinity prop clustering by pulse type with iterative q
qvals <- seq(0.1,1,0.1)

sil.df <- data.frame()

for(c in 1:length(qvals)){
cluster.dfq0 <- apcluster::apcluster(
  negDistMat(r = 2), q=qvals[c],
  all.features.sub,
  maxits = 5000,
  convits = 500,
  nonoise = T
)



silq0 <- cluster::silhouette(x = cluster.dfq0@idx, dist = dist(all.features.sub))
sil.coef <- summary(silq0)$avg.width
qval <- qvals[c]
TempRow <- cbind.data.frame(sil.coef,qval)
sil.df <- rbind.data.frame(sil.df,TempRow)
}

maxq <- which.max(sil.df$sil.coef)

cluster.dfq0 <- apcluster::apcluster(
  negDistMat(r = 2), q=qvals[maxq],
  all.features.sub,
  maxits = 5000,
  convits = 500,
  nonoise = T
)


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
write.csv(Affinity.rand.df,'data/Affinity.rand.df.csv')  
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


# Fuzzy ---------------------------------------------------------------------
# Read in data
all.features <- read.csv('data/46-features.csv')

# Remove pulse type
all.features <- subset(all.features, select=-c(Pulse.Type))

# Scale all features
z_all.features<-scale(all.features, center = TRUE, scale = TRUE)


N.samples <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)
N.randomization <- 25

fuzzy.rand.df <- data.frame()

for(a in 1:length(N.samples)){
  for(b in 1:N.randomization){
    
    Samples.vec <- sample( c(1:nrow(all.features)), size = N.samples[a], replace = FALSE)
    
    all.features.sub <- z_all.features[Samples.vec,]
    
    ## Interate memb.exp (1.1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5) to find best solution
    
    sil.coef.list <- list()
    for(c in 1:length(mebexp.vals)){
    k2u11 <-  clValid(all.features.sub, 2:7, clMethods=c("fanny"),
              validation="internal")
    sil.coef <- k2u11$silinfo$avg.width
    sil.coef.list[[c]] <- sil.coef
    }
    
    sil.index <- which.max(unlist(sil.coef.list))
    max.sil <-  sil.coef.list[[which.max(unlist(sil.coef.list))]]
    k2u11 <- fanny(all.features.sub, k =k.vals[c] , memb.exp =1)
    PulseHardAssignment <- k2u11$clustering
    NPulseHardAssignment <- unique(PulseHardAssignment)
    
    
    if(length(NPulseHardAssignment) >1){
      
      sil.coef <- max.sil
      sil.coef 
      
      n.clusters <- length(NPulseHardAssignment)
      
      Temp.row <- cbind.data.frame(n.clusters,sil.coef,a,b)
      print(Temp.row)
    } else{
      Temp.row <- cbind.data.frame(1,NA,a,b)
    }
    
    colnames(Temp.row) <- c('n.clusters','sil.coef','n.samples','randomization')
    fuzzy.rand.df <- rbind.data.frame(fuzzy.rand.df,Temp.row)
    write.csv(fuzzy.rand.df,'data/fuzzy.rand.df.csv')  
  }
}

fuzzy.rand.df$n.samples <- as.factor(fuzzy.rand.df$n.samples)
levels(fuzzy.rand.df$n.samples) <- N.samples

ggpubr::ggscatter(data=fuzzy.rand.df,
                  x='n.samples', y='n.clusters',position = position_jitter(0.00001))

ggpubr::ggboxplot(data=fuzzy.rand.df,
                  x='n.samples', y='n.clusters',outlier.shape = NA)

ggpubr::ggviolin(data=fuzzy.rand.df,
                 x='n.samples', y='n.clusters')

# SVM ---------------------------------------------------------------------

N.samples.svm <- c(100, 200, 300, 400, 500, 600, 700, 800, 900)


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
                  x='n.samples', y='svm.accuracy',outlier.shape = NA)+stat_compare_means(aes(label = after_stat(p.signif)),method = "t.test", ref.group = "900")



# Randomly select features ---------------------------------------------------------------------
####Read in features 
all.features <- read.csv('data/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

####Make pulse type a factor
all.features$Pulse.Type <- factor(all.features$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

### Loop to randomize
Affinity.feature.df <- data.frame()

N.features <- c(2,4,8,16,32,40)
N.randomization.affinity <- 25

for(b in 12:(N.randomization.affinity)){
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
    
    Temp.row <- cbind.data.frame(n.clusters,sil.coef,a,b)
    print(Temp.row)
  } else{
    Temp.row <- cbind.data.frame(1,NA,a,b)
  }
  
  colnames(Temp.row) <- c('n.clusters','sil.coef','n.samples','randomization')
  Affinity.feature.df <-  rbind.data.frame(Affinity.feature.df,Temp.row)
  
}
}


Affinity.feature.df$n.features <- as.factor(Affinity.feature.df$n.samples)
levels(Affinity.feature.df$n.features) <- N.features

ggpubr::ggviolin(data=Affinity.feature.df,
                  x='n.features', y='n.clusters')


