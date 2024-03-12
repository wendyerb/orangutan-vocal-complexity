# Load packages
library(ggpubr)    # For creating plots
library(multcomp)  # For multiple comparisons
library(apcluster) # For affinity propagation clustering
library(tidyverse) # For data manipulation and visualization
library(e1071)     # For support vector machine (SVM) classification
library(cluster)   # For cluster analysis
library(clValid)   # For cluster validation


# Part 1. Affinity over all features and observations -----------------------------
####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

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

silq0 <- cluster::silhouette(x = cluster.dfq0@idx, dist = dist(all.features[, -c(47:48)]))
summary(silq0)

sil.coef <- summary(silq0)$avg.width
sil.coef        

## create new data frame for adding in cluster ID to feature set
all.features.affinity <- all.features 

## make a new column with the cluster ID (exemplar number)
all.features.affinity$affinity.id <- as.factor(cluster.dfq0@idx)
#write.csv(all.features.affinity,'affinity_clusters.csv',row.names = F)

# Part 2. Fuzzy with all features and observations --------------------------------

####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

## Read in and scale data
z_all.features<-scale(all.features[1:46], center = TRUE, scale = TRUE)

## Vary k from 2 to 7 using fuzzy 'fanny' clustering in clValid package
## Internal Validation
intvalid <- clValid(z_all.features, 2:7, clMethods=c("fanny"),
                    validation="internal")
summary(intvalid)

# Return a hard cluster based on highest membership
attr(intvalid@measures)

## Stability
stable <- clValid(z_all.features, 2:7, clMethods=c("fanny"),
                  validation="stability", maxitems = 1000)
summary(stable)

## Based on the internal validation and stability analysis, the best cluster solution is K=2

## Interate memb.exp (1.1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5) to find best solution
k2u11 <- fanny(z_all.features, k = 2, memb.exp = 1.1)
summary(k2u11)
k2u11$membership

# Export chosen element of output to CSV
#write.csv(k2u11$clustering, file = "K2U11_cluster.csv")

# Part 3. Affinity propagation clustering random samples ---------------------------------------------------------------------

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
    #write.csv(Affinity.rand.df,'data_V1/Affinity.rand.df.csv')  
  }
}

# Part 4. Fuzzy random samples---------------------------------------------------------------------
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
    #write.csv(fuzzy.rand.df,'data_V1/fuzzy.rand.df.csv')  
  }
}


# Part 5. Affinity and fuzzy randomly select features ---------------------------------------------------------------------
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
    #write.csv(CombinedRandomFeatures.df,'data_V1/CombinedRandomFeatures.df.csv')
  }
}



# Part 6. Balanced data across pulse types ----------------------------------------------------------------

####Read in features 
all.features <- read.csv('data_V1/46-features.csv')

#### Check distribution of pulse types
table(all.features$Pulse.Type)

### Loop to randomize evenly balanced samples
CombinedRandomFeatures.df.balanced <- data.frame()

# Remove pulse type
N.randomization <- 25

N.samples <- min(table(all.features$Pulse.Type))

Unique.pulsetype <- unique(all.features$Pulse.Type)

for(b in 1:(N.randomization)){
  
  CombinedBalancedPulseTypeDF <- data.frame()
  
  for(a in 1:length(Unique.pulsetype)){   
    
    TempSubsetPulse <- subset(all.features,Pulse.Type==Unique.pulsetype[a])
    
    Samples.vec <- sample( c(1:nrow(TempSubsetPulse)), size = N.samples, replace = FALSE)
    
    all.features.temp <- TempSubsetPulse[Samples.vec,]
    
    CombinedBalancedPulseTypeDF <- rbind.data.frame(CombinedBalancedPulseTypeDF,all.features.temp)
  }
  
  print(table(CombinedBalancedPulseTypeDF$Pulse.Type))
  
  CombinedBalancedPulseTypeDF <- subset(CombinedBalancedPulseTypeDF, select=-c(Pulse.Type))
  
  # Affinity prop clustering by pulse type with q set to 0
  # q is set to zero based on manual iterations over different values of q
  cluster.dfq0 <- apcluster::apcluster(
    negDistMat(r = 2), q=0,
    CombinedBalancedPulseTypeDF,
    maxits = 5000,
    convits = 500,
    nonoise = T
  )
  
  #cluster.dfq0
  
  silq0 <- cluster::silhouette(x = cluster.dfq0@idx, dist = dist(CombinedBalancedPulseTypeDF))
  summary(silq0)
  
  if(length(cluster.dfq0@exemplars) >1){
    
    sil.coef <- summary(silq0)$avg.width
    sil.coef 
    
    n.clusters <- length(cluster.dfq0@exemplars)
    
    Temp.row.affinity <- cbind.data.frame(n.clusters,sil.coef,b)
    print(Temp.row.affinity)
  } else{
    Temp.row.affinity <- cbind.data.frame(1,NA,b)
  }
  
  colnames(Temp.row.affinity) <- c('n.clusters','sil.coef','randomization')
  
  sil.coef.list <- list()
  for(c in 2:7){
    Fclustoutput <- fanny(CombinedBalancedPulseTypeDF, k =  c, memb.exp = 1.1)
    silq0 <- cluster::silhouette(x = Fclustoutput$clustering,  dist = dist(CombinedBalancedPulseTypeDF))
    sil.coef.list[[c]] <- summary(silq0)$avg.width
  }
  
  sil.index <- which.max(unlist(sil.coef.list)[-1])
  
  max.sil.fuzzy <-  sil.coef.list[[(2:7) [sil.index]]]
  NClust <- (2:7) [sil.index]
  
  Fclustoutput <- fanny(CombinedBalancedPulseTypeDF, k =  NClust, memb.exp = 1.1)
  
  PulseHardAssignment <- Fclustoutput$clustering
  NPulseHardAssignment <- length(unique(PulseHardAssignment))
  
  Temp.row.fuzzy <- cbind.data.frame(NPulseHardAssignment,max.sil.fuzzy,'fuzzy',a,b)
  colnames(Temp.row.fuzzy) <- c('n.clusters','sil.coef','algorithm','N.features','randomization')
  Temp.row.affinity <- cbind.data.frame(n.clusters,sil.coef,'affinity',a,b)
  colnames(Temp.row.affinity) <- c('n.clusters','sil.coef','algorithm','N.features','randomization')
  
  CombinedRandomFeatures.row <- rbind.data.frame(Temp.row.affinity,Temp.row.fuzzy)
  
  CombinedRandomFeatures.df.balanced <-  rbind.data.frame(CombinedRandomFeatures.df.balanced,CombinedRandomFeatures.row)
  #write.csv(CombinedRandomFeatures.df.balanced,'data_V1/CombinedRandomFeaturesBalanced.df.csv')
}

# Check table output
head(CombinedRandomFeatures.df.balanced)

# How many clusters with the randomization?
table(CombinedRandomFeatures.df.balanced$algorithm,
      CombinedRandomFeatures.df.balanced$n.clusters)

