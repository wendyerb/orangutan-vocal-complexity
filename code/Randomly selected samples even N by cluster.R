# Load packages
library(ggpubr)    # For creating plots
library(multcomp)  # For multiple comparisons
library(apcluster) # For affinity propagation clustering
library(tidyverse) # For data manipulation and visualization
library(e1071)     # For support vector machine (SVM) classification
library(cluster)   # For cluster analysis
library(clValid)   # For cluster validation

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
    write.csv(CombinedRandomFeatures.df.balanced,'data_V1/CombinedRandomFeaturesBalanced.df.csv')
  }

# Check table output
head(CombinedRandomFeatures.df.balanced)

# How many clusters with the randomization?
table(CombinedRandomFeatures.df.balanced$algorithm,
      CombinedRandomFeatures.df.balanced$n.clusters)
