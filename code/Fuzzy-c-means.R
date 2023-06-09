### Part 4: Fuzzy c-means ###
library(cluster)
library(clValid)

#### Set working directory
#setwd('/Users/Wendy/github/orangutan-vocal-complexity/data')

####Read in features 
all.features <- read.csv('data/46-features.csv')

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


# Export chosen element of output to CSV
write.csv(k2u11$clustering, file = "K2U11_cluster.csv")


## Read in file with typicality coefficients
Typicality <- read.csv('data/fanny_typicality.csv')
str(Typicality)

## Separate data frames for typical and atypical calls
Typical <- Typicality %>% 
  filter(Cluster_Typical == "1" | Cluster_Typical == "2")
Atypical <- Typicality %>% 
  filter(Cluster_Atypical == "1" | Cluster_Atypical == "2") 

## Pulse type as factor
Typical$Pulse.Type <- factor(Typical$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))
Atypical$Pulse.Type <- factor(Atypical$Pulse.Type, levels = c("HU", "VO", "HR","LR", "IN", "SI"))

## Typicality coefficient plots
Typicality %>% 
  ggplot(aes(x=Typicality.d., fill = Pulse.Type)) + 
  theme_classic() +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Typicality coefficient", y = "Count", fill = "Pulse")


Typical %>% 
  ggplot(aes(x=Cluster_Typical, fill = Pulse.Type)) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  stat_bin(binwidth = 1) +
  labs(x = "Typicality coefficient", y = "count", fill = "Pulse")

Typ.Plot <- Typical %>% 
  ggplot(aes(x=Cluster_Typical, fill = Pulse.Type)) +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(title = element_text(size = 20)) +
  geom_bar() +
  labs(x = "Cluster number", y = "Count", fill = "Pulse")
Typ.Plot

Atyp.Plot <- Atypical %>% 
  ggplot(aes(x=Cluster_Atypical, fill = Pulse.Type)) +
  theme_classic() +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(title = element_text(size = 20)) +
  geom_bar() +
  labs(x = "Cluster number", y = "Count", fill = "Pulse", title = "Atypical calls")

cowplot::plot_grid(Typ.Plot, Atyp.Plot)

## Plot for stacked barplot of cluster number by pulse type
Typical %>% 
  ggplot(aes(x=Pulse.Type, fill=as.factor(Cluster_Typical))) +
  theme_classic() +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  geom_bar(position="fill") +
  guides(fill=guide_legend(title='Cluster')) +
  labs(x = "Pulse type", y = "Proportion")

Typical %>% 
  ggplot(aes(x=Typicality.d.)) + 
  geom_density() +
  theme_classic() +
  facet_wrap(vars(Pulse.Type))

### Use this for typicality coefficient by pulse type plot
Typicality %>% 
  ggplot(aes(x=Typicality.d., y=Pulse.Type, fill=Pulse.Type)) + 
  geom_boxplot(outlier.shape=1) +
  theme_classic() +
  scale_x_continuous(breaks = c(0,0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
  theme(axis.text = element_text(size=14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  labs(x = "Typicality coefficient", y = NULL, fill="Pulse") 

Typical %>% 
  ggplot(aes(x=Cluster_Typical)) + 
  geom_density() +
  theme_classic() +
  facet_wrap(vars(Pulse.Type))

Typical %>% 
  ggplot(aes(x=Pulse.Type, y=Cluster_Typical)) + 
  theme_classic() +
  labs(x = "Typicality coefficient", y = "cluster") 
