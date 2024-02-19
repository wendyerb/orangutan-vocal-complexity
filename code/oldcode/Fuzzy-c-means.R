### Part 4: Fuzzy c-means ###
library(cluster)
library(clValid)

#### Set working directory
#setwd('/Users/Wendy/github/orangutan-vocal-complexity/data')

####Read in features 
all.features <- read.csv('46-features.csv')

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
write.csv(k2u11$clustering, file = "K2U11_cluster.csv")


## Read in file with typicality coefficients
Typicality <- read.csv('fanny_typicality.csv')
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

cbPalette <- c("#762a83", "#af8dc3", "#e7d4e8", "#d9f0d3", "#7fbf7b", "#1b7837")

cbPallete3 <- c("#c51b7d","#e9a3c9","#fde0ef","#e6f5d0","#a1d76a","#4d9221")
cbPalette2 <- c("#d73027", "#fc8d59", "#fee090", "#e0f3f8", "#91bfdb", "#4575b4")
cbPalette1 <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854", "#ffd92f")


Typ.Plot <- Typical %>% 
  ggplot(aes(x=Cluster_Typical, fill = Pulse.Type)) +
  theme_classic() + scale_fill_manual(values = cbPalette) +
  scale_x_continuous(breaks = c(1, 2)) +
  theme(axis.text = element_text(size = 14)) +
  theme(axis.title = element_text(size = 20)) +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 20)) +
  theme(title = element_text(size = 20)) +
  geom_bar() +
  labs(x = "Cluster number", y = "Count", fill = "Pulse", title = "Typical calls")
Typ.Plot

Atyp.Plot <- Atypical %>% 
  ggplot(aes(x=Cluster_Atypical, fill = Pulse.Type)) + scale_fill_manual(values = cbPalette) +
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
cowplot::plot_grid(Fig6a,Fig6b,
                   labels=c('a)', 'b)'),label_x = 0.8)

Typical %>% 
  ggplot(aes(x=Typicality.d.)) + 
  geom_density() +
  theme_classic() +
  facet_wrap(vars(Pulse.Type))

### Use this for typicality coefficient by pulse type plot
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

# Arrange figures 5a-5b in a grid
cowplot::plot_grid(Fig5a,Fig5b,
                   labels=c('a)', 'b)'),label_x = 0.8)

Typical %>% 
  ggplot(aes(x=Cluster_Typical)) + 
  geom_density() +
  theme_classic() +
  facet_wrap(vars(Pulse.Type))

Typical %>% 
  ggplot(aes(x=Pulse.Type, y=Cluster_Typical)) + 
  theme_classic() +
  labs(x = "Typicality coefficient", y = "Cluster") 

