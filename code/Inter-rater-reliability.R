### Part 1: Inter-rater reliability of audio-visually classified pulses ###

library(irr)

## Read in csv files
## original 6 classes
IRR <- read.csv("irr_6classes.csv")
## revised 3 classes
IRR3 <- read.csv("irr_3classes.csv")


# Calculate pairwise Kappa for each observer
onetwo <- kappa2(IRR[,c("Wendy", "Whitney")], weight = "unweighted", sort.levels = FALSE)
onetwo <- onetwo$value # 0.4783312

onethree<- kappa2(IRR[,c("Wendy", "Haley")], weight = "unweighted", sort.levels = FALSE)
onethree <- onethree$value # 0.6014439

twothree<- kappa2(IRR[,c("Whitney", "Haley")], weight = "unweighted", sort.levels = FALSE)
twothree <- twothree$value # 0.5989738

onetwothree <- c(onetwo, onethree, twothree)

# Then arithmetic mean()
meankappa <- mean(onetwothree)
meankappa # 0.599

### Repeat for IRR3 
