#### Exploring the Yellow Island data! ####

## Read in the data
counts <- read.csv("QuadratCounts.csv")
yellowPA <- read.csv("yellowPA.csv")

# First rule: always look at your data!
View(counts)
View(yellowPA)

#### Quadrat Counts Data: Cut in half!
# WhereSub will tell me which column the Substrate
# data starts in
WhereSub <- which(counts[1,]=="Substrate")
# I also want to know which column the Species Counts
# data starts in
WhereSpecies <- which(counts[1,]=="Species")

# I can use WhereSub and WhereSpecies to grab *just*
# the Substrate data
Substrate <- counts[,WhereSub:(WhereSpecies[1]-1)]

# And I can use WhereSpecies again to grab *just*
# the Species Counts data
Species <- counts[,WhereSpecies[1]:ncol(counts)]

# Let's see what we made;
View(Substrate)
View(Species)

# QUESTION: Does the percent coverage for each quadrat sum to 100% for Substrate?
# I want a vector of the site numbers (to use for plotting)
sites <- as.numeric(counts[-c(1,2),which(counts[2,]=="Site")])
sites

# This code creates a barplot and legend to answer my question!
barplot(height=t(as.matrix(Substrate[3:16,])),
        names.arg=sites,
        main="Substrate",
        ylab="Percent Coverage",
        xlab="Sites (Quadrat 1 and Quadrat 2)",
        ylim=c(0,150),
        col=c("gray10", "gray50", "gray90"))

legend(x="topright",
       legend=as.character(Substrate[2,]),
       fill=c("gray10", "gray50", "gray90"))

# Can we make a stacked barplot() with all species?

barplot(height=t(as.matrix(Species[3:16,])),
        names.arg=sites,
        main="Species Counts",
        ylab="Counts",
        xlab="Sites",
        col=1:24
        )

legend(x="topright",
       legend=as.character(SpeciesNames),
       fill=1:24)

# QUESTION: How many limpets were seen across sites?

limpet <- as.numeric(counts[-c(1,2),which(counts[2,]=="Limpet")])

barplot(height=t(limpet),
        names.arg=sites,
        main="Limpets",
        xlab="Sites (Quadrat 1 and Quadrat 2)",
        ylab="Counts")

# QUESTION: How many of EACH species were seen across sites?
SpeciesNames <- as.character(Species[2,])

# par(mfrow=c(6,4))

for(s in 1:ncol(Species)) {
  
  SppCount <- as.numeric(counts[-c(1,2), which(counts[2,]==SpeciesNames[s])])
  barplot(height=t(SppCount),
          names.arg=sites,
          main=SpeciesNames[s],
          xlab="Sites (Quadrat 1 and Quadrat 2)",
          ylab="Counts")
  
}

#### Yellow Island Presence-absence Data
# For this data, I'm going to use some special libraries
install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Let's remind ourselves of the data
View(yellowPA)

# QUESTION: Where are species present and what is their abundance score?

# To prepare to manipulate this data for plotting,
# I'm going to remove some of the metadeta comments
# And the last four columns of the original data frame
PAdata <- data.matrix(yellowPA[1:149,c(1,3:18)])

# Now I'm going to rearrange the variables in the PAdata set
# We'll slowly step through these
part1 <- melt(PAdata[,c(2,4,6,8,10,12,14,16)])
part2 <- melt(PAdata[,c(3,5,7,9,11,13,15,17)])
PAdata2 <- cbind(yellowPA[1:149,1],part1[,-1],part2[,-1])

# This gives better column names to match the moves I made
colnames(PAdata2) <- c("Scientific_Name", "Site", "PA", "Score_Site", "Score")

# This is my very first time trying this type of plot! 
# This is using the ggplot2 philosophy for coding plots

ggplot(PAdata2, aes(Site, Scientific_Name, fill=Score, alpha=PA)) + 
  geom_tile(colour = "gray50") +
  scale_alpha_identity(guide = "none") +
  coord_equal(expand = 0) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename="PAplot.png", plot=last_plot(), height=11.5, units="in", width=8.5)

# This is very rough draft plot, but amazing for visualizing your data after
# returning from the field.

