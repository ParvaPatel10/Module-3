# 1. Print your name at the top of the script and load these libraries: FSA, FSAdata, magrittr, dplyr, tidyr plyr and tidyverse
print(" Myself Parva Patel and NEUID is : 002195186")

#install.packages("plyr")
library(plyr)

#install.packages("FSA")
library(FSA)

#install.packages("FSAdata")
library(FSAdata)

#install.packages("magrittr")
library(magrittr)

#install.packages("dplyr")
library(dplyr)

#install.packages("tidyr")
library(tidyr)

#install.packages("tidyverse")
library(tidyverse)


# 2. Import the inchBio.csv and name the table <bio>

bio <- read.csv("inchBio.csv")
View(bio)

# 3. Display the head, tail and structure of <bio>

headtail(bio)
str(bio)

# 4. Create an object, <counts>, that counts and lists all the species records

counts<- table(bio$species)
counts

# 5. Display just the 8 levels (names) of the species

unique(bio$species)

#6. Create a <tmp> object that displays the different species and the number of record of each species in the dataset. 

tmp <- table(bio$species)
tmp

#7. Create a subset, <tmp2>, of just the species variable and display the first five records

tmp2 <- subset(bio,select = species)
tmp2
head(tmp2, 5)

#8.Create a table, <w>, of the species variable. Display the class of w

w <- table(bio$species)
w
class(w)

#9.Convert <w> to a data frame named <t> and display the results

t <- as.data.frame(w)
t

#10. Extract and display the frequency values from the <t> data frame

t$Freq

#11.Create a table named <cSpec> from the bio species attribute (variable) and confirm that you created a table which displays the number of species in the dataset <bio>

cSpec <- table(bio$species)*100
cSpec
class(cSpec)

#12.Create a table named <cSpecPct> that displays the species and percentage of records for each species. Confirm you created a table class.

cSpecPct <-prop.table(cSpec)*100
cSpecPct
class(cSpecPct)

#13.Convert the table, <cSpecPct>, to a data frame named <u> and confirm that <u> is a data frame

u <- as.data.frame(cSpecPct)
u
class(u)

#14.Create a barplot of <cSpec> with the following: titled Fish Count with the following specifications:

# Title: Fish Count
# Y axis is labeled COUNTS
# Color the bars Light Green
# Rotate Y axis to be horizontal
# Set the X axis font magnification to 60% of nominal

barplot(cSpec, main="Fish Count", ylab ="COUNTS", col = "lightgreen", cex.names = 0.60, las=2)

#15.Create a barplot of <cSpecPct>, with the following specifications:

# Y axis limits of 0 to 4
# Y axis label color of Light Blue
# Title of Fish Relative Frequency

barplot(cSpecPct, ylim = c(0,40), main="Fish Relative Frequency", ylab="COUNTS",col.lab="lightblue")

#16.Rearrange the <u> cSpec Pct data frame in descending order of relative frequency. Save the rearranged data frame as the object <d>

d <- u[order(-u$Freq),]
d
class(d)

#17.Rename the <d> columns Var 1 to Species, and Freq to RelFreq

d
colnames(d) <- c("Species","RelFreq")
d

#18.Add new variables to <d> and call them cumfreq, counts, and cumcounts

counts
t$Freq
tdesc <- t[order(-t$Freq),]
tdesc$Freq

d <- d %>% mutate(cumfreq=cumsum(d$RelFreq), counts=tdesc$Freq, cumcounts=cumsum(tdesc$Freq))
d

#19.Create a parameter variable <def_par> to store parameter variables

def_par <- par(no.readonly = TRUE)

#20.Create a barplot, <pc>, with the following specifications:

# d$counts of width 1, spacing of .15
# no boarder
# Axes: F
# Yaxis limit 0,3.05*max
# d$counts na.rm is true
# y label is Cummulative Counts
# scale x axis to 70%
# names.arg: d$Species
# Title of the barplot is Species Pareto
# las: 2)

pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, 
              ylim = c(0,3.05*228), ylab = "Cummulative Counts", 
              names.arg = d$Species, las=2, cex.names = 0.70, 
              main = "Species Pareto",d$counts,na.rm = TRUE)

#21.Add a cumulative counts line to the <pc> plot with the following:
# Spec line type is b
# Scale plotting text at 70%
# Data values are solid circles with color cyan4

pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto")
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")

#22.Place a grey box around the pareto plot.

pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto")
lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col="cyan4")
box(col = "grey62")


#23.Add a left side axis with the following specifications
# Horizontal values at tick marks at cumcounts on side 2
# Tickmark color of grey62
# Color of axis is grey62
# Axis scaled to 80% of normal

pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto\n Parva Patel")
lines(pc, d$cumcounts, type = "b", cex = 0.8, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)


#24.Add axis details on right side of box with the specifications:
# Spec: Side 4
# Tickmarks at cumcounts with labels from 0 to cumfreq with %,
# Axis color of cyan5 and label color of cyan4
# Axis font scaled to 80% of nominal

#25.Display the finished Species Pareto Plot (without the star watermarks). Have your last name on the plot

pc <- barplot(d$counts, width = 1, space = 0.15,border = NA, axes = F, ylim = c(0,3.05*228), ylab = "Cummulative Counts", names.arg = d$Species, las=2, cex.names = 0.70, main = "Species Pareto\n Parva Patel")
lines(pc, d$cumcounts, type = "b", cex = 0.8, pch = 19, col="cyan4")
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumcounts), labels = paste(c(0, round(d$cumfreq)) ,"%",sep=""), 
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)
 
